globalVariables(c("rank_logFC", "rank_PValue", "rank_absLogFC", "total_features"))

#' Append ranks to dgeTbl
#' @param dgeTbl A dgeTbl, a data.frame that at least contain following columns
#' \code{logFC} and \code{PValue}
#' @return The dgeTble with four columns appended: \code{rank_logFC},
#' \code{rank_PValue}, \code{rank_absLogFC}, and \code{total_features}, and
#' sorted by increasing P-values
#' @export
#' @importFrom dplyr mutate arrange desc
#' @importFrom magrittr '%>%'
#' @examples
#' myTbl <- data.frame(GeneSymbol=LETTERS[1:5], 
#'   logFC=rnorm(5), PValue=runif(5, 0, 1))
#' appendRanks(myTbl)
appendRanks <- function(dgeTbl) {
  res <-  mutate(dgeTbl, rank_PValue=1:nrow(dgeTbl)) %>%
    arrange(logFC) %>%
    mutate(rank_logFC=1:nrow(dgeTbl)) %>%
    arrange(desc(abs(logFC))) %>%
    mutate(rank_absLogFC=1:nrow(dgeTbl)) %>%
    mutate(total_features=nrow(dgeTbl)) %>%
    arrange(PValue)
  return(res)
}

#' Retrieve a knockdown table from edgeRes
#' @param edgeRes An \code{EdgeResult} object
#' @param feature The feature to be retrieved
#' @param feature_label The column which contains feature label, gene symbol by
#' default
#' @return A compact table containing essential information about knockdown
#' @importFrom rlang '!!'
#' @export
kdTable <- function(edgeRes, feature, feature_label="GeneSymbol") {
  knockdown_efficiency <- NULL
  kdAllTbl <- do.call(rbind, lapply(ribiosNGS::dgeTables(edgeRes), appendRanks))
  kdTbl <- kdAllTbl %>% filter(!!as.symbol(feature_label) == feature)
  res <- kdTbl %>%
    mutate(knockdown_efficiency=1-2^logFC) %>%
           select(!!as.symbol(feature_label),
                  Contrast, logFC, FDR,
                  knockdown_efficiency,
                  rank_logFC, rank_absLogFC, rank_PValue, total_features)
  return(res)
}

#' Print a kdTable nicely with gt
#' @param kdTable a knockdown table (kdTable) returned by \code{kdTable}
#' @param feature_label The column which contains feature label, gene symbol by
#' @param ... Passed to \code{\link[gt]{gt}}
#' default
#' @return A \code{gt} table object
#' @export
#' @importFrom gt gt tab_header fmt_number fmt_scientific fmt_percent tab_options pct
#' @importFrom dplyr vars
gtKdTable <- function(kdTable, feature_label="GeneSymbol", ...) {
  rank_Pvalue <- rank_logFC <- rank_absLogFC <- total_features <- NULL
  knockdown_efficiency <- FDR <- NULL
  gs <- kdTable[,feature_label][1]
  res <- kdTable %>% gt::gt(...) %>%
    tab_header(title = paste(gs, "knockdown summary")) %>%
    fmt_number(columns=c(rank_PValue, rank_logFC, rank_absLogFC), decimals=0) %>%
    fmt_number(columns=c(logFC,  total_features), n_sigfig=3) %>%
    fmt_percent(columns=c(knockdown_efficiency), decimals=1) %>%
    fmt_scientific(columns=c(FDR)) %>%
    tab_options(table.width = pct(100))
}

#' Plot gene expression with knockdown efficiency
#' @param goiExpr A data.frame containing expression of gene of interest in linear scale, which
#'   must contain columns given below as \code{exprsVar} and \code{groupVar}.
#' @param exprsVar Character, the variable name of expression. The unit must be in linear scale, not in logarithmic scale, otherwise the knockdown efficiency calculation will be wrong.
#' @param groupVar Character, the variable name of grouping. The column must be a factor or a character.
#' @param controlGroup \code{NULL} or character. If \code{groupVar} is a factor and \code{controlGroup} is \code{NULL}, then the first level is assumed to be the control. Otherwise, \code{controlGroup} must be in the group variable.
#' @param trans Character, transformation of the y-axis, commonly used values include \code{identity} (do not transform), \code{log10}, and \code{log2}.
#' @param exprsUnit Character, unit name of expression (TPM, CPM, RPKM, etc.)
#' @param test Character, statistical test, \code{wilcox.test} and \code{t.test} are supported.
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes_string geom_boxplot scale_fill_manual theme_bw
#'   theme geom_point position_jitterdodge ylab xlab scale_y_log10 geom_hline
#'   scale_y_continuous sec_axis
#' @importFrom ggpubr stat_compare_means
#' @importFrom scales percent
#' @importFrom ribiosUtils haltifnot
#' @importFrom rlang .data
#' @export
#' @examples 
#' 
#' myData <- data.frame(group=gl(3,4),
#'  exprs=as.vector(sapply(c(100, 10, 1), function(x) rnorm(4, x))))
#' plotKnockdown(myData)
#' 
#' myData2 <-  data.frame(group=rep(c("Vehicle", "Dose1", "Dose2"), each=4),
#'  exprs=as.vector(sapply(c(100, 10, 1), function(x) rnorm(4, x))))
#' plotKnockdown(myData2, controlGroup="Vehicle")
plotKnockdown <- function(goiExpr,
                              exprsVar="exprs",
                              groupVar="group",
                              controlGroup=NULL,
                              trans = "identity",
                              exprsUnit="Arbitrary Unit",
                              test=c("wilcox.test", "t.test")) {
  test <- match.arg(test)
  haltifnot(exprsVar %in% colnames(goiExpr),
            msg=sprintf("Column '%s' not found in inputData", exprsVar))
  haltifnot(groupVar %in% colnames(goiExpr),
            msg=sprintf("Column '%s' not found in inputData", groupVar))
  group <- goiExpr[, groupVar]
  if(is.factor(group) && is.null(controlGroup)) {
    controlGroup <- levels(group)[1]
  } else {
    if(!is.factor(group)) {
      haltifnot(!is.null(controlGroup),
                msg=sprintf("'%s' is a character vector, therefore controlGroup cannot be NULL.",
                            groupVar))
    }
    haltifnot(controlGroup %in% group,
              msg=sprintf(sprintf("'%s' is a factor, but the control group ('%s') is not found.",
                                  groupVar, controlGroup)))
    goiExpr[, groupVar] <- relevels(group, refs=controlGroup, 
                                    missingLevels = "pass",
                                    unrecognisedLevels = "error")
  }
  isControl <- group==controlGroup
  controlExpr <- goiExpr[isControl, exprsVar]
  medianControl <- median(controlExpr, na.rm=TRUE)
  res <- ggplot(goiExpr, aes_string(x = groupVar, y = exprsVar,
                                    color= groupVar, 
                                    group = groupVar)) +
    geom_boxplot(outlier.shape = NA) + 
    theme_bw(base_size=13) + theme(legend.position = "none") +
    geom_point(position = position_jitterdodge(), pch=4) +
    ylab(sprintf("Expression [%s]", exprsUnit)) + 
    xlab(groupVar) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5),
          legend.position = "none") +
    geom_hline(yintercept=medianControl, linetype=2) +
    scale_y_continuous(trans=trans, n.breaks=10,
                       sec.axis = sec_axis(~1-./medianControl,
                                           breaks=c(0, 0.2,0.5,0.6,.7,.8,.9),
                                           labels=scales::percent,
                                           name="KD efficiency")) +
    ggpubr::stat_compare_means(label = "p.signif", 
                               method=test,
                               size=7, col="red",
                               symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")),
                               ref.group = controlGroup, hide.ns = TRUE, label.y.npc=.9)
  return(res)
}
