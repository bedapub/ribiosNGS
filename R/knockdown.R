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
  kdAllTbl <- do.call(rbind, lapply(ribiosNGS::dgeTables(edgeRes), appendRanks))
  kdTbl <- kdAllTbl %>% filter(!!as.symbol(feature_label) == feature)
  res <- kdTbl %>%
    mutate(knockdown_efficiency=1-2^logFC) %>%
           select(Contrast, !!as.symbol(feature_label), logFC, FDR,
                  knockdown_efficiency,
                  rank_PValue, rank_logFC, rank_absLogFC, total_features)
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
  gs <- kdTable[,feature_label][1]
  res <- kdTable %>% gt::gt(...) %>%
    tab_header(title = paste(gs, "knockdown summary")) %>%
    fmt_number(columns=c(rank_PValue, rank_logFC, rank_absLogFC), decimals=0) %>%
    fmt_number(columns=c(logFC,  total_features), n_sigfig=3) %>%
    fmt_percent(columns=c(knockdown_efficiency)) %>%
    fmt_scientific(columns=c(FDR)) %>%
    tab_options(table.width = pct(100))
}

#' Plot Gene knockdown with knockdown efficiency
#' @param goiExpr A data.frame containing expression of gene of interest, which
#'   must contain following columns: \code{log2_CPM}, \code{GROUP} (a column or
#'   character strings, among which a case-sensitive \code{Control} is available)
#' @note The function is just a prototype and will be likely updated to make it more
#' standardized and flexible.
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes_string geom_boxplot scale_fill_manual theme_bw
#'   theme geom_point position_jitterdodge ylab xlab scale_y_log10 geom_hline
#'   scale_y_continuous sec_axis
#' @importFrom ggpubr stat_compare_means
#' @importFrom scales percent
#' @export
plotGeneKnockdown <- function(goiExpr) {
  log2_CPM <- NULL
  goiExpr <- goiExpr %>% mutate(CPM=2^log2_CPM)
  geneBaseTpm <- with(goiExpr, median(CPM[GROUP=="Control"]))
  ggplot(goiExpr, aes_string(x = "GROUP", y = "CPM",
                             color="GROUP", fill = "GROUP", group = "GROUP")) +
    geom_boxplot(outlier.shape = NA) + 
    theme_bw(base_size=13) + theme(legend.position = "none") +
    geom_point(position = position_jitterdodge(), pch=4) +
    ylab("Expression [cpm]") + xlab("Treatment") +
    scale_y_log10(n.breaks=10) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5),
          legend.position = "none") +
    geom_hline(yintercept=geneBaseTpm, linetype=2) +
    scale_y_continuous(trans="log10", n.breaks=10,
                       sec.axis = sec_axis(~1-./geneBaseTpm,
                                           breaks=c(0, 0.2,0.5,0.6,.7,.8,.9),
                                           labels=scales::percent,
                                           name="KD efficiency")) +
    ggpubr::stat_compare_means(aes(label = "..p.signif.."), method="wilcox.test",
                               size=7, col="red",
                               ref.group = "Control", hide.ns = TRUE, label.y.npc=.9)
}
