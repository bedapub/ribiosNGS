mroast2tbl <- function(mres) {
  res <- data.frame(## adjusted P values
                    Down.adj.p.value=mres$Adj.P.Value[,"Down"],
                    Up.adj.p.value=mres$Adj.P.Value[,"Up"],
                    Mixed.adj.p.value=mres$Adj.P.Value[,"Mixed"],
                    ## Active proportion
                    Down.active.proportion=mres$Active.Proportion[,"Down"],
                    Up.active.proportion=mres$Active.Proportion[,"Up"],
                    Mixed.active.proportion=mres$Active.Proportion[,"Mixed"],
                    ## rotation-test p-values
                    Down.p.value=mres$P.Value[,"Down"],
                    Up.p.value=mres$P.Value[,"Up"],
                    Mixed.p.value=mres$P.Value[,"Mixed"],
                    ## row names
                    row.names=rownames(mres$P.Value))
  return(res)
}
