#' @title Compute BMI metrics for CSCI
#' 
#' Computes all CSCI related BMI metrics. This includes, clinger percent taxa, coleoptera,
#' percent taxa, taxnomic richness, EPT percent taxa, number of shredder taxa, and percent
#' of individuals that are intolerant.
#' 
#' @param x An object of class BMIagg or BMIprc
#' @export BMICSCI
#' @import data.table
#' @import vegan
#' @import plyr

BMICSCI <- function(x, effort=2){
  stopifnot("BMIagg" %in% class(x))
  x <- data.table(x[[effort]])
  x$Habit <- as.character(x$Habit)
  x <- rename(x, c("distinct_SAFIT1" = "distinct_SAFIT2", "SAFIT2" = "iggSAFIT2", "SAFIT1" = "SAFIT2"))
  result <- x[, list(
    Taxonomic_Richness = nrow(.SD[distinct_SAFIT2=="Distinct"]),
    Intolerant_Percent = sum(BAResult[which(ToleranceValue <= 2)])/sum(BAResult[!is.na(ToleranceValue)]),
    Shredder_Taxa = nrow(.SD[distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SH")]),
    Clinger_PercentTaxa = nrow(.SD[distinct_SAFIT2=="Distinct" & (Habit == "CN")])/nrow(.SD[distinct_SAFIT2=="Distinct" & !is.na(Habit)]),
    Coleoptera_PercentTaxa = nrow(.SD[distinct_SAFIT2=="Distinct" & (Order == "Coleoptera")])/nrow(.SD[distinct_SAFIT2=="Distinct"]), 
    EPT_PercentTaxa = nrow(.SD[distinct_SAFIT2=="Distinct" & (Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera"))])/nrow(.SD[distinct_SAFIT2=="Distinct"])
  ),
  by=SampleID]
  data.frame(result)
}

