#' @title Compute BMI metrics for CSCI
#' 
#' Computes all CSCI related BMI metrics. This includes, clinger percent taxa, coleoptera,
#' percent taxa, taxnomic richness, EPT percent taxa, number of shredder taxa, and percent
#' of individuals that are intolerant.
#' 
#' @param x An object of class BMIagg or BMIprc
#' @export BMICSCI
#' @import dplyr
#' @import vegan
#' @import plyr


BMICSCI <- function(x, effort=2){
  stopifnot("BMIagg" %in% class(x))
  x <- x[[effort]]
  x$Habit <- as.character(x$Habit)
  x <- plyr::rename(x, c("distinct_SAFIT1" = "distinct_SAFIT2", "SAFIT2" = "iggSAFIT2", "SAFIT1" = "SAFIT2"),
              warn_missing=FALSE)
  dplyr::summarise(dplyr::group_by(x, SampleID), 
    Taxonomic_Richness = sum1(distinct_SAFIT2=="Distinct"),
    Intolerant_Percent = sum1(BAResult[which(ToleranceValue <= 2)])/sum1(BAResult[!is.na(ToleranceValue)]),
    Shredder_Taxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SH")),
    Clinger_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & Habit == "CN")/sum1(distinct_SAFIT2=="Distinct" & !is.na(Habit)),
    Coleoptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Coleoptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    EPT_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")))/sum1(distinct_SAFIT2=="Distinct")
  )
}

