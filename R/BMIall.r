#' @title Compute all BMI metrics
#' 
#' Computes all available BMI metrics.
#' 
#' @param x An object of class BMIagg or BMIprc
#' @export BMIall
#' @import plyr
#' @import vegan
#' @include loadMetaData.r
#' @include Acari.r
#' @include aggregate.r
#' @include Amphipoda.r
#' @include BMI.r
#' @include BMIall.r
#' @include BMIMetrics.r
#' @include bugdata.r
#' @include Burrower.r
#' @include CGCG.r
#' @include Chironomidae.r
#' @include Climber.r
#' @include Clinger.r
#' @include Coleoptera.r
#' @include Crustacea.r
#' @include Diptera.r
#' @include Ephemeroptera.R
#' @include EPT.r
#' @include Invasive.r
#' @include loadMetaData.r
#' @include Noninsect.r
#' @include Plecoptera.r
#' @include Predator.r
#' @include sample.r
#' @include Scraper.r
#' @include Shredder.r
#' @include Swimmer.r
#' @include Tolerance.r
#' @include Total_Taxa.r
#' @include Trichoptera.r

BMIall <- function(x){
  metadata <- loadMetaData()
  cbind.fill<-function(...){
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))) 
  }
  result <- cbind.fill(
    Acari_Percent(x),
    Acari_PercentTaxa(x),
    Acari_Taxa(x),
    Amphipoda_Percent(x),
    Burrower_Percent(x),
    Burrower_PercentTaxa(x),
    Burrower_Taxa(x),
    CFCG_Percent(x),
    CFCG_PercentTaxa(x),
    CFCG_Taxa(x),
    Chironomidae_PercentTaxa(x),
    Chironomidae_Taxa(x),
    Chironominae_Percent(x),
    Chironominae_PercentOfMidges(x),
    Climber_Percent(x),
    Climber_PercentTaxa(x),
    Climber_Taxa(x),
    Clinger_Percent(x),
    Clinger_PercentTaxa(x),
    Clinger_Taxa(x),
    Coleoptera_Percent(x),
    Coleoptera_PercentTaxa(x, level="SAFIT1"),
    Coleoptera_PercentTaxa(x, level="SAFIT2"),
    Coleoptera_Taxa(x, level="SAFIT1"),
    Coleoptera_Taxa(x, level="SAFIT2"),
    Crustacea_Percent(x),
    Diptera_Percent(x),
    Diptera_PercentTaxa(x, level="SAFIT1"),
    Diptera_PercentTaxa(x, level="SAFIT2"),
    Diptera_Taxa(x, level="SAFIT1"),
    Diptera_Taxa(x, level="SAFIT2"),
    EPT_Percent(x),
    EPT_PercentTaxa(x, level="SAFIT1"),
    EPT_PercentTaxa(x, level="SAFIT2"),
    EPT_Taxa(x, level="SAFIT1"),
    EPT_Taxa(x, level="SAFIT2"),
    Ephemeroptera_Percent(x),
    Ephemeroptera_PercentTaxa(x, level="SAFIT1"),
    Ephemeroptera_PercentTaxa(x, level="SAFIT2"),
    Ephemeroptera_Taxa(x, level="SAFIT1"),
    Ephemeroptera_Taxa(x, level="SAFIT2"),
    Intolerant_Percent(x),
    Intolerant_PercentTaxa(x),
    Intolerant_Taxa(x),
    Invasive_Percent(x),
    Invasive_PercentTaxa(x),
    Invasive_Taxa(x),
    Noninsect_Percent(x),
    Noninsect_PercentTaxa(x, level="SAFIT1"),
    Noninsect_PercentTaxa(x, level="SAFIT2"),
    Noninsect_Taxa(x, level="SAFIT1"),
    Noninsect_Taxa(x, level="SAFIT2"),
    Orthocladiinae_PercentOfMidges(x),
    Ostracoda_Percent(x),
    Plecoptera_Percent(x),
    Plecoptera_PercentTaxa(x, level="SAFIT1"),
    Plecoptera_PercentTaxa(x, level="SAFIT2"),
    Plecoptera_Taxa(x, level="SAFIT1"),
    Plecoptera_Taxa(x, level="SAFIT2"),
    Predator_Percent(x),
    Predator_PercentTaxa(x),
    Predator_Taxa(x),
    Scraper_Percent(x),
    Scraper_PercentTaxa(x),
    Scraper_Taxa(x),
    Shredder_Percent(x),
    Shredder_PercentTaxa(x),
    Shredder_Taxa(x),
    Shannon_Diversity(x, level="SAFIT1"),
    Shannon_Diversity(x, level="SAFIT2"),
    Simpson_Diversity(x, level="SAFIT1"),
    Simpson_Diversity(x, level="SAFIT2"),
    Swimmer_Percent(x),
    Swimmer_PercentTaxa(x),
    Swimmer_Taxa(x),
    Tanypodinae_Percent(x),
    Tanypodinae_PercentOfMidges(x),
    ToleranceValue(x),
    Tolerant_Percent(x),
    Tolerant_PercentTaxa(x),
    Tolerant_Taxa(x),
    Total_Taxa(x),
    Trichoptera_Percent(x),
    Trichoptera_PercentTaxa(x, level="SAFIT1"),
    Trichoptera_PercentTaxa(x, level="SAFIT2"),
    Trichoptera_Taxa(x, level="SAFIT1"),
    Trichoptera_Taxa(x, level="SAFIT2")
    )
  result <- data.frame(result)
  result <- result[, -(seq.int(3, 173, by=2))] 
   colnames(result) <- c("SampleID",
                       "Acari_Percent",
                       "Acari_PercentTaxa",
                       "Acari_Taxa",
                       "Amphipoda_Percent",
                       "Burrower_Percent",
                       "Burrower_PercentTaxa",
                       "Burrower_Taxa",
                       "CFCG_Percent",
                       "CFCG_PercentTaxa",
                       "CFCG_Taxa",
                       "Chironomidae_PercentTaxa",
                       "Chironomidae_Taxa",
                       "Chironominae_Percent",
                       "Chironominae_PercentOfMidges",
                       "Climber_Percent",
                       "Climber_PercentTaxa",
                       "Climber_Taxa",
                       "Clinger_Percent",
                       "Clinger_PercentTaxa",
                       "Clinger_Taxa",
                       "Coleoptera_Percent",
                       "Coleoptera_PercentTaxa_SAFIT1",
                       "Coleoptera_PercentTaxa_SAFIT2",
                       "Coleoptera_Taxa_SAFIT1",
                       "Coleoptera_Taxa_SAFIT2",
                       "Crustacea_Percent",
                       "Diptera_Percent",
                       "Diptera_PercentTaxa_SAFIT1",
                       "Diptera_PercentTaxa_SAFIT2",
                       "Diptera_Taxa_SAFIT1",
                       "Diptera_Taxa_SAFIT2",
                       "EPT_Percent",
                       "EPT_PercentTaxa_SAFIT1",
                       "EPT_PercentTaxa_SAFIT2",
                       "EPT_Taxa_SAFIT1",
                       "EPT_Taxa_SAFIT2",
                       "Ephemeroptera_Percent",
                       "Ephemeroptera_PercentTaxa_SAFIT1",
                       "Ephemeroptera_PercentTaxa_SAFIT2",
                       "Ephemeroptera_Taxa_SAFIT1",
                       "Ephemeroptera_Taxa_SAFIT2",
                       "Intolerant_Percent",
                       "Intolerant_PercentTaxa",
                       "Intolerant_Taxa",
                       "Invasive_Percent",
                       "Invasive_PercentTaxa",
                       "Invasive_Taxa",
                       "Noninsect_Percent",
                       "Noninsect_PercentTaxa_SAFIT1",
                       "Noninsect_PercentTaxa_SAFIT2",
                       "Noninsect_Taxa_SAFIT1",
                       "Noninsect_Taxa_SAFIT2",
                       "Orthocladiinae_PercentOfMidges",
                       "Ostracoda_Percent",
                       "Plecoptera_Percent",
                       "Plecoptera_PercentTaxa_SAFIT1",
                       "Plecoptera_PercentTaxa_SAFIT2",
                       "Plecoptera_Taxa_SAFIT1",
                       "Plecoptera_Taxa_SAFIT2",
                       "Predator_Percent",
                       "Predator_PercentTaxa",
                       "Predator_Taxa",
                       "Scraper_Percent",
                       "Scraper_PercentTaxa",
                       "Scraper_Taxa",
                       "Shredder_Percent",
                       "Shredder_PercentTaxa",
                       "Shredder_Taxa",
                       "Shannon_Diversity_SAFIT1",
                       "Shannon_Diversity_SAFIT2",
                       "Simpson_Diversity_SAFIT1",
                       "Simpson_Diversity_SAFIT2",
                       "Swimmer_Percent",
                       "Swimmer_PercentTaxa",
                       "Swimmer_Taxa",
                       "Tanypodinae_Percent",
                       "Tanypodinae_PercentOfMidges",
                       "ToleranceValue",
                       "Tolerant_Percent",
                       "Tolerant_PercentTaxa",
                       "Tolerant_Taxa",
                       "Total_Taxa",
                       "Trichoptera_Percent",
                       "Trichoptera_PercentTaxa_SAFIT1",
                       "Trichoptera_PercentTaxa_SAFIT2",
                       "Trichoptera_Taxa_SAFIT1",
                       "Trichoptera_Taxa_SAFIT2")
  as.data.frame(result)
}