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
  result <- data.frame(
    Acari_Percent(x),
    Acari_PercentTaxa(x)[, 2],
    Acari_Taxa(x)[, 2],
    Amphipoda_Percent(x)[, 2],
    Burrower_Percent(x)[, 2],
    Burrower_PercentTaxa(x)[, 2],
    Burrower_Taxa(x)[, 2],
    CFCG_Percent(x)[, 2],
    CFCG_PercentTaxa(x)[, 2],
    CFCG_Taxa(x)[, 2],
    Chironomidae_PercentTaxa(x)[, 2],
    Chironomidae_Taxa(x)[, 2],
    Chironominae_Percent(x)[, 2],
    Chironominae_PercentOfMidges(x)[, 2],
    Climber_Percent(x)[, 2],
    Climber_PercentTaxa(x)[, 2],
    Climber_Taxa(x)[, 2],
    Clinger_Percent(x)[, 2],
    Clinger_PercentTaxa(x)[, 2],
    Clinger_Taxa(x)[, 2],
    Coleoptera_Percent(x)[, 2],
    Coleoptera_PercentTaxa(x, level="SAFIT1")[, 2],
    Coleoptera_PercentTaxa(x, level="SAFIT2")[, 2],
    Coleoptera_Taxa(x, level="SAFIT1")[, 2],
    Coleoptera_Taxa(x, level="SAFIT2")[, 2],
    Crustacea_Percent(x)[, 2],
    Diptera_Percent(x)[, 2],
    Diptera_PercentTaxa(x, level="SAFIT1")[, 2],
    Diptera_PercentTaxa(x, level="SAFIT2")[, 2],
    Diptera_Taxa(x, level="SAFIT1")[, 2],
    Diptera_Taxa(x, level="SAFIT2")[, 2],
    EPT_Percent(x)[, 2],
    EPT_PercentTaxa(x, level="SAFIT1")[, 2],
    EPT_PercentTaxa(x, level="SAFIT2")[, 2],
    EPT_Taxa(x, level="SAFIT1")[, 2],
    EPT_Taxa(x, level="SAFIT2")[, 2],
    Ephemeroptera_Percent(x)[, 2],
    Ephemeroptera_PercentTaxa(x, level="SAFIT1")[, 2],
    Ephemeroptera_PercentTaxa(x, level="SAFIT2")[, 2],
    Ephemeroptera_Taxa(x, level="SAFIT1")[, 2],
    Ephemeroptera_Taxa(x, level="SAFIT2")[, 2],
    Intolerant_Percent(x)[, 2],
    Intolerant_PercentTaxa(x)[, 2],
    Intolerant_Taxa(x)[, 2],
    Invasive_Percent(x)[, 2],
    Invasive_PercentTaxa(x)[, 2],
    Invasive_Taxa(x)[, 2],
    Noninsect_Percent(x)[, 2],
    Noninsect_PercentTaxa(x, level="SAFIT1")[, 2],
    Noninsect_PercentTaxa(x, level="SAFIT2")[, 2],
    Noninsect_Taxa(x, level="SAFIT1")[, 2],
    Noninsect_Taxa(x, level="SAFIT2")[, 2],
    Orthocladiinae_PercentOfMidges(x)[, 2],
    Ostracoda_Percent(x)[, 2],
    Plecoptera_Percent(x)[, 2],
    Plecoptera_PercentTaxa(x, level="SAFIT1")[, 2],
    Plecoptera_PercentTaxa(x, level="SAFIT2")[, 2],
    Plecoptera_Taxa(x, level="SAFIT1")[, 2],
    Plecoptera_Taxa(x, level="SAFIT2")[, 2],
    Predator_Percent(x)[, 2],
    Predator_PercentTaxa(x)[, 2],
    Predator_Taxa(x)[, 2],
    Scraper_Percent(x)[, 2],
    Scraper_PercentTaxa(x)[, 2],
    Scraper_Taxa(x)[, 2],
    Shredder_Percent(x)[, 2],
    Shredder_PercentTaxa(x)[, 2],
    Shredder_Taxa(x)[, 2],
    Shannon_Diversity(x, level="SAFIT1")[, 2],
    Shannon_Diversity(x, level="SAFIT2")[, 2],
    Simpson_Diversity(x, level="SAFIT1")[, 2],
    Simpson_Diversity(x, level="SAFIT2")[, 2],
    Swimmer_Percent(x)[, 2],
    Swimmer_PercentTaxa(x)[, 2],
    Swimmer_Taxa(x)[, 2],
    Tanypodinae_Percent(x)[, 2],
    Tanypodinae_PercentOfMidges(x)[, 2],
    ToleranceValue(x)[, 2],
    Tolerant_Percent(x)[, 2],
    Tolerant_PercentTaxa(x)[, 2],
    Tolerant_Taxa(x)[, 2],
    Total_Taxa(x)[, 2],
    Trichoptera_Percent(x)[, 2],
    Trichoptera_PercentTaxa(x, level="SAFIT1")[, 2],
    Trichoptera_PercentTaxa(x, level="SAFIT2")[, 2],
    Trichoptera_Taxa(x, level="SAFIT1")[, 2],
    Trichoptera_Taxa(x, level="SAFIT2")[, 2]
    )
  names(result) <- c("SampleID",
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
  result
}