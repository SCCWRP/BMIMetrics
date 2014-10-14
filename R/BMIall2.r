#' @title Compute all BMI metrics
#' 
#' Computes all available BMI metrics.
#' 
#' @param x An object of class BMIagg or BMIprc
#' @export BMIall
#' @import dplyr
#' @import vegan
#' @import plyr

sum1 <- function(x)sum(x, na.rm=TRUE)

BMIall <- function(x, effort=2){
  stopifnot("BMIagg" %in% class(x))
  x <- data.table(x[[effort]])
  x$Habit <- as.character(x$Habit)
  x <- plyr::rename(x, c("distinct_SAFIT1" = "distinct_SAFIT2", "SAFIT2" = "iggSAFIT2", "SAFIT1" = "SAFIT2"),
              warn_missing=FALSE)
  dplyr::summarise(group_by(x, SampleID), 
    ###Community Metrics###
    Invasive_Percent = sum1(BAResult[Invasive == 1])/sum1(BAResult),
    Invasive_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Invasive == 1))/sum1(distinct_SAFIT2=="Distinct"), 
    Invasive_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Invasive == 1)),
    Taxonomic_Richness = sum1(distinct_SAFIT2=="Distinct"),
    Shannon_Diversity = diversity(tapply(BAResult, as.character(SAFIT2), sum), index= "shannon"),
    Simpson_Diversity = diversity(tapply(BAResult, as.character(SAFIT2), sum), index= "simpson"),
    Dominant_Percent = sum1(tail(sort(tapply(BAResult, SAFIT2, sum)), 3))/sum1(BAResult),
    ###Tolerance Metrics###
    Intolerant_Percent = sum1(BAResult[which(ToleranceValue <= 2)])/sum1(BAResult[!is.na(ToleranceValue)]),
    Intolerant_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & ToleranceValue <= 2)/sum1(distinct_SAFIT2=="Distinct"),
    Intolerant_Taxa = sum1(distinct_SAFIT2=="Distinct" & ToleranceValue <= 2),
    Tolerant_Percent = sum1(BAResult[which(ToleranceValue >= 8)])/sum1(BAResult),
    Tolerant_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & ToleranceValue >= 8)/sum1(distinct_SAFIT2=="Distinct"),
    Tolerant_Taxa = sum1(distinct_SAFIT2=="Distinct" & ToleranceValue >= 8),
    Tolerance_Value = sum1(BAResult * ToleranceValue)/sum1(BAResult[!is.na(ToleranceValue)]),
    ###Feeding Group Metrics###
    CFCG_Percent = sum1(BAResult[FunctionalFeedingGroup == "CF" | FunctionalFeedingGroup == "CG"])/sum1(BAResult),
    CFCG_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "CF" | FunctionalFeedingGroup == "CG"))/sum1(distinct_SAFIT2=="Distinct"), 
    CFCG_Taxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "CF" | FunctionalFeedingGroup == "CG")),
    Predator_Percent = sum1(BAResult[FunctionalFeedingGroup == "P"])/sum1(BAResult),
    Predator_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "P"))/sum1(distinct_SAFIT2=="Distinct"), 
    Predator_Taxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "P")),
    Scraper_Percent = sum1(BAResult[FunctionalFeedingGroup == "SC"])/sum1(BAResult),
    Scraper_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SC"))/sum1(distinct_SAFIT2=="Distinct"), 
    Scraper_Taxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SC")),
    Shredder_Percent = sum1(BAResult[FunctionalFeedingGroup == "SH"])/sum1(BAResult),
    Shredder_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SH"))/sum1(distinct_SAFIT2=="Distinct"), 
    Shredder_Taxa = sum1(distinct_SAFIT2=="Distinct" & (FunctionalFeedingGroup == "SH")),
    ###Habit Group Metrics###
    Burrower_Percent = sum1(BAResult[!is.na(Habit) & Habit == "BU"])/sum1(BAResult[!is.na(Habit)]),
    Burrower_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "BU"))/sum1(distinct_SAFIT2=="Distinct" & !is.na(Habit)), 
    Burrower_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "BU")),
    Climber_Percent = sum1(BAResult[!is.na(Habit) & Habit == "CB"])/sum1(BAResult[!is.na(Habit)]),
    Climber_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "CB"))/sum1(distinct_SAFIT2=="Distinct" & !is.na(Habit)), 
    Climber_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "CB")),
    Clinger_Percent = sum1(BAResult[!is.na(Habit) & Habit == "CN"])/sum1(BAResult[!is.na(Habit)]),
    Clinger_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "CN"))/sum1(distinct_SAFIT2=="Distinct" & !is.na(Habit)), 
    Clinger_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "CN")),
    Swimmer_Percent = sum1(BAResult[!is.na(Habit) & Habit == "SW"])/sum1(BAResult[!is.na(Habit)]),
    Swimmer_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "SW"))/sum1(distinct_SAFIT2=="Distinct" & !is.na(Habit)), 
    Swimmer_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Habit == "SW")),
    ###Insect Order Metrics###
    Coleoptera_Percent = sum1(BAResult[Order == "Coleoptera"])/sum1(BAResult),
    Coleoptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Coleoptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    Coleoptera_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Coleoptera")),
    Diptera_Percent = sum1(BAResult[Order == "Diptera"])/sum1(BAResult),
    Diptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Diptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    Diptera_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Diptera")),
    Ephemeroptera_Percent = sum1(BAResult[Order == "Ephemeroptera"])/sum1(BAResult),
    Ephemeroptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Ephemeroptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    Ephemeroptera_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Ephemeroptera")),
    EPT_Percent = sum1(BAResult[Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")])/sum1(BAResult),
    EPT_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")))/sum1(distinct_SAFIT2=="Distinct"), 
    EPT_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera"))),
    Plecoptera_Percent = sum1(BAResult[Order == "Plecoptera"])/sum1(BAResult),
    Plecoptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Plecoptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    Plecoptera_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Plecoptera")),
    Trichoptera_Percent = sum1(BAResult[Order == "Trichoptera"])/sum1(BAResult),
    Trichoptera_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Trichoptera"))/sum1(distinct_SAFIT2=="Distinct"), 
    Trichoptera_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Trichoptera")),
    ###Chironomid Taxa Metrics###
    Chironomidae_Percent = sum1(BAResult[Family == "Chironomidae"])/sum1(BAResult),
    Chironomidae_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Family == "Chironomidae"))/sum1(distinct_SAFIT2=="Distinct"), 
    Chironomidae_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Family == "Chironomidae")),
    Chironominae_Percent = sum1(BAResult[Subfamily == "Chironominae"])/sum1(BAResult),
    Chironominae_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Chironominae"))/sum1(distinct_SAFIT2=="Distinct"), 
    Chironominae_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Chironominae")),
    Chironominae_PercentOfMidges = sum1(BAResult[Subfamily == "Chironominae"])/sum1(BAResult[Family == "Chironomidae"]),
    Tanypodinae_Percent = sum1(BAResult[Subfamily == "Tanypodinae"])/sum1(BAResult),
    Tanypodinae_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Tanypodinae"))/sum1(distinct_SAFIT2=="Distinct"), 
    Tanypodinae_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Tanypodinae")),
    Tanypodinae_PercentOfMidges = sum1(BAResult[Subfamily == "Tanypodinae"])/sum1(BAResult[Family == "Chironomidae"]),
    Orthocladiinae_Percent = sum1(BAResult[Subfamily == "Orthocladiinae"])/sum1(BAResult),
    Orthocladiinae_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Orthocladiinae"))/sum1(distinct_SAFIT2=="Distinct"), 
    Orthocladiinae_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subfamily == "Orthocladiinae")),
    Orthocladiinae_PercentOfMidges = sum1(BAResult[Subfamily == "Orthocladiinae"])/sum1(BAResult[Family == "Chironomidae"]),
    ###Other Taxa Metrics###
    Acari_Percent = sum1(BAResult[Subclass == "Acari"])/sum1(BAResult),
    Acari_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subclass == "Acari"))/sum1(distinct_SAFIT2=="Distinct"), 
    Acari_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subclass == "Acari")),
    Oligochaeta_Percent = sum1(BAResult[Subclass == "Oligochaeta"])/sum1(BAResult),
    Oligochaeta_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subclass == "Oligochaeta"))/sum1(distinct_SAFIT2=="Distinct"), 
    Oligochaeta_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subclass == "Oligochaeta")),
    Amphipoda_Percent = sum1(BAResult[Order == "Amphipoda"])/sum1(BAResult),
    Amphipoda_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Amphipoda"))/sum1(distinct_SAFIT2=="Distinct"), 
    Amphipoda_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Amphipoda")),
    Ostracoda_Percent = sum1(BAResult[Order == "Ostracoda"])/sum1(BAResult),
    Ostracoda_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Ostracoda"))/sum1(distinct_SAFIT2=="Distinct"), 
    Ostracoda_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Order == "Ostracoda")),
    Crustacea_Percent = sum1(BAResult[Subphylum == "Crustacea"])/sum1(BAResult),
    Crustacea_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Subphylum  == "Crustacea"))/sum1(distinct_SAFIT2=="Distinct"), 
    Crustacea_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Subphylum  == "Crustacea")),
    Noninsect_Percent = sum1(BAResult[Class != "Insecta"])/sum1(BAResult),
    Noninsect_PercentTaxa = sum1(distinct_SAFIT2=="Distinct" & (Class != "Insecta"))/sum1(distinct_SAFIT2=="Distinct"), 
    Noninsect_Taxa = sum1(distinct_SAFIT2=="Distinct" & (Class != "Insecta"))
  )
}

