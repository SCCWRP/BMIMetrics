#' @name Scraper
#' @title Scraper related metrics
#' 
#' Calculates metrics related to the Scraper feeding group.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Scraper_Percent(x)
#' Scraper_PercentTaxa(x)
#' Scraper_Taxa(x)
#' 
#' @export Scraper_Percent
#' @export Scraper_PercentTaxa
#' @export Scraper_Taxa
#' @S3method Scraper_Percent BMIagg
#' @S3method Scraper_PercentTaxa BMIagg
#' @S3method Scraper_Taxa BMIagg
#' @S3method Scraper_Percent BMIprc
#' @S3method Scraper_PercentTaxa BMIprc
#' @S3method Scraper_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Scraper_Percent <- function(x){
  UseMethod("Scraper_Percent", x)
}

Scraper_PercentTaxa <- function(x){
  UseMethod("Scraper_PercentTaxa", x)
}

Scraper_Taxa <- function(x){
  UseMethod("Scraper_Taxa", x)
}

Scraper_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$FunctionalFeedingGroup == "SC")])/sum(df$BAResult.subsample)
  })
}

Scraper_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$FunctionalFeedingGroup == "SC")])/sum(df$BAResult)
  })
}

Scraper_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "SC")])/nrow(df)
  })
}

Scraper_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "SC")])/nrow(df)
  })
}

Scraper_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "SC")])
  })
}

Scraper_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "SC")])
  })
}