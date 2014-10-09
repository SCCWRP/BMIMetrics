#' @name Shredder
#' @title Shredder related metrics
#' 
#' Calculates metrics related to the Shredder feeding group.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Shredder_Percent(x)
#' Shredder_PercentTaxa(x)
#' Shredder_Taxa(x)
#' 
#' @export Shredder_Percent
#' @export Shredder_PercentTaxa
#' @export Shredder_Taxa
#' @S3method Shredder_Percent BMIagg
#' @S3method Shredder_PercentTaxa BMIagg
#' @S3method Shredder_Taxa BMIagg
#' @S3method Shredder_Percent BMIprc
#' @S3method Shredder_PercentTaxa BMIprc
#' @S3method Shredder_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r 

Shredder_Percent <- function(x){
  UseMethod("Shredder_Percent", x)
}

Shredder_PercentTaxa <- function(x){
  UseMethod("Shredder_PercentTaxa", x)
}

Shredder_Taxa <- function(x){
  UseMethod("Shredder_Taxa", x)
}

Shredder_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$FunctionalFeedingGroup == "SH")])/sum(df$BAResult.subsample)
  })
}

Shredder_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$FunctionalFeedingGroup == "SH")])/sum(df$BAResult)
  })
}

Shredder_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(!duplicated(df$SAFIT1) & df$FunctionalFeedingGroup == "SH"), ])/nrow(df[!duplicated(df[, "SAFIT1"]),])
  })
}

Shredder_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(!duplicated(df$SAFIT1) & df$FunctionalFeedingGroup == "SH"), ])/nrow(df[!duplicated(df[, "SAFIT1"]),])
  })
}

Shredder_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(!duplicated(df$SAFIT1) & df$FunctionalFeedingGroup == "SH"), ])
  })
}

Shredder_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(!duplicated(df$SAFIT1) & df$FunctionalFeedingGroup == "SH"), ])
  })
}