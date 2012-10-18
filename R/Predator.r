#' @name Predator
#' @title Predator related metrics
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Predator_Percent(x)
#' Predator_PercentTaxa(x)
#' Predator_Taxa(x)
#' 
#' @export Predator_Percent
#' @export Predator_PercentTaxa
#' @export Predator_Taxa
#' @S3method Predator_Percent BMIagg
#' @S3method Predator_PercentTaxa BMIagg
#' @S3method Predator_Taxa BMIagg
#' @S3method Predator_Percent BMIprc
#' @S3method Predator_PercentTaxa BMIprc
#' @S3method Predator_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Predator_Percent <- function(x){
  UseMethod("Predator_Percent", x)
}

Predator_PercentTaxa <- function(x){
  UseMethod("Predator_PercentTaxa", x)
}

Predator_Taxa <- function(x){
  UseMethod("Predator_Taxa", x)
}

Predator_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$FunctionalFeedingGroup == "P")])/sum(df$BAResult.subsample)
  })
}

Predator_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$FunctionalFeedingGroup == "P")])/sum(df$BAResult)
  })
}

Predator_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "P")])/nrow(df)
  })
}

Predator_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "P")])/nrow(df)
  })
}

Predator_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "P")])
  })
}

Predator_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$FunctionalFeedingGroup == "P")])
  })
}