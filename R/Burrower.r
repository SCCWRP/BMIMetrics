#' @name Burrower
#' @title Burrower
#' 
#' Calculates metrics related to burrowing.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Burrower_Percent(x)
#' Burrower_PercentTaxa(x)
#' Burrower_Taxa(x)
#' 
#' @export Burrower_Percent
#' @export Burrower_PercentTaxa
#' @export Burrower_Taxa
#' @S3method Burrower_Percent BMIagg
#' @S3method Burrower_PercentTaxa BMIagg
#' @S3method Burrower_Taxa BMIagg
#' @S3method Burrower_Percent BMIprc
#' @S3method Burrower_PercentTaxa BMIprc
#' @S3method Burrower_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Burrower_Percent <- function(x){
  UseMethod("Burrower_Percent", x)
}

Burrower_PercentTaxa <- function(x){
  UseMethod("Burrower_PercentTaxa", x)
}

Burrower_Taxa <- function(x){
  UseMethod("Burrower_Taxa", x)
}

Burrower_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Habit =="BU")])/sum(df$BAResult.subsample)
  })
}

Burrower_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Habit =="BU")])/sum(df$BAResult)
  })
}

Burrower_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit =="BU"), ])/nrow(df)
  })
}

Burrower_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit =="BU"), ])/nrow(df)
  })
}

Burrower_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit =="BU"), ])
  })
}

Burrower_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit =="BU"), ])
  })
}