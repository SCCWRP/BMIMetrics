#' @name Swimmer
#' @title Swimmer related metrics
#' 
#' Calculates metrics related to the Swimmer habit group.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Swimmer_Percent(x)
#' Swimmer_PercentTaxa(x)
#' Swimmer_Taxa(x)
#' 
#' @export Swimmer_Percent
#' @export Swimmer_PercentTaxa
#' @export Swimmer_Taxa
#' @S3method Swimmer_Percent BMIagg
#' @S3method Swimmer_PercentTaxa BMIagg
#' @S3method Swimmer_Taxa BMIagg
#' @S3method Swimmer_Percent BMIprc
#' @S3method Swimmer_PercentTaxa BMIprc
#' @S3method Swimmer_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r 

Swimmer_Percent <- function(x){
  UseMethod("Swimmer_Percent", x)
}

Swimmer_PercentTaxa <- function(x){
  UseMethod("Swimmer_PercentTaxa", x)
}

Swimmer_Taxa <- function(x){
  UseMethod("Swimmer_Taxa", x)
}

Swimmer_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Habit == "SW")])/sum(df$BAResult.subsample)
  })
}

Swimmer_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Habit == "SW")])/sum(df$BAResult)
  })
}

Swimmer_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit == "SW"), ])/nrow(df)
  })
}

Swimmer_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit == "SW"), ])/nrow(df)
  })
}

Swimmer_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit == "SW"), ])
  })
}

Swimmer_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    nrow(df[which(df$Habit == "SW"), ])
  })
}