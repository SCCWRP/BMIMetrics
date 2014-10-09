#' @name Clinger
#' @title Clinger
#' 
#' Calculates metrics related to the clinger habit group.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Clinger_Percent(x)
#' Clinger_PercentTaxa(x)
#' Clinger_Taxa(x)
#' 
#' @export Clinger_Percent
#' @export Clinger_PercentTaxa
#' @export Clinger_Taxa
#' @S3method Clinger_Percent BMIagg
#' @S3method Clinger_PercentTaxa BMIagg
#' @S3method Clinger_Taxa BMIagg
#' @S3method Clinger_Percent BMIprc
#' @S3method Clinger_PercentTaxa BMIprc
#' @S3method Clinger_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Clinger_Percent <- function(x){
  UseMethod("Clinger_Percent", x)
}

Clinger_PercentTaxa <- function(x){
  UseMethod("Clinger_PercentTaxa", x)
}

Clinger_Taxa <- function(x){
  UseMethod("Clinger_Taxa", x)
}

Clinger_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Habit == "CN")])/sum(df$BAResult.subsample)
  })
}

Clinger_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Habit == "CN")])/sum(df$BAResult)
  })
}

Clinger_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    length(unique(df$SAFIT1[which(df$Habit == "CN")]))/length(unique(df$SAFIT1))
  })
}

Clinger_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    length(unique(df$SAFIT1[which(df$Habit == "CN")]))/length(unique(df$SAFIT1))
  })
}

Clinger_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    length(unique(df$SAFIT1[which(df$Habit == "CN")]))
  })
}

Clinger_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT1 == "Distinct",], "SampleID", function(df){
    length(unique(df$SAFIT1[which(df$Habit == "CN")]))
  })
}