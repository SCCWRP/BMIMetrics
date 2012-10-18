#' @name Climber
#' @title Climber
#' 
#' Calculates metrics related to the climber habit group.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Climber_Percent(x)
#' Climber_PercentTaxa(x)
#' Climber_Taxa(x)
#' 
#' @export Climber_Percent
#' @export Climber_PercentTaxa
#' @export Climber_Taxa
#' @S3method Climber_Percent BMIagg
#' @S3method Climber_PercentTaxa BMIagg
#' @S3method Climber_Taxa BMIagg
#' @S3method Climber_Percent BMIprc
#' @S3method Climber_PercentTaxa BMIprc
#' @S3method Climber_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Climber_Percent <- function(x){
  UseMethod("Climber_Percent", x)
}

Climber_PercentTaxa <- function(x){
  UseMethod("Climber_PercentTaxa", x)
}

Climber_Taxa <- function(x){
  UseMethod("Climber_Taxa", x)
}

Climber_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Habit == "CB")])/sum(df$BAResult.subsample)
  })
}

Climber_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Habit == "CB")])/sum(df$BAResult)
  })
}

Climber_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$Habit == "CB")])/nrow(df)
  })
}

Climber_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$Habit == "CB")])/nrow(df)
  })
}

Climber_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult.subsample >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$Habit == "CB")])
  })
}

Climber_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Habit")])
  ddply(x[x$BAResult >0 & x$SAFIT1== "Distinct", ], "SampleID", function(df){
    nrow(df[which(df$Habit == "CB")])
  })
}