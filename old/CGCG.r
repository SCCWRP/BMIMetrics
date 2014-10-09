#' @name CFCG
#' @title CFCG
#' 
#' Calculates metrics related to collector foragers/ collector gatherers.
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' CFCG_Percent(x)
#' CFCG_PercentTaxa(x)
#' CFCG_Taxa(x)
#' 
#' @export CFCG_Percent
#' @export CFCG_PercentTaxa
#' @export CFCG_Taxa
#' @S3method CFCG_Percent BMIagg
#' @S3method CFCG_PercentTaxa BMIagg
#' @S3method CFCG_Taxa BMIagg
#' @S3method CFCG_Percent BMIprc
#' @S3method CFCG_PercentTaxa BMIprc
#' @S3method CFCG_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r 

CFCG_Percent <- function(x){
  UseMethod("CFCG_Percent", x)
}

CFCG_PercentTaxa <- function(x){
  UseMethod("CFCG_PercentTaxa", x)
}

CFCG_Taxa <- function(x){
  UseMethod("CFCG_Taxa", x)
}

CFCG_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$FunctionalFeedingGroup %in% c("CF", "CG"))])/sum(df$BAResult.subsample)
  })
}

CFCG_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$FunctionalFeedingGroup %in% c("CF", "CG"))])/sum(df$BAResult)
  })
}

CFCG_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$FunctionalFeedingGroup %in% c("CF", "CG"), ])/nrow(df[!duplicated(df[, "SAFIT2"]),])
  })
}

CFCG_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$FunctionalFeedingGroup %in% c("CF", "CG"), ])/nrow(df[!duplicated(df[, "SAFIT2"]),])
  })
}

CFCG_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult.subsample >0 & x$distinct_SAFIT2 == "Distinct",], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$FunctionalFeedingGroup %in% c("CF", "CG"), ])
  })
}

CFCG_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "FunctionalFeedingGroup")])
  ddply(x[x$BAResult >0 & x$distinct_SAFIT2 == "Distinct", ], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$FunctionalFeedingGroup %in% c("CF", "CG"), ])
  })
}