#' @name Invasive
#' @title Invasive Taxa Related Metrics
#' 
#' @description Calculates various metrics based on Invasive taxa
#' 
#' @usage 
#' Invasive_Percent(x)
#' Invasive_PercentTaxa(x)
#' Invasive_Taxa(x)
#' 
#' @param x An object of class BMIprc or BMIagg
#' 
#' @S3method Invasive_Percent BMIprc 
#' @S3method Invasive_Percent BMIagg
#' @S3method Invasive_PercentTaxa BMIprc 
#' @S3method Invasive_PercentTaxa BMIagg
#' @S3method Invasive_Taxa BMIprc 
#' @S3method Invasive_Taxa BMIagg
#' @export Invasive_Percent
#' @export Invasive_PercentTaxa
#' @export Invasive_Taxa
#' @import plyr
#' @include loadMetaData.r


Invasive_Percent <- function(x){
  UseMethod("Invasive_Percent", x)  
}
Invasive_PercentTaxa <- function(x){
  UseMethod("Invasive_PercentTaxa", x)
}
Invasive_Taxa <- function(x){
  UseMethod("Invasive_Taxa", x)
}

Invasive_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Invasive ==1])/sum(df$BAResult.subsample)
  })
}

Invasive_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Invasive ==1])/sum(df$BAResult)
  })
}

Invasive_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$Invasive ==1 ,])/nrow(df[!duplicated(df[, "SAFIT2"]),])
  })
}

Invasive_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$Invasive ==1, ])/nrow(df[!duplicated(df[, "SAFIT2"]),])
  })
}

Invasive_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$Invasive ==1, ])
  })
}

Invasive_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Invasive <- metadata$Invasive[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[!duplicated(df$SAFIT2) & df$Invasive ==1, ])
  })
}