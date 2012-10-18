#' @name Noninsect
#' @title Noninsect Related Metrics
#' 
#' Calculates various metrics based on Noninsect taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Noninsect_Percent(x)
#' Noninsect_PercentTaxa(x)
#' Noninsect_Taxa(x)
#' 
#' @export Noninsect_Percent
#' @export Noninsect_PercentTaxa
#' @export Noninsect_Taxa
#' @S3method Noninsect_Percent BMIagg
#' @S3method Noninsect_PercentTaxa BMIagg
#' @S3method Noninsect_Taxa BMIagg
#' @S3method Noninsect_Percent BMIprc
#' @S3method Noninsect_PercentTaxa BMIprc
#' @S3method Noninsect_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


Noninsect_Percent <- function(x, ...){
  UseMethod("Noninsect_Percent", x)  
}
Noninsect_PercentTaxa <- function(x, ...){
  UseMethod("Noninsect_PercentTaxa", x)
}
Noninsect_Taxa <- function(x, ...){
  UseMethod("Noninsect_Taxa", x)
}

Noninsect_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Class != "Insecta"])/sum(df$BAResult.subsample)
  })
}

Noninsect_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Class != "Insecta"])/sum(df$BAResult)
  })
}

Noninsect_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Class != "Insecta", "SAFIT1"])/nrow(df)
  })
}

Noninsect_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Class != "Insecta", "SAFIT1"])/nrow(df)
  })
}

Noninsect_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Class != "Insecta", "SAFIT1"])
  })
}

Noninsect_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Class != "Insecta", "SAFIT1"])
  })
}