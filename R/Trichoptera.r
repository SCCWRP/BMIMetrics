#' @name Trichoptera
#' @title Trichoptera Related Metrics
#' 
#' Calculates various metrics based on Trichoptera taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Trichoptera_Percent(x)
#' Trichoptera_PercentTaxa(x)
#' Trichoptera_Taxa(x)
#' 
#' @export Trichoptera_Percent
#' @export Trichoptera_PercentTaxa
#' @export Trichoptera_Taxa
#' @S3method Trichoptera_Percent BMIagg
#' @S3method Trichoptera_PercentTaxa BMIagg
#' @S3method Trichoptera_Taxa BMIagg
#' @S3method Trichoptera_Percent BMIprc
#' @S3method Trichoptera_PercentTaxa BMIprc
#' @S3method Trichoptera_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r
#'


Trichoptera_Percent <- function(x, ...){
  UseMethod("Trichoptera_Percent", x)  
}
Trichoptera_PercentTaxa <- function(x, ...){
  UseMethod("Trichoptera_PercentTaxa", x)
}
Trichoptera_Taxa <- function(x, ...){
  UseMethod("Trichoptera_Taxa", x)
}

Trichoptera_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Trichoptera"])/sum(df$BAResult.subsample)
  })
}

Trichoptera_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Trichoptera"])/sum(df$BAResult)
  })
}

Trichoptera_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Trichoptera", "SAFIT1"])/nrow(df)
  })
}

Trichoptera_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Trichoptera", "SAFIT1"])/nrow(df)
  })
}

Trichoptera_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Trichoptera", "SAFIT1"])
  })
}

Trichoptera_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Trichoptera", "SAFIT1"])
  })
}