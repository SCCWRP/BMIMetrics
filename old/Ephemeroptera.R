#' @name Ephemeroptera
#' @title Ephemeroptera Related Metrics
#' 
#' Calculates various metrics based on Ephemeroptera taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Ephemeroptera_Percent(x)
#' Ephemeroptera_PercentTaxa(x)
#' Ephemeroptera_Taxa(x)
#' 
#' @export Ephemeroptera_Percent
#' @export Ephemeroptera_PercentTaxa
#' @export Ephemeroptera_Taxa
#' @S3method Ephemeroptera_Percent BMIagg
#' @S3method Ephemeroptera_PercentTaxa BMIagg
#' @S3method Ephemeroptera_Taxa BMIagg
#' @S3method Ephemeroptera_Percent BMIprc
#' @S3method Ephemeroptera_PercentTaxa BMIprc
#' @S3method Ephemeroptera_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


Ephemeroptera_Percent <- function(x, ...){
  UseMethod("Ephemeroptera_Percent", x)  
}
Ephemeroptera_PercentTaxa <- function(x, ...){
  UseMethod("Ephemeroptera_PercentTaxa", x)
}
Ephemeroptera_Taxa <- function(x, ...){
  UseMethod("Ephemeroptera_Taxa", x)
}

Ephemeroptera_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Ephemeroptera"])/sum(df$BAResult.subsample)
  })
}

Ephemeroptera_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Ephemeroptera"])/sum(df$BAResult)
  })
}

Ephemeroptera_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Ephemeroptera", level]))/nrow(df[!duplicated(df[, level]),])
  })
}

Ephemeroptera_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Ephemeroptera", level]))/nrow(df[!duplicated(df[, level]),])
  })
}

Ephemeroptera_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Ephemeroptera", level]))
  })
}

Ephemeroptera_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Ephemeroptera", level]))
  })
}