#' @name Plecoptera
#' @title Plecoptera Related Metrics
#' 
#' Calculates various metrics based on Plecoptera taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Plecoptera_Percent(x)
#' Plecoptera_PercentTaxa(x)
#' Plecoptera_Taxa(x)
#' 
#' @export Plecoptera_Percent
#' @export Plecoptera_PercentTaxa
#' @export Plecoptera_Taxa
#' @S3method Plecoptera_Percent BMIagg
#' @S3method Plecoptera_PercentTaxa BMIagg
#' @S3method Plecoptera_Taxa BMIagg
#' @S3method Plecoptera_Percent BMIprc
#' @S3method Plecoptera_PercentTaxa BMIprc
#' @S3method Plecoptera_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


Plecoptera_Percent <- function(x, ...){
  UseMethod("Plecoptera_Percent", x)  
}
Plecoptera_PercentTaxa <- function(x, ...){
  UseMethod("Plecoptera_PercentTaxa", x)
}
Plecoptera_Taxa <- function(x, ...){
  UseMethod("Plecoptera_Taxa", x)
}

Plecoptera_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Plecoptera"])/sum(df$BAResult.subsample)
  })
}

Plecoptera_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Plecoptera"])/sum(df$BAResult)
  })
}

Plecoptera_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Plecoptera", level]))/nrow(df[!duplicated(df[, level]),])
  })
}

Plecoptera_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Plecoptera", level]))/nrow(df[!duplicated(df[, level]),])
  })
}

Plecoptera_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Plecoptera", level]))
  })
}

Plecoptera_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order =="Plecoptera", level]))
  })
}