#' Coleoptera Related Metrics
#' 
#' Calculates various metrics based on Coleoptera taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Coleoptera_Percent(x)
#' Coleoptera_PercentTaxa(x)
#' Coleoptera_Taxa(x)
#' 
#' @export Coleoptera_Percent
#' @export Coleoptera_PercentTaxa
#' @export Coleoptera_Taxa
#' @S3method Coleoptera_Percent BMIagg
#' @S3method Coleoptera_PercentTaxa BMIagg
#' @S3method Coleoptera_Taxa BMIagg
#' @S3method Coleoptera_Percent BMIprc
#' @S3method Coleoptera_PercentTaxa BMIprc
#' @S3method Coleoptera_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


Coleoptera_Percent <- function(x, ...){
  UseMethod("Coleoptera_Percent", x)  
}
Coleoptera_PercentTaxa <- function(x, ...){
  UseMethod("Coleoptera_PercentTaxa", x)
}
Coleoptera_Taxa <- function(x, ...){
  UseMethod("Coleoptera_Taxa", x)
}

Coleoptera_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Coleoptera"])/sum(df$BAResult.subsample)
  })
}

Coleoptera_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Coleoptera"])/sum(df$BAResult)
  })
}

Coleoptera_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    length(df[df$Order =="Coleoptera", "SAFIT1"])/nrow(df)
  })
}

Coleoptera_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    length(df[df$Order =="Coleoptera", "SAFIT1"])/nrow(df)
  })
}

Coleoptera_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    length(df[df$Order =="Coleoptera", "SAFIT1"])
  })
}

Coleoptera_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    length(df[df$Order =="Coleoptera", "SAFIT1"])
  })
}