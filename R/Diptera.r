#' Diptera Related Metrics
#' 
#' Calculates various metrics based on Diptera taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' Diptera_Percent(x)
#' Diptera_PercentTaxa(x)
#' Diptera_Taxa(x)
#' 
#' @export Diptera_Percent
#' @export Diptera_PercentTaxa
#' @export Diptera_Taxa
#' @S3method Diptera_Percent BMIagg
#' @S3method Diptera_PercentTaxa BMIagg
#' @S3method Diptera_Taxa BMIagg
#' @S3method Diptera_Percent BMIprc
#' @S3method Diptera_PercentTaxa BMIprc
#' @S3method Diptera_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


Diptera_Percent <- function(x, ...){
  UseMethod("Diptera_Percent", x)  
}
Diptera_PercentTaxa <- function(x, ...){
  UseMethod("Diptera_PercentTaxa", x)
}
Diptera_Taxa <- function(x, ...){
  UseMethod("Diptera_Taxa", x)
}

Diptera_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Diptera"])/sum(df$BAResult.subsample)
  })
}

Diptera_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Diptera"])/sum(df$BAResult)
  })
}

Diptera_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Diptera", "SAFIT1"])/nrow(df)
  })
}

Diptera_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Diptera", "SAFIT1"])/nrow(df)
  })
}

Diptera_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Diptera", "SAFIT1"])
  })
}

Diptera_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(df[df$Order =="Diptera", "SAFIT1"])
  })
}