#' @name Acari
#' @title Acari Related Metrics
#' 
#' Calculates various metrics based on Acari taxa
#' 
#' @usage
#' Acari_Percent(x)
#' Acari_PercentTaxa(x)
#' Acari_Taxa(x)
#' 
#' @param x An object of class BMIprc or BMIagg 
#' @export Acari_Percent
#' @export Acari_PercentTaxa
#' @export Acari_Taxa
#' @S3method Acari_Percent BMIagg
#' @S3method Acari_PercentTaxa BMIagg
#' @S3method Acari_Taxa BMIagg
#' @S3method Acari_Percent BMIprc
#' @S3method Acari_PercentTaxa BMIprc
#' @S3method Acari_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r

Acari_Percent <- function(x){
  UseMethod("Acari_Percent", x)  
}
Acari_PercentTaxa <- function(x){
  UseMethod("Acari_PercentTaxa", x)
}
Acari_Taxa <- function(x){
  UseMethod("Acari_Taxa", x)
}

Acari_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Subclass =="Acari"])/sum(df$BAResult.subsample)
  })
}

Acari_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Subclass =="Acari"])/sum(df$BAResult)
  })
}

Acari_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[df$Subclass =="Acari"])/nrow(dfdf)
  })
}

Acari_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x$SAFIT2 == "Distinct", ], "SampleID", function(df){
    nrow(df[df$Subclass =="Acari"])/nrow(dfdf)
  })
}

Acari_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x$SAFIT2 == "Distinct", ], "SampleID", function(df){
    nrow(df[df$Subclass =="Acari"])
  })
}

Acari_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Subclass <- metadata$Subclass[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x$SAFIT2 == "Distinct", ], "SampleID", function(df){
    nrow(df[df$Subclass =="Acari"])
  })
}