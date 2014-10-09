#' Crustacea Related Metrics
#' 
#' Calculates various metrics based on Crustacea taxa
#' 
#' @param x An object of class BMIprc or BMIagg
#' @param level Either "SAFIT1" or "SAFIT2"
#' @export Crustacea_Percent
#' @export Ostracoda_Percent
#' @S3method Crustacea_Percent BMIagg
#' @S3method Crustacea_Percent BMIprc
#' @S3method Ostracoda_Percent BMIagg
#' @S3method Ostracoda_Percent BMIprc
#' @import plyr
#' @include loadMetaData.r

Crustacea_Percent <- function(x, ...){
  UseMethod("Crustacea_Percent", x)  
}
Ostracoda_Percent <- function(x, ...){
  UseMethod("Ostracoda_Percent", x)
}

Crustacea_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Subphylum <- metadata$Subphylum[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Subphylum =="Crustacea"])/sum(df$BAResult.subsample)
  })
}

Crustacea_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Subphylum <- metadata$Subphylum[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Subphylum =="Crustacea"])/sum(df$BAResult)
  })
}

Ostracoda_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Class =="Ostracoda"])/sum(df$BAResult.subsample)
  })
}

Ostracoda_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Class <- metadata$Class[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Class =="Ostracoda"])/sum(df$BAResult)
  })
}