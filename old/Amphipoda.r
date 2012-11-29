#' Amphipoda_Percent
#' 
#' Calculates the percent individuals belonging to Amphipoda per sample
#' 
#' @param x An object of class BMIprc or BMIagg
#' @export 
#' @S3method Amphipoda_Percent BMIagg
#' @S3method Amphipoda_Percent BMIprc
#' @import plyr
#' @include loadMetaData.r

Amphipoda_Percent <- function(x){
  UseMethod("Amphipoda_Percent", x)
}

Amphipoda_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order =="Amphipoda"])/sum(df$BAResult.subsample)
  })
}

Amphipoda_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order =="Amphipoda"])/sum(df$BAResult)
  })
}