#' @name EPT
#' @title EPT Related Metrics
#' 
#' Calculates various metrics based on EPT taxa
#' 
#' @param level Either "SAFIT1" or "SAFIT2"
#' @param x An object of class BMIprc or BMIagg
#' 
#' @usage 
#' EPT_Percent(x)
#' EPT_PercentTaxa(x)
#' EPT_Taxa(x)
#' 
#' @export EPT_Percent
#' @export EPT_PercentTaxa
#' @export EPT_Taxa
#' @S3method EPT_Percent BMIagg
#' @S3method EPT_PercentTaxa BMIagg
#' @S3method EPT_Taxa BMIagg
#' @S3method EPT_Percent BMIprc
#' @S3method EPT_PercentTaxa BMIprc
#' @S3method EPT_Taxa BMIprc
#' @import plyr
#' @include loadMetaData.r


EPT_Percent <- function(x, ...){
  UseMethod("EPT_Percent", x)  
}
EPT_PercentTaxa <- function(x, ...){
  UseMethod("EPT_PercentTaxa", x)
}
EPT_Taxa <- function(x, ...){
  UseMethod("EPT_Taxa", x)
}

EPT_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera")])/sum(df$BAResult.subsample)
  })
}

EPT_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera")])/sum(df$BAResult)
  })
}

EPT_PercentTaxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera"), level]))/nrow(df[!duplicated(df[, level]),])
  })
}

EPT_PercentTaxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera"), level]))/nrow(df[!duplicated(df[, level]),])
  })
}

EPT_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera"), level]))
  })
}

EPT_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  metadata <- loadMetaData()
  x$Order <- metadata$Order[match(x$FinalID, metadata$FinalID)]
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    length(unique(df[df$Order %in% c("Ephemeroptera", "Trichoptera", "Plecoptera"), level]))
  })
}