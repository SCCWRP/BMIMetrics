#' @name Total_Taxa
#' @title Benthic Community Metrics
#' 
#' Calculates metrics concerning total number of taxa and sample diversity.
#' 
#' @usage
#' Total_Taxa
#' Shannon_Diversity
#' Simpson_Diversity
#' 
#' @param x An object of class BMIprc or BMIagg
#' @param level Either "SAFIT1" or "SAFIT2"
#' @export Total_Taxa
#' @export Shannon_Diversity
#' @export Simpson_Diversity
#' @S3method Total_Taxa BMIagg
#' @S3method Shannon_Diversity BMIagg
#' @S3method Simpson_Diversity BMIagg
#' @S3method Total_Taxa BMIprc
#' @S3method Shannon_Diversity BMIprc
#' @S3method Simpson_Diversity BMIprc
#' @import plyr
#' @import vegan
#' @importFrom reshape cast
#' @include loadMetaData.r

Total_Taxa <- function(x, level){
  UseMethod("Total_Taxa", x)
}
Shannon_Diversity <- function(x, level){
  UseMethod("Shannon_Diversity", x)
}
Simpson_Diversity <- function(x, level){
  UseMethod("Simpson_Diversity", x)
}

Total_Taxa.BMIprc <- function(x, level = "SAFIT1"){
  ddply(x[x$BAResult.subsample >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    nrow(df)
  })
}

Total_Taxa.BMIagg <- function(x, level = "SAFIT1"){
  ddply(x[x$BAResult >0 & x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    nrow(df)
  })
}

Shannon_Diversity.BMIprc <- function(x, level = "SAFIT1"){
  ddply(x[x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    community <- cast(as.formula(paste("SampleID ~", level)), data=df, fun.aggregate=sum, value="BAResult.subsample")
    diversity(community, index="shannon")
  })
}

Shannon_Diversity.BMIagg <- function(x, level = "SAFIT1"){
  ddply(x[x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    community <- cast(as.formula(paste("SampleID ~", level)), data=df, fun.aggregate=sum, value="BAResult")
    diversity(community, index="shannon")
  })
}

Simpson_Diversity.BMIprc <- function(x, level = "SAFIT1"){
  ddply(x[x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    community <- cast(as.formula(paste("SampleID ~", level)), data=df, fun.aggregate=sum, value="BAResult.subsample")
    diversity(community, index="simpson")
  })
}

Simpson_Diversity.BMIagg <- function(x, level = "SAFIT1"){
  ddply(x[x[, paste("distinct_", level, sep="")] == "Distinct", ], "SampleID", function(df){
    community <- cast(as.formula(paste("SampleID ~", level)), data=df, fun.aggregate=sum, value="BAResult")
    diversity(community, index="simpson")
  })
}