#' @name Chironomidae
#' @title Chironomidae related metrics
#' 
#' @description Calculates metrics related to chironomids.
#' 
#' @usage
#' Chironomidae_PercentTaxa(x)
#' Chironomidae_Taxa(x)
#' Chironominae_Percent(x)
#' Chironominae_PercentOfMidges(x)
#' Tanypodinae_Percent(x)
#' Tanypodinae_PercentOfMidges(x)
#' 
#' @param x An object of class BMIprc or BMIagg
#' @export Chironomidae_PercentTaxa
#' @export Chironomidae_Taxa
#' @export Chironominae_Percent
#' @export Chironominae_PercentOfMidges
#' @export Tanypodinae_Percent
#' @export Tanypodinae_PercentOfMidges
#' @export Orthocladiinae_PercentOfMidges
#' @S3method Chironomidae_PercentTaxa BMIagg
#' @S3method Chironomidae_Taxa BMIagg
#' @S3method Chironominae_Percent BMIagg
#' @S3method Chironominae_PercentOfMidges BMIagg
#' @S3method Tanypodinae_Percent BMIagg
#' @S3method Tanypodinae_PercentOfMidges BMIagg
#' @S3method Chironomidae_PercentTaxa BMIprc
#' @S3method Chironomidae_Taxa BMIprc
#' @S3method Chironominae_Percent BMIprc
#' @S3method Chironominae_PercentOfMidges BMIprc
#' @S3method Tanypodinae_Percent BMIprc
#' @S3method Tanypodinae_PercentOfMidges BMIprc
#' @S3method Orthocladiinae_PercentOfMidges BMIagg
#' @S3method Orthocladiinae_PercentOfMidges BMIprc
#' @import plyr
#' @include loadMetaData.r

Chironomidae_PercentTaxa <- function(x){
  UseMethod("Chironomidae_PercentTaxa", x)
}
Chironomidae_Taxa <- function(x){
  UseMethod("Chironomidae_Taxa", x)
}
Chironominae_Percent <- function(x){
  UseMethod("Chironominae_Percent", x)
}
Chironominae_PercentOfMidges <- function(x){
  UseMethod("Chironominae_PercentOfMidges", x)
}
Tanypodinae_Percent <- function(x){
  UseMethod("Tanypodinae_Percent", x)
}
Tanypodinae_PercentOfMidges <- function(x){
  UseMethod("Tanypodinae_PercentOfMidges", x)
}

Orthocladiinae_PercentOfMidges <- function(x){
  UseMethod("Orthocladiinae_PercentOfMidges", x)
}


Chironomidae_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    length(df$SAFIT2[which(df$Family == "Chironomidae")])/nrow(df)
  })
}

Chironomidae_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    length(df$SAFIT2[which(df$Family == "Chironomidae")])/nrow(df)
  })
}

Chironomidae_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    length(df$SAFIT2[which(df$Family == "Chironomidae")])
  })
}

Chironomidae_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    length(df$SAFIT2[which(df$Family == "Chironomidae")])
  })
}

Chironominae_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Subfamily == "Chironominae")])/sum(df$BAResult.subsample)
  })
}

Chironominae_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Subfamily == "Chironominae")])/sum(df$BAResult)
  })
}

Tanypodinae_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Subfamily == "Tanypodinae")])/sum(df$BAResult.subsample)
  })
}

Tanypodinae_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Subfamily == "Tanypodinae")])/sum(df$BAResult)
  })
}

Tanypodinae_PercentOfMidges.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Subfamily == "Tanypodinae")])/sum(df$BAResult.subsample[which(df$Family == "Chironomidae")])
  })
}

Tanypodinae_PercentOfMidges.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Subfamily == "Tanypodinae")])/sum(df$BAResult[which(df$Family == "Chironomidae")])
  })
}

Chironominae_PercentOfMidges.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Subfamily == "Chironominae")])/sum(df$BAResult.subsample[which(df$Family == "Chironomidae")])
  })
}

Chironominae_PercentOfMidges.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Subfamily == "Chironominae")])/sum(df$BAResult[which(df$Family == "Chironomidae")])
  })
}

Orthocladiinae_PercentOfMidges.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$Subfamily == "Orthocladiinae")])/sum(df$BAResult.subsample[which(df$Family == "Chironomidae")])
  })
}

Orthocladiinae_PercentOfMidges.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "Family", "Subfamily")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$Subfamily == "Orthocladiinae")])/sum(df$BAResult[which(df$Family == "Chironomidae")])
  })
}