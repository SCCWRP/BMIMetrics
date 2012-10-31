#' @name Tolerance
#' @aliases Tolerant
#' @aliases Intolerant
#' @title Tolerance related metrics
#' 
#' Calculates metrics related to tolerance values.
#' 
#' @usage 
#' Intolerant_Percent(x)
#' Intolerant_PercentTaxa(x)
#' Intolerant_Taxa(x)
#' Tolerant_Percent(x)
#' Tolerant_PercentTaxa(x)
#' Tolerant_Taxa(x)
#' 
#' @export Intolerant_Percent
#' @export Intolerant_PercentTaxa
#' @export Intolerant_Taxa
#' @S3method Intolerant_Percent BMIagg
#' @S3method Intolerant_PercentTaxa BMIagg
#' @S3method Intolerant_Taxa BMIagg
#' @S3method Intolerant_Percent BMIprc
#' @S3method Intolerant_PercentTaxa BMIprc
#' @S3method Intolerant_Taxa BMIprc
#' @export Tolerant_Percent
#' @export Tolerant_PercentTaxa
#' @export Tolerant_Taxa
#' @S3method Tolerant_Percent BMIagg
#' @S3method Tolerant_PercentTaxa BMIagg
#' @S3method Tolerant_Taxa BMIagg
#' @S3method Tolerant_Percent BMIprc
#' @S3method Tolerant_PercentTaxa BMIprc
#' @S3method Tolerant_Taxa BMIprc
#' @export ToleranceValue
#' @S3method ToleranceValue BMIagg
#' @S3method ToleranceValue BMIprc
#' @import plyr
#' @include loadMetaData.r

Intolerant_Percent <- function(x){
  UseMethod("Intolerant_Percent", x)
}

Intolerant_PercentTaxa <- function(x){
  UseMethod("Intolerant_PercentTaxa", x)
}

Intolerant_Taxa <- function(x){
  UseMethod("Intolerant_Taxa", x)
}

ToleranceValue <- function(x){
  UseMethod("ToleranceValue", x)
}
Tolerant_Percent <- function(x){
  UseMethod("Tolerant_Percent", x)
}
Tolerant_PercentTaxa <- function(x){
  UseMethod("Tolerant_PercentTaxa", x)
}
Tolerant_Taxa <- function(x){
  UseMethod("Tolerant_Taxa", x)
}

Intolerant_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$ToleranceValue <= 2)])/sum(df$BAResult.subsample)
  })
}

Intolerant_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$ToleranceValue <= 2)])/sum(df$BAResult)
  })
}

Intolerant_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue <= 2), ])/nrow(df)
  })
}

Intolerant_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue <= 2), ])/nrow(df)
  })
}

Intolerant_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue <= 2), ])
  })
}

Intolerant_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue <= 2), ])
  })
}

Tolerant_Percent.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult.subsample[which(df$ToleranceValue >= 8)])/sum(df$BAResult.subsample)
  })
}

Tolerant_Percent.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x, "SampleID", function(df){
    sum(df$BAResult[which(df$ToleranceValue >= 8)])/sum(df$BAResult)
  })
}

Tolerant_PercentTaxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue >= 8), ])/nrow(df)
  })
}

Tolerant_PercentTaxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue >= 8), ])/nrow(df)
  })
}

Tolerant_Taxa.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult.subsample >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue >= 8), ])
  })
}

Tolerant_Taxa.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    nrow(df[which(df$ToleranceValue >= 8), ])
  })
}

ToleranceValue.BMIprc <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    p <- !is.na(df$ToleranceValue)
    sum(df$BAResult.subsample[p] * as.numeric(df$ToleranceValue[p]), na.rm=T)/sum(df$BAResult.subsample[p])
  })
}

ToleranceValue.BMIagg <- function(x){
  metadata <- loadMetaData()
  x <- merge(x, metadata[, c("FinalID", "LifeStageCode", "ToleranceValue")])
  ddply(x[x$BAResult >0, ], "SampleID", function(df){
    p <- !is.na(df$ToleranceValue)
    sum(df$BAResult[p] * as.numeric(df$ToleranceValue[p]), na.rm=T)/sum(df$BAResult[p])
  })
}