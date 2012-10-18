#' BMI
#' 
#' Checks a data frame and converts it to an object of class BMI.
#' The data frame must have the following columns: SampleID, FinalID, LifeStageCode, and BAResult. 
#' A column for DistinctCode may be
#' included; if it is absent the column will be created and filled with
#' NAs. All FinalID/LifeStageCode combinations in the data frame must 
#' occur in the internal database, or the function will return an error.
#' 
#' @param x A data frame
#' @export 
#' @import stringr
#' @include loadMetaData.r
#' 

BMI <- function(x){
  metadata <- loadMetaData()
  
  ###Format Check###
  columns <- c("SampleID", "FinalID", "LifeStageCode", "BAResult")
  if(sum(columns %in% names(x)) != 4)
    stop(paste("The following columns are missing:", columns[!(columns %in% names(x))]))
  
  if(!("DistinctCode" %in% names(x)))x$DistinctCode <- rep(NA, nrow(x))
  
  ###Clean data###
  x$FinalID <- as.character(x$FinalID)
  x$FinalID <- str_trim(x$FinalID)
  x$LifeStageCode <- as.character(x$LifeStageCode)
  
  ###Data check###
  namecheck <- paste(x$FinalID, x$LifeStageCode) %in% paste(metadata$FinalID, metadata$LifeStageCode)
  if(length(which(!namecheck))>0)
    stop(paste("The follow FinalID/LifeStageCode combinations did not match with the internal database:",
               x[!namecheck, c("FinalID", "LifeStageCode")]))
  

  
  if(length(which(!(x$BAResult - round(x$BAResult) %in% 0)))>0)
    stop("BAResult may only contain whole numbers")
  
  if(length(which(x$BAResult < 0)))
    stop("BAResult may not contain negative values")
  
  ###Convert###
  class(x) <- c("BMI", "data.frame")
  x
}

