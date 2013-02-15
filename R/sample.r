#' Sample Bug Results
#' 
#' Takes bug data and subsamples those that have more than 500 individuals in the sample
#' 
#' @param x An object of class BMIagg
#' @S3method sample BMI
#' @S3method sample default
#' @export 
#' @import vegan
#' @include loadMetaData.r 


sample <- function(x, ...){
  UseMethod("sample", x)
}

sample.default <- function (x, size, replace = FALSE, prob = NULL) 
{
  if (length(x) == 1L && is.numeric(x) && x >= 1) {
    if (missing(size)) 
      size <- x
    .Internal(sample(x, size, replace, prob))
  }
  else {
    if (missing(size)) 
      size <- length(x)
    x[.Internal(sample(length(x), size, replace, prob))]
  }
}

sample.BMI <- function(x, number=500){
  x$SampleID <- as.character(x$SampleID)
  x$originalBAResult <- x$BAResult
  rarifydown <- function(data){unlist(sapply(unique(data$SampleID), function(sample){
    v <- data[data$SampleID==sample, "BAResult"]
    if(sum(v)>=number){rrarefy(v, number)} else
    {v}
  }))}
  x$BAResult <- rarifydown(x)
  x <- subset(x, BAResult != 0)
  x$SampleID <- as.factor(x$SampleID)
  class(x) <- c("BMIsub", "BMI", "data.frame")
  x
}