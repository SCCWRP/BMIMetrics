#' Sample Bug Results
#' 
#' Takes bug data and subsamples those that have more than 500 individuals in the sample
#' 
#' @param x An object of class BMIagg
#' @S3method sample BMI
#' @S3method sample BMIagg
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

sample.BMI <- function(x){
  print("Aggregate data before subsampling.")
}

sample.BMIagg <- function(x){
  x$SampleID <- as.character(x$SampleID)
  rarifydown <- function(data){unlist(sapply(unique(data$SampleID), function(sample){
    v <- data[data$SampleID==sample, "BAResult"]
    if(sum(v)>=500){rrarefy(v, 500)} else
    {v}
  }))}
  x$BAResult.subsample <- rarifydown(x)
  class(x) <- c("BMIprc", "BMIagg", "BMI", "data.frame")
  x
}