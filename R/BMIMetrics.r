#' Suite of metric calculators for benthic macroinvertebrate data
#'
#' To use this package, first read in your data, or use the example
#' data set with \code{data(bugdata)} and check it using \code{\link{BMI}}. Then, aggregate the data to
#' consistent taxonomic effort levels using \code{\link{aggregate.BMI}}.
#' Next make the sample size consistent using \code{sample}.
#' Any of the metric functions can be used on objects of class \code{BMIagg} or
#' class \code{BMIprc}. Metrics can be calculated using individual functions (see 
#' \code{help(package="BMImetrics")}), or using \code{\link{BMIall}}.
#' 
#' @examples
#' data(bugdata)
#' bugdata <- BMI(bugdata)
#' bugdata.agg <- aggregate(bugdata)
#' bugdata.prc <- sample(bugdata.agg)
#' 
#' metrics <- BMIall(bugdata.prc)
#' View(metrics)
#' 
#' @import vegan plyr stringr
#' @importFrom reshape cast
#' @docType package
#' @name BMIMetrics
#' @aliases BMIMetrics
NULL