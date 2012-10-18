\docType{package}
\name{BMIMetrics}
\alias{BMIMetrics}
\alias{BMIMetrics-package}
\title{Suite of metric calculators for benthic macroinvertebrate data}
\description{
  To use this package, first read in your data, or use the
  example data set with \code{data(bugdata)} and check it
  using \code{\link{BMI}}. Then, aggregate the data to
  consistent taxonomic effort levels using
  \code{\link{aggregate.BMI}}. Next make the sample size
  consistent using \code{sample}. Any of the metric
  functions can be used on objects of class \code{BMIagg}
  or class \code{BMIprc}.
}
\examples{
data(bugdata)
bugdata <- BMI(bugdata)
bugdata.agg <- aggregate(bugdata)
bugdata.prc <- sample(bugdata.agg)

Chironomidae_PercentTaxa(bugdata.prc)
Chironomidae_PercentTaxa(bugdata.agg)

Invasive_Percent(bugdata.prc)
Invasive_Percent(bugdata.agg)

Shannon_Diversity(bugdata.prc)
Shannon_Diversity(bugdata.agg)
}

