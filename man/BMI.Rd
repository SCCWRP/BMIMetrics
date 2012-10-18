\name{BMI}
\alias{BMI}
\title{BMI}
\usage{
  BMI(x)
}
\arguments{
  \item{x}{A data frame}
}
\description{
  Checks a data frame and converts it to an object of class
  BMI. The data frame must have the following columns:
  SampleID, FinalID, LifeStageCode, and BAResult. A column
  for DistinctCode may be included; if it is absent the
  column will be created and filled with NAs. All
  FinalID/LifeStageCode combinations in the data frame must
  occur in the internal database, or the function will
  return an error.
}

