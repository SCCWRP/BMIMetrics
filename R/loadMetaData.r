#' loadMetaData
#' 
#' Calls the taxonomic data used in BMI analysis in a data frame
#' 
#' @export 
#' 

loadMetaData <- function(){
  load(system.file("metadata.rdata", package="BMIMetrics"))
  #metadata <- taxonomy
  metadata
}
