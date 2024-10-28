#' loadMetaData
#' 
#' Calls the taxonomic data used in BMI analysis in a data frame
#' 
#' @export 
#' 

loadMetaData <- function(metadata.year = 2025){
  if (!(metadata.year %in% c(2024,2025))) {
    stop("Valid metadata years would be 2024 and 2025")
  }
  load(system.file( paste0("metadata_STE", metadata.year, ".rdata"), package="BMIMetrics"))
  #metadata <- taxonomy
  metadata
}
