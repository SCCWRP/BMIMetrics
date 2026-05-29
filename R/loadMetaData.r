#' loadMetaData
#' 
#' Calls the taxonomic data used in BMI analysis in a data frame
#' 
#' @export 
#' 

loadMetaData <- function(metadata.year = 2026){
  if (!(metadata.year %in% c(2011, 2025, 2026))) {
    stop("Valid metadata years would be 2011, 2025, or 2026")
  }
  
  filename <- paste0("metadata_STE", metadata.year, ".rdata")
  
  load(system.file( filename, package="BMIMetrics"))
  #metadata <- taxonomy
  metadata
}
