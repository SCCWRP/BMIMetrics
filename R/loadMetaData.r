###Load Metadata

loadMetaData <- function(){
  load(system.file("metadata.rdata", package="BMIMetrics"))
  #metadata <- taxonomy
  metadata
}
