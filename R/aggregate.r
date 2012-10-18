#' Aggregate FinalID
#' 
#' Aggregates to  taxonomic effort levels SAFIT1 and SAFIT2
#' 
#' @param x An object of class BMI
#' @export 
#' @import plyr
#' @include loadMetaData.r

aggregate.BMI <- function(x){
  metadata <- loadMetaData()
  ###Aggregate FinalID###
  x <- ddply(x, "SampleID", function(df){
    ddply(df, "FinalID", function(sdf){
      id <- unique(sdf[, !(names(sdf) %in% "BAResult")])
      BAResult <- sum(sdf$BAResult)
      cbind(id, BAResult)
    })
  })
  
  for(effort in c("SAFIT1", "SAFIT2")){
    ###Match to STE###
    x[, effort] <- rep(NA, length(x$FinalID))
    x[, effort] <- metadata[match(x$FinalID, metadata$FinalID), as.character(effort)]
    x[, effort] <- as.character(x[, effort])
    
    
    ###Determine Distinctiveness###
    distinctsorter <- function(taxon, data){
      level <- metadata$TaxonomicLevelCode[match(taxon, metadata$FinalID)] 
      levelname <- as.character(metadata$TaxonomicLevelName[match(taxon, metadata$FinalID)])
      if(is.na(levelname))return(T)
      samelevel <- metadata$FinalID[which(metadata[, levelname] == taxon)]
      matchedlevel <- x[, effort] %in% metadata[match(samelevel, metadata$FinalID), effort]
      result <- which(metadata$TaxonomicLevelCode[match(data[matchedlevel, effort], metadata$FinalID)] >= level)
      length(result) > 1
    }
    distinctlist <- dlply(x, "SampleID", function(df){
      sapply(1:nrow(df), function(i){
        ifelse(distinctsorter(df[i, effort], df), "Non-Distinct", "Distinct")
      })})
    distinctCol <- paste("distinct_", effort, sep="")
    x[, distinctCol] <- unlist(distinctlist)
    ###Override##
    x$DistinctCode <- as.character(x$DistinctCode)
    if(any(!is.na(x$DistinctCode))){
      x[which(x[, distinctCol] == "Non-distinct" & x$DistinctCode == "Yes"), distinctCol] <- "Distinct"
    }
  } 
  class(x) <- c("BMIagg", "BMI", "data.frame")
  x
}


