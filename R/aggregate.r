#' Aggregate FinalID
#' 
#' Aggregates to  taxonomic effort levels SAFIT1 and SAFIT2
#' 
#' @param x An object of class BMI
#' @export 
#' @import plyr
#' @include loadMetaData.r


aggregate.BMI <- function(x, effortlevel = c("SAFIT1", "SAFIT2")){
  ###Load in metadata; this function won't work unless package is installed,
  ###but you can manually read in metadata.rdata, and then comment out this line
  metadata <- loadMetaData()
  
  ###Clean up###
  x <- x[, names(x) %in% c("StationCode", "SampleDate", "Replicate", "SampleID", "FinalID",
                           "LifeStageCode", "BAResult", "originalBAResult", "DistinctCode")]
  x$DistinctCode[is.na(x$DistinctCode)] <- 0
  
  ###Merge in metadata FinalID###
  x <- merge(x, metadata[, c(c("FinalID", "LifeStageCode", "TaxonomicLevelCode"),
                             effortlevel, paste0(effortlevel, "_effortlevel"))],
             by=c("FinalID", "LifeStageCode"), type="left")
  x <- x[apply(x[, effortlevel, drop=FALSE], 1, function(y)!any(y == "Exclude")), ]
  x <- na.omit(x) 


  agglist <- lapply(effortlevel, function(effort){
    
    ###Distinctiveness for those aggregated up, i.e. 
    ###FinalIDs that are higher resolution than SAFIT1/2
    x.aggup <- x[x$TaxonomicLevelCode > x[, paste(effort, "_effortlevel", sep="")], , drop=FALSE]
    x.aggup[, paste("distinct_", effort, sep= "")] <- rep(NA, nrow(x.aggup))
#     
    x.aggup <- ddply(x.aggup, "SampleID", function(fdf){
      ddply(fdf, effort, function(df){
        if(nrow(df)==1)df[, paste("distinct_", effort, sep="")] <- "Distinct" else{
          df <- df[order(df$LifeStageCode), , drop=FALSE]
          selected <- which(df$TaxonomicLevelCode == max(df$TaxonomicLevelCode))
          df[selected[1], paste("distinct_", effort, sep="")] <- "Distinct"
          df[!(1:nrow(df) %in% selected[1]), paste("distinct_", effort, sep="")] <- "Non-Distinct"
        }
        df
      })
    })
    x <- suppressMessages(join(x, x.aggup, type="full", match="first"))
#     
    ###Taxonomist Overide###
    if(any(x$DistinctCode > 0)){
      x[x$DistinctCode > 0, paste("distinct_", effort, sep="")] <- "Distinct"
    }
    ###Distinctiveness for those already above SAFIT aggregation (Check for nested taxa)
    ###Technically, this step could handle everything the first step did, but it's much slower due
    ###to having to reference the metadata repeatedly
    x$LifeStageCode <- as.factor(x$LifeStageCode)
    
    x <- ddply(x, "SampleID", function(df){
      nestcheck <- is.na(df[, paste("distinct_", effort, sep="")])
      if(sum(nestcheck)==0) return(df)
      df[which(nestcheck), paste("distinct_", effort, sep="")] <- sapply(which(nestcheck), function(i){
        slash <- strsplit(df$FinalID[i], "/ ")[[1]]
        ref <- if(length(slash) > 1) 
            c(slash[1], df$FinalID[i])
          else
            df$FinalID[i]
        nmatch <- metadata[match(df$FinalID, metadata$FinalID), 
                           as.character(metadata$TaxonomicLevelName[match(ref,
                                                                          metadata$FinalID)])]
        nestmatch <- unlist(nmatch)  %in% c(df$FinalID[i], slash)
        if(sum(nestmatch) > 2){
          "Non-Distinct"
        } else if(sum(nestmatch) == 2 && 
                    sum(df$FinalID %in% df[i, "FinalID"]) == 2 &&
                    as.numeric(df$LifeStageCode[i]) == 
                    min(as.numeric(df$LifeStageCode[df$FinalID %in% df[i, "FinalID"]]))) {
          "Distinct"
        } else if(sum(nestmatch) == 2){
          "Non-Distinct"
        } else {
          "Distinct"
        }
      })
      df
    })
    

    #Check to make sure only one is distinct when FinalID == SAFIT effort level

    x <- ddply(x, .(SampleID, FinalID), function(df){
      effortcol <- df[, paste0(effort, "_effortlevel")]
      distinctcol <- df[, paste("distinct_", effort, sep="")]
      if(all(df$TaxonomicLevelCode == effortcol) & sum(distinctcol == "Distinct") > 1) {
        df[, paste("distinct_", effort, sep="")] <- c("Distinct", rep("Non-Distinct", nrow(df)-1))
      }
      df
    })
    


  metadata$merge <- paste(metadata$FinalID, metadata$LifeStageCode)
  x$merge <- paste(x[, effort], x$LifeStageCode)
  x <- cbind(x, metadata[match(x$merge, metadata$merge), 
                         c("FunctionalFeedingGroup", "Subphylum", "Class", "Subclass", "Order",
                           "Family", "Subfamily", "Invasive", "ToleranceValue", "Habit")])
  x <- arrange(x, SampleID) #put the data frame rows in a nice order for reading
  })
  class(agglist) <- c("BMIagg") #assign a class so metric functions know what to do with it

  agglist
}


