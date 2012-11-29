#' Aggregate FinalID
#' 
#' Aggregates to  taxonomic effort levels SAFIT1 and SAFIT2
#' 
#' @param x An object of class BMI
#' @export 
#' @import plyr
#' @include loadMetaData.r


aggregate.BMI <- function(x){
  ###Load in metadata; this function won't work unless package is installed,
  ###but you can manually read in metadata.rdata, and then comment out this line
  metadata <- loadMetaData()
  
  ###Clean up###
  x <- x[, names(x) %in% c("StationCode", "SampleDate", "Replicate", "SampleID", "FinalID",
                           "LifeStageCode", "BAResult", "originalBAResult", "DistinctCode")]
  x$DistinctCode[is.na(x$DistinctCode)] <- 0
  
  ###Merge in metadata FinalID###
  x <- merge(x, metadata[,c("FinalID", "LifeStageCode", "SAFIT1", "SAFIT2", "TaxonomicLevelCode",
                             "SAFIT1_effortlevel", "SAFIT2_effortlevel")], by=c("FinalID", "LifeStageCode"), type="left")
  x <- na.omit(x) 


  agglist <- lapply(c("SAFIT1", "SAFIT2"), function(effort){
    
    ###Distinctiveness for those aggregated up, i.e. 
    ###FinalIDs that are higher resolution than SAFIT1/2
    x.aggup <- x[x$TaxonomicLevelCode > x[, paste(effort, "_effortlevel", sep="")], ]
    x.aggup[, paste("distinct_", effort, sep="")] <- rep(NA, nrow(x.aggup))
    x.aggup <- ddply(x.aggup, "SampleID", function(fdf){
      ddply(fdf, effort, function(df){
        if(nrow(df)==1)df[, paste("distinct_", effort, sep="")] <- "Distinct" else{
          df <- df[order(df$LifeStageCode), ]
          selected <- which(df$TaxonomicLevelCode == max(df$TaxonomicLevelCode))
          df[selected[1], paste("distinct_", effort, sep="")] <- "Distinct"
          df[!(1:nrow(df) %in% selected[1]), paste("distinct_", effort, sep="")] <- "Non-Distinct"
        }
        df
      })
    })

    x <- join(x, x.aggup, type="full", match="first")

    ###Taxonomist Overide###
    x[is.na(x[, paste("distinct_", effort, sep="")]) & x$DistinctCode > 0, paste("distinct_", effort, sep="")] <- "Distinct"

    ###Distinctiveness for those already above SAFIT aggregation (Check for nested taxa)
    ###Technically, this step could handle everything the first step did, but it's much slower due
    ###to having to reference the metadata repeatedly
    x$LifeStageCode <- as.factor(x$LifeStageCode)
    
    x <- ddply(x, "SampleID", function(df){
      nestcheck <- is.na(df[, paste("distinct_", effort, sep="")])
      if(sum(nestcheck)==0)df else{
        df[which(nestcheck), paste("distinct_", effort, sep="")] <- sapply(which(nestcheck), function(i){
          nestmatch <- metadata[match(df$FinalID, metadata$FinalID), 
                                as.character(metadata$TaxonomicLevelName[match(df$FinalID[i], metadata$FinalID)])] %in% df$FinalID[i]
          if(sum(nestmatch) > 2){
            "Non-Distinct"} else 
              if(sum(nestmatch) == 2 && sum(df$FinalID %in% df[i, "FinalID"]) == 2 &&
                   as.numeric(df$LifeStageCode[i]) == 
                   min(as.numeric(df$LifeStageCode[df$FinalID %in% df[i, "FinalID"]]))){"Distinct"} else
                     if(sum(nestmatch) == 2){"Non-Distinct"} else
                     {"Distinct"}
        })
        df
      }
      
    })
   

  metadata$merge <- paste(metadata$FinalID, metadata$LifeStageCode)
  x$merge <- paste(x$SAFIT2, x$LifeStageCode)
#   x <- join(x, unique(metadata[, c("merge", "FunctionalFeedingGroup", "Subphylum", "Class", "Subclass", "Order",
#                              "Family", "Subfamily", "Invasive", "ToleranceValue", "Habit")]),
#              by="merge", type="inner")
  x <- cbind(x, metadata[match(x$merge, metadata$merge), 
                         c("FunctionalFeedingGroup", "Subphylum", "Class", "Subclass", "Order",
                           "Family", "Subfamily", "Invasive", "ToleranceValue", "Habit")])
  x <- arrange(x, SampleID, SAFIT1) #put the data frame rows in a nice order for reading
  })
  class(agglist) <- c("BMIagg") #assign a class so metric functions know what to do with it

  agglist
}


