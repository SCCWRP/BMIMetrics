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
  x <- x[, names(x) %in% c("StationCode", "SampleDate", "Replicate", "SampleID", "FinalID", "LifeStageCode", "BAResult", "DistinctCode")]
  x$DistinctCode[is.na(x$DistinctCode)] <- 0
  
  ###Merge in metadata FinalID###
  x <- merge(x, metadata[,c("FinalID", "LifeStageCode", "SAFIT1", "SAFIT2", "TaxonomicLevelCode",
                             "SAFIT1_effortlevel", "SAFIT2_effortlevel")], by=c("FinalID", "LifeStageCode"), type="left")
  x <- na.omit(x) 

  
  for(effort in c("SAFIT1", "SAFIT2")){
    
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

    x <- merge(x, x.aggup, all=T)

    ###Taxonomist Overide###
    x[is.na(x[, paste("distinct_", effort, sep="")]) & x$DistinctCode == 1, paste("distinct_", effort, sep="")] <- "Distinct"

    ###Distinctiveness for those already above SAFIT aggregation (Check for nested taxa)
    ###Technically, this step could handle everything the first step did, but it's much slower due
    ###to having to reference the metadata repeatedly
    x <- ddply(x, "SampleID", function(df){
      nestcheck <- is.na(df[, paste("distinct_", effort, sep="")])
      if(sum(nestcheck)==0)df else{
        df[which(nestcheck), paste("distinct_", effort, sep="")] <- sapply(which(nestcheck), function(i){
          nestmatch <- metadata[match(df$FinalID, metadata$FinalID), 
                                as.character(metadata$TaxonomicLevelName[match(df$FinalID[i], metadata$FinalID)])] %in% df$FinalID[i]
          if(sum(nestmatch) > 2){
            "Non-Distinct"} else 
              if(sum(nestmatch) == 1) "Distinct" else
                if((df$FinalID[i] == df$FinalID[setdiff(which(nestmatch), i)]) & 
                   (df$LifeStageCode[i] < df$LifeStageCode[setdiff(which(nestmatch), i)])){
               "Non-Distinct"
              } else
            {"Distinct"}
        })
        df
      }
      
    })
  } 
  x <- arrange(x, SampleID, SAFIT1) #put the data frame rows in a nice order for reading
  class(x) <- c("BMIagg", "BMI", "data.frame") #assign a class so metric functions know what to do with it
  x
}


