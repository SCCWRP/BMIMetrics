#' Aggregate FinalID
#' 
#' Aggregates to  taxonomic effort levels SAFIT1 and SAFIT2
#' 
#' @param x An object of class BMI
#' @export 
#' @import plyr
#' @include loadMetaData.r


aggregate.BMI <- function(x){
  
  #metadata <- loadMetaData()
  x <- x[, c("StationCode", "SampleDate", "Replicate", "SampleID", "FinalID", "LifeStageCode", "BAResult", "DistinctCode")]
  x$DistinctCode[is.na(x$DistinctCode)] <- 1
  ###Merge in metadata FinalID###
  x <- merge(x, metadata[,c("FinalID", "LifeStageCode", "SAFIT1", "SAFIT2", "TaxonomicLevelCode",
                             "SAFIT1_effortlevel", "SAFIT2_effortlevel")], by=c("FinalID", "LifeStageCode"), type="left")
  x <- na.omit(x)

  
  for(effort in c("SAFIT1", "SAFIT2")){
    ###Distinctiveness for those aggregated up###
    x.aggup <- x[x$TaxonomicLevelCode > x[, paste(effort, "_effortlevel", sep="")], ]
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
    x[is.na(x[, paste("distinct_", effort, sep="")]) & x$DistinctCode == 0, paste("distinct_", effort, sep="")] <- "Distinct"
    
    ###Distinctiveness for those already above SAFIT aggregation (Check for nested taxa)###
    x <- ddply(x, "SampleID", function(df){
      nestcheck <- is.na(df[, paste("distinct_", effort, sep="")])
      if(sum(nestcheck)==0)df else{
        df[which(nestcheck), paste("distinct_", effort, sep="")] <- sapply(which(nestcheck), function(i){
          if(sum(metadata[match(df$FinalID, metadata$FinalID), 
                      as.character(metadata$TaxonomicLevelName[match(df$FinalID[i], metadata$FinalID)])] %in% df$FinalID[i]) >1){
            "Non-Distinct"} else 
            {"Distinct"}
        })
        df
      }
      
    })
  } 
  x <- arrange(x, SampleID, SAFIT1)
  class(x) <- c("BMIagg", "BMI", "data.frame")
  x
}


