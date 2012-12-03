BMIselect <- function(data, metrics, level=2){
  
  metricfunction <- function(name, level){
    effort <- paste("distinct_SAFIT", level, sep="")
    namesplit <- strsplit(name, "_")[[1]]
    taxaname <- namesplit[1]
    metriccat <- namesplit[2]
    ifelse(any(taxaname %in% metadata$Order), category <- "Order",
           ifelse(any(taxaname %in% metadata$FunctionalFeedingGroup), category <- "FunctionalFeedingGroup",
                  ifelse(any(taxaname %in% metadata$Habit), category <- "Habit",
                         ifelse(taxaname %in% c("Tanypodinae", "Orthocladiinae", "Chironominae"), category <- "Subfamily",
                                ifelse(taxaname == "Chironomidae", category <- "Family",
                                       ifelse(taxaname %in% c("Acari", "Oligochaeta"), category <- "Subclass",
                                              ifelse(taxaname =="Crustacea", category <- "Subphylum",
                                              )))))))
    
    exp <- ifelse(metriccat == "PercentTaxa", expression(nrow(df[distinct & criterion, ])/nrow(df[distinct, ])),
                  ifelse(metriccat == "Percent", expression(sum(df$BAResult[criterion])/sum(df$BAResult)),
                         nrow(df[distinct & criterion, ])))
      
    
    function(df){
      distinct <- df[, effort] == "Distinct"
      criterion <- df[, category] %in% taxaname
      
      eval(exp)
      
    }
  }
  
  
  loadMetaData()
  results <- ddply(data[[level]], "SampleID", function(df){
    vapply(metrics, function(name){
      metric <- metricfunction(name, level=level)
      metric(df)
    }, numeric(1))
  })
  names(results) <- c("SampleID", metrics)
  results
}
