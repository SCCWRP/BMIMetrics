BMItaxaselect <- function(data, metrics, level=2){
  loadMetaData()
  
  metricfunction <- function(name, level){
    effort <- paste0("distinct_SAFIT", level)
    namesplit <- strsplit(name, "_")[[1]]
    taxon <- namesplit[1]
    metric.type <- namesplit[2]
    
    category <- as.character(metadata$TaxonomicLevelName[metadata$FinalID==taxon])[1]
    
    exp <- switch(metric.type,
                  "PercentTaxa" = quote(sum(distinct & criterion)/sum(distinct)),
                  "Percent" = quote(sum(BAResult[criterion])/sum(BAResult)),
                  "Taxa" = quote(sum(distinct & criterion))
                  )
    function(df){
      distinct <- df[, effort] == "Distinct"
      criterion <- df[, category] %in% taxon
      
      eval(exp, df)
      
    }
  }
  
  results <- ddply(data[[level]], "SampleID", function(df){
    vapply(metrics, function(name){
      metric <- metricfunction(name, level=level)
      metric(df)
    }, numeric(1))
  })
  names(results) <- c("SampleID", metrics)
  results
}