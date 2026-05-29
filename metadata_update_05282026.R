library(readxl)

load("~/BMIMetrics/inst/metadata.rdata")
added <- read_xlsx("data/updated metadata 05282026.xlsx")

added <- added[,1:42]

updated_metadata <- rbind(metadata,added)

save(updated_metadata, file = "inst/metadata.RData")