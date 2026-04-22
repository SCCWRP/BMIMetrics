# Updating metadata - April 22, 2026
library(readxl)

data <- read_xlsx("New CSCI metadata_032726.xlsx")

save(data, file = 'inst/metadata_1.2.3.RData')