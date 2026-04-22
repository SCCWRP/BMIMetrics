# Updating metadata - April 22, 2026
library(readxl)

data <- read_xlsx("New CSCI metadata_032726.xlsx")

save(data, file = 'inst/metadata_1.2.3.RData')

install.packages('devtools')
library(devtools)
install_github('SCCWRP/BMIMetrics') # <--- This is a dependency that will not automatically install when CSCI is installed
install_github('SCCWRP/CSCI')
library(CSCI)