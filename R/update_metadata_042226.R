# Updating metadata - April 22, 2026
library(readxl)

data <- read_xlsx("New CSCI metadata_032726.xlsx")

save(data, file = 'inst/metadata_1.2.3.RData')

# install.packages('devtools')
library(devtools)
# install_github('SCCWRP/BMIMetrics') # <--- This is a dependency that will not automatically install when CSCI is installed
# install_github('SCCWRP/CSCI')
library(CSCI)

#A list of two data frames: bugs and stations
data(bugs_stations) 

# run the estimator
results <- CSCI(bugs = bugs_stations[[1]], stations = bugs_stations[[2]])

# see all the components of the report
ls(results)