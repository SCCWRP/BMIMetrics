library(readr)
library(dplyr)

load("~/BMIMetrics/inst/metadata.rdata")
CSCI_bugs <- read_csv("CSCI_bugs.csv")

# Here I am just looking at the FinalID and LifeStageCode
meta_test <- metadata[,c(1,2)]
bug_test <- CSCI_bugs[,c(3,5)]

# This gives 19 entries of the code that ARE NOT in the meta data
diffs_test <- anti_join(bug_test, meta_test, by = c("FinalID", "LifeStageCode"))

# If this helps, in their data, their LifeStageCodes are not the same as meta data
uniq_meta <- unique(meta_test$LifeStageCode)
uniq_bug <- unique(bug_test$LifeStageCode)