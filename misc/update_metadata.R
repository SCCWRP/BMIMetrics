library(tidyverse)

load(file = 'inst/metadata.rdata')

toadd <- metadata[metadata$FinalID %in% 'Peltodytes simplex', ] %>% 
  mutate(LifeStageCode = 'L')

metadata <- metadata %>% 
  bind_rows(toadd) %>% 
  arrange(FinalID)

save(metadata, file = 'inst/metadata.RData')
