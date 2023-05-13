## ---- include = FALSE---------------------------------------------------------
library(tidyverse)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("simmr")
#  library(simmr)

## ----exampledataset2 ,eval = TRUE---------------------------------------------
# For 1 site
CedarIslandNC <- reslr::NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

