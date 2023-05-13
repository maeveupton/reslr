## ---- eval = TRUE-------------------------------------------------------------
# Not on CRAN yet
#install.packages("reslr")
devtools::install_github("maeveupton/reslr")

## ---- message=FALSE-----------------------------------------------------------
library(reslr)

## ---- eval = TRUE-------------------------------------------------------------
library(readr)
path_to_data <- system.file("extdata", "one_data_site_ex.csv", package = "reslr")
example_one_datasite <- read.csv(path_to_data)

