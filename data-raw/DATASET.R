## code to prepare `DATASET` dataset goes here
metadata <- read.csv("./data-raw/metadata.csv")

usethis::use_data(metadata, internal = TRUE, overwrite = TRUE)
