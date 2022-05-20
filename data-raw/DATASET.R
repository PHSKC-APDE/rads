## code to prepare `DATASET` dataset goes here
metadata <- data.table::fread("./data-raw/metadata.csv")

usethis::use_data(metadata, internal = TRUE, overwrite = TRUE)
