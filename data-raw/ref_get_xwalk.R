# save CSV (which will be updated by hand when needed) to an *.rda file for easy
# importation in R

ref_get_xwalk <- data.table::fread("data-raw/ref_get_xwalk.csv")

save(list = "ref_get_xwalk", file = "data/ref_get_xwalk.rda" , compress = "bzip2", version = 3)

usethis::use_data(ref_get_xwalk, overwrite = TRUE)
