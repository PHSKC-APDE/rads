# Raw CSV (updated by hand when needed) lives in inst/extdata/ so it is
# installed with the package. This script converts it to an *.rda file for
# easy importation in R.

ref_get_xwalk <- data.table::fread("inst/extdata/ref_get_xwalk.csv")

save(list = "ref_get_xwalk", file = "data/ref_get_xwalk.rda" , compress = "bzip2", version = 3)

usethis::use_data(ref_get_xwalk, overwrite = TRUE)
