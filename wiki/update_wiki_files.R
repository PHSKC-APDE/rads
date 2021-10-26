library('knitr')

rmds = list.files('vignettes', '\\.Rmd$', full.names = T)


for(rrr in rmds){
  print(rrr)

  knitr::knit(rrr, file.path('wiki', paste0(tools::file_path_sans_ext(basename(rrr)), '.md')))

}
