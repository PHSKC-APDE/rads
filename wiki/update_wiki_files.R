library('knitr')
library('quarto')

#start in the rads directory
start_dir = getwd()
rmds = list.files('vignettes', '\\.Rmd$|\\.qmd$', full.names = T)
rmds = file.path(start_dir, rmds)

td = tempdir()

wiki = file.path(td, 'rads.wiki')

setwd(td)
system('git clone https://github.com/PHSKC-APDE/rads.wiki.git')
setwd(wiki)

#Knit
for(rrr in rmds){
  print(rrr)
  out = paste0(tools::file_path_sans_ext(basename(rrr)), '.md')
  if(grepl('\\.Rmd$', rrr)){knitr::knit(rrr, out)}
  if(grepl('\\.qmd$', rrr)){quarto::quarto_render(input = rrr, output_format = "md")}
  file.copy(out, file.path(start_dir, 'wiki', basename(out)))

  #clear out the headers
  rl = readLines(out)

  if(rl[1] == '---'){
    nlines = which(rl == '---')
    rl = rl[seq(max(nlines)+1, length(rl))]
    writeLines(rl, con = out)
  }

}

#push changes
system('git add .')
system(paste("git commit -m", shQuote('update wiki from automated process')))
system('git push origin master')


unlist(wiki)
