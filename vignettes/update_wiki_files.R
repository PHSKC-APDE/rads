library('knitr')
library('quarto')

# Start in rads directory
start_dir <- getwd()

# Find all Rmd and Qmd files
rmds <- normalizePath(list.files('vignettes', pattern = '\\.rmd$|\\.qmd$', full.names = TRUE, ignore.case = TRUE))

# Set up temporary directory and clone wiki repo
td <- tempdir()
setwd(td)
wiki_dir <- file.path(td, 'rads.wiki')
system('git clone https://github.com/PHSKC-APDE/rads.wiki.git')
setwd(wiki_dir)

# Knit / Render each file
for(rmd_file in rmds) {
  cat("Processing:", rmd_file, "\n")

  # Determine output filename
  out_file <- paste0(tools::file_path_sans_ext(basename(rmd_file)), '.md')

  # Render based on file type
  if(grepl('\\.rmd$', rmd_file, ignore.case = TRUE)) {
    knitr::knit(
      rmd_file,
      output = out_file
      )
  } else if(grepl('\\.qmd$', rmd_file, ignore.case = TRUE)) {
    quarto::quarto_render(
      input = rmd_file,
      output_format = "gfm",
      output_file = out_file
    )
  }

  # Clear out YAML headers
  if(file.exists(out_file)) {
    rl <- readLines(out_file)
    if(length(rl) > 0 && rl[1] == '---') {
      end_yaml <- which(rl == '---')[2]
      if(!is.na(end_yaml)) {
        rl <- rl[(end_yaml+1):length(rl)]
        writeLines(rl, con = out_file)
      }
    }
  } else {
    warning(paste("Output file not created:", out_file))
  }
}

# Push changes to https://github.com/PHSKC-APDE/rads.wiki.git
system('git add .')
system('git commit -m "Update wiki from automated process"')
system('git push origin master')  # Using master as specified for your wiki


# Clean up
setwd(start_dir)
gc()
unlink(wiki_dir, recursive = TRUE, force = TRUE)
rm(wiki_dir)
