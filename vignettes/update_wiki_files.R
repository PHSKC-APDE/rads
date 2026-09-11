library('quarto')

# Start in rads directory
start_dir <- getwd()

# Find all Qmd files
qmds <- normalizePath(list.files('vignettes', pattern = '\\.qmd$', full.names = TRUE, ignore.case = TRUE))

# Set up temporary directory and clone wiki repo
td <- tempdir()
setwd(td)
wiki_dir <- file.path(td, 'rads.wiki')
system('git clone https://github.com/PHSKC-APDE/rads.wiki.git')
setwd(wiki_dir)

# Render each file
for(qmd_file in qmds) {
  cat("Processing:", qmd_file, "\n")

  # Render gihub flavored markdown ----
  out_file <- paste0(tools::file_path_sans_ext(basename(qmd_file)), '.md')

  quarto::quarto_render(
    input = qmd_file,
    output_format = "gfm",
    output_file = out_file
  )

  # Move the rendered file from vignettes/ into wiki repo ----
  rendered_path <- file.path(dirname(qmd_file), out_file)

  wiki_out_file <- file.path(wiki_dir, out_file)

  file.copy(rendered_path, wiki_out_file, overwrite = TRUE)
  file.remove(rendered_path)

  if (!file.exists(wiki_out_file)) {
    warning(paste0("\u26a0\ufe0f Failed to move '", out_file, "' into wiki repo:", wiki_out_file))
  }

  # Move any associated support directory (e.g. .png, .jpg, etc.) into wiki repo ----
  support_dir_name <- paste0(tools::file_path_sans_ext(basename(qmd_file)), '_files')
  rendered_support_dir <- file.path(dirname(qmd_file), support_dir_name)
  wiki_support_dir <- file.path(wiki_dir, support_dir_name)

  if (dir.exists(rendered_support_dir)) {
    file.copy(rendered_support_dir, wiki_dir, recursive = TRUE, overwrite = TRUE)
    unlink(rendered_support_dir, recursive = TRUE, force = TRUE)

    if (!dir.exists(wiki_support_dir)) {
      warning(paste0("\u26a0\ufe0f Failed to move support directory '", support_dir_name, "' into wiki repo"))
    }
  }
}

# Push changes to https://github.com/PHSKC-APDE/rads.wiki.git
system('git add .')
system('git commit -m "Update wiki from automated process"')
system('git push origin master')


# Clean up
setwd(start_dir)
gc()
unlink(wiki_dir, recursive = TRUE, force = TRUE)
rm(wiki_dir)
