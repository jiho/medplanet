#
# Clean and recompile everything
#

## Cleanup cache and html ----

cached_dirs <- list.files(".", pattern = "(_cache)|(_files)")
unlink(cached_dirs, recursive = TRUE)

html_output <- list.files(".", pattern = glob2rx("*.html"))
file.remove(html_output)

file.remove("data.rda")


## Regenerate everything ----

source("prepare_data.R")

rmd_files <- list.files(".", pattern = "[1-9].*?\\.Rmd")
for(file in rmd_files) {
  rmarkdown::render(file, envir=new.env())
}
