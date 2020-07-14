# Build slides----

# The functions below are taken from blogdown
require_rebuild <- function(pdf, rmd) {
  !file_test('-f', pdf) | file_test('-ot', pdf, rmd)
}

is_draft <- function(rmd) {
  x <- rmarkdown::yaml_front_matter(rmd)$draft
  ifelse(is.null(x), FALSE, x)
}

needs_source <- function(rmd) {
  x <- rmarkdown::yaml_front_matter(rmd)$source
  ifelse(is.null(x), FALSE, x)
}

##############################################
library(magrittr)
library(stringr)

# Find Rmd files that are not drafts
slide_sources <- list.files("content/slides",
                            full.names = TRUE,
                            pattern = "Rmd$") %>% 
  Filter(purrr::negate(is_draft), .)

# For each file, update pdf and R source if necessary
for (source in slide_sources) {
  slide_pdf <- str_replace(source, "content/slides", "static/slides") %>% 
    str_replace(".Rmd", ".pdf")
  r_source <- str_replace(source, "content/slides", "static/slides") %>% 
    str_replace(".Rmd", ".R")
  
  if (require_rebuild(slide_pdf, source)) {
    rmarkdown::render(input = source,
                      output_dir = "static/slides",
                      quiet = TRUE)
  }
  if (needs_source(source) && require_rebuild(r_source, source)) {
    knitr::purl(input = source,
                output = r_source,
                quiet = TRUE)
  }
}