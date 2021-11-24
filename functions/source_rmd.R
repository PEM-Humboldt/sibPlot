source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  envir <- globalenv()
  source(tempR, local = envir, ...)
}