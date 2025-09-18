#' Build documentation and website for the R package
#' import

libs = list("devtools", "pkgdown")
# Install missing packages
installed_libs = rownames(installed.packages())
for (lib in libs) {
  if (!lib %in% installed_libs) {
    install.packages(lib)
  }
}

library(devtools)
library(pkgdown)
devtools::document()
pkgdown::build_site()
