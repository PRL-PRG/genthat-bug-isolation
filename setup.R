options(repos="http://cran.us.r-project.org")

load_library <- function(pkg) {
  if (!require(pkg, quietly=TRUE, character.only=TRUE)) {
    install.packages(pkg)
    library(pkg, character.only=TRUE)
  }
}

load_library("devtools")
load_library("testthat")
load_library("roxygen2")
load_library("optparse")
