library(devtools)
library(roxygen2)
library(ermeeth)
library(tidyverse)

usethis::use_build_ignore("dev_history.r")
usethis::use_build_ignore("tests")
usethis::use_vignette("Functions_presentation")
usethis::use_gpl3_license()
###to build and update
#### Check if ermeeth is installed

usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("purrr")
usethis::use_package("stringr")
usethis::use_package("sys")
usethis::use_package("tidyr")
usethis::use_package("scales")
usethis::use_package("ggh4x")
usethis::use_package("data.table")
usethis::use_package("ofce")
usethis::use_package("utils")
usethis::use_package("colorspace")
usethis::use_package("flextable")
usethis::use_package("officer")
usethis::use_package("zip")
usethis::use_package("readxl")
usethis::use_package("crayon")

# devtools::build(path = "../ThreeMe/ThreeME_V3/R_ThreeME/src/")
devtools::build(path = "../ThreeME_V3/R_ThreeME/src/")
devtools::build_vignettes()
## Retrouver les fichiers du inst
system.file("bridge_c28_s32.R",package = "ermeeth")
system.file("models",package = "tresthor")

CheckLazyDataCompression <- function(pkg){
  pkg_name <- sub("_.*", "", pkg)
  lib <- tempfile(); dir.create(lib)
  zs <- c("gzip", "bzip2", "xz")
  res <- integer(3); names(res) <- zs
  for (z in zs) {
    opts <- c(paste0("--data-compress=", z),
              "--no-libs", "--no-help", "--no-demo", "--no-exec", "--no-test-load")
    install.packages(pkg, lib, INSTALL_opts = opts, repos = NULL, quiet = TRUE)
    res[z] <- file.size(file.path(lib, pkg_name, "data", "Rdata.rdb"))
  }
  ceiling(res/1024)
}
CheckLazyDataCompression("ermeeth")


## Creer la doc du package
rm(list= ls())
roxygen2::roxygenise()
