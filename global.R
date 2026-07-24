# global.R
# Application initialization, package imports, and sourcing of modules.

# Load required packages
library(shiny)
library(bslib)
library(haven)
library(dtsmartr)
library(shinycssloaders)
library(purrr)
library(fs)
library(dplyr)
library(tidyr)
library(ggplot2)

# Source all utility and module R files
r_files <- dir_ls("R", glob = "*.R")
for (file in r_files) {
  source(file, local = TRUE)
}

cat("ClinDataExplorer: Sourced modules and utilities successfully!\n")
