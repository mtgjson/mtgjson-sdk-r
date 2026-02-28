# Install dependencies for the Format Metagame Analyst dashboard.
#
# Usage:
#   Rscript install.R
#
# The mtgjsonsdk package must be installed separately:
#   devtools::install_local("path/to/mtgjson-sdk-r")

install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "DT",
  "scales"
), repos = "https://cloud.r-project.org")
