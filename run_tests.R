library(testthat)
library(R6)
library(DBI)
library(duckdb)
library(jsonlite)

# Source all R files
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) {
  source(f)
}

# Source test helpers
source("tests/testthat/helper-fixtures.R")

# Run tests
test_dir("tests/testthat", reporter = "check")
