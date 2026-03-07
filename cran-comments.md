## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.2

## Notes

This is a new submission. The package provides an R client for querying
MTGJSON Magic: The Gathering card data via DuckDB-backed Parquet files.

Tests that depend on DuckDB are skipped on CRAN to avoid heavy
compilation requirements in check environments.
