test_that("Connection execute returns data.frame", {
  conn <- create_test_connection()
  on.exit(conn$close())

  df <- conn$execute("SELECT * FROM cards WHERE name = $1", params = list("Lightning Bolt"))
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true("uuid" %in% names(df))
})

test_that("Connection execute_scalar returns single value", {
  conn <- create_test_connection()
  on.exit(conn$close())

  count <- conn$execute_scalar("SELECT COUNT(*) FROM cards")
  expect_true(is.numeric(count))
  expect_equal(count, 5)
})

test_that("Connection execute_scalar returns NULL for empty", {
  conn <- create_test_connection()
  on.exit(conn$close())

  result <- conn$execute_scalar(
    "SELECT uuid FROM cards WHERE uuid = $1", params = list("nonexistent"))
  expect_null(result)
})

test_that("Connection execute returns empty data.frame for no results", {
  conn <- create_test_connection()
  on.exit(conn$close())

  df <- conn$execute("SELECT * FROM cards WHERE uuid = $1",
                     params = list("nonexistent"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

test_that("Connection register_table_from_data works", {
  conn <- create_test_connection()
  on.exit(conn$close())

  test_df <- data.frame(id = 1:3, val = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
  conn$register_table_from_data("test_table", test_df)
  expect_true("test_table" %in% conn$registered_views)

  result <- conn$execute("SELECT * FROM test_table")
  expect_equal(nrow(result), 3)
})

test_that("Connection raw returns DBI connection", {
  conn <- create_test_connection()
  on.exit(conn$close())

  raw <- conn$raw()
  expect_true(inherits(raw, "duckdb_connection"))
})

test_that("Connection ensure_views is idempotent", {
  conn <- create_test_connection()
  on.exit(conn$close())

  n_before <- length(conn$registered_views)
  conn$ensure_views("cards")
  n_after <- length(conn$registered_views)
  # Should not add duplicate
  expect_equal(n_before, n_after)
})
