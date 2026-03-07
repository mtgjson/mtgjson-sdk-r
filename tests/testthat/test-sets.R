skip_on_cran()

test_that("sets$get returns set by code", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  result <- sq$get("MH3")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Modern Horizons 3")
})

test_that("sets$get is case-insensitive", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  result <- sq$get("mh3")
  expect_equal(nrow(result), 1)
})

test_that("sets$get returns NULL for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  expect_null(sq$get("NONEXISTENT"))
})

test_that("sets$list returns all sets", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  result <- sq$list()
  expect_equal(nrow(result), 4)
})

test_that("sets$list filters by set_type", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  result <- sq$list(set_type = "expansion")
  expect_true(nrow(result) > 0)
  expect_true(all(result$type == "expansion"))
})

test_that("sets$search by name", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  result <- sq$search(name = "Horizons")
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("Horizons", result$name)))
})

test_that("sets$count works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  sq <- SetQuery$new(conn)

  expect_equal(sq$count(), 4L)
})
