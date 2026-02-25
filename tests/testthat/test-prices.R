test_that("prices$today returns latest prices", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$today("uuid-bolt-1")
  expect_true(nrow(result) > 0)
  expect_true("price" %in% names(result))
})

test_that("prices$today with provider filter", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$today("uuid-bolt-1", provider = "tcgplayer")
  expect_true(nrow(result) > 0)
})

test_that("prices$history returns ordered results", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$history("uuid-bolt-1")
  expect_true(nrow(result) >= 2)
})

test_that("prices$price_trend returns stats", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$price_trend("uuid-bolt-1")
  expect_true(is.list(result))
  expect_true("min_price" %in% names(result))
  expect_true("max_price" %in% names(result))
  expect_true("avg_price" %in% names(result))
})

test_that("prices$price_trend returns NULL for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  expect_null(pq$price_trend("nonexistent"))
})

test_that("prices$cheapest_printing works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$cheapest_printing("Lightning Bolt")
  expect_true(is.list(result))
  expect_true("price" %in% names(result))
  # M11 at 1.75 should be cheapest
  expect_equal(result$uuid, "uuid-bolt-2")
})

test_that("prices$cheapest_printing returns NULL for unknown card", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  expect_null(pq$cheapest_printing("Nonexistent Card"))
})

test_that("prices$get returns nested structure", {
  conn <- create_test_connection()
  on.exit(conn$close())
  pq <- PriceQuery$new(conn)

  result <- pq$get("uuid-bolt-1")
  expect_true(is.list(result))
  expect_true("paper" %in% names(result))
})
