test_that("identifiers$find_by_scryfall_id works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  result <- iq$find_by_scryfall_id("scry-bolt-a25")
  expect_true(nrow(result) > 0)
  expect_equal(result$name[1], "Lightning Bolt")
})

test_that("identifiers$find_by_tcgplayer_id works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  result <- iq$find_by_tcgplayer_id("111")
  expect_true(nrow(result) > 0)
})

test_that("identifiers$find_by unknown type errors", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  expect_error(iq$find_by("bogusColumn", "123"), "Unknown identifier type")
})

test_that("identifiers$find_by known type works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  result <- iq$find_by("scryfallId", "scry-bolt-a25")
  expect_true(nrow(result) > 0)
})

test_that("identifiers$get_identifiers returns named list", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  result <- iq$get_identifiers("uuid-bolt-1")
  expect_true(is.list(result))
  expect_equal(result$scryfallId, "scry-bolt-a25")
})

test_that("identifiers$get_identifiers returns NULL for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  iq <- IdentifierQuery$new(conn)

  expect_null(iq$get_identifiers("nonexistent"))
})
