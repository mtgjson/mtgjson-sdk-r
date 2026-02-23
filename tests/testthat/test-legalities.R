test_that("legalities$formats_for_card works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  result <- lq$formats_for_card("uuid-bolt-1")
  expect_true(is.character(result))
  expect_true("modern" %in% names(result))
  expect_equal(result[["modern"]], "Legal")
})

test_that("legalities$formats_for_card returns empty for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  result <- lq$formats_for_card("nonexistent")
  expect_equal(length(result), 0)
})

test_that("legalities$is_legal returns TRUE for legal card", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  expect_true(lq$is_legal("uuid-bolt-1", "modern"))
})

test_that("legalities$is_legal returns FALSE for not-legal card", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  expect_false(lq$is_legal("uuid-bolt-1", "standard"))
})

test_that("legalities$legal_in returns legal cards", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  result <- lq$legal_in("modern")
  expect_true(nrow(result) > 0)
})

test_that("legalities$banned_in works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  lq <- LegalityQuery$new(conn)

  result <- lq$banned_in("legacy")
  expect_true(nrow(result) > 0)
  expect_true("Goblin Guide" %in% result$name)
})
