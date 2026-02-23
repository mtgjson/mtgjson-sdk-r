test_that("decks$list returns all decks", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  result <- dq$list()
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("decks$list filters by set_code", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  result <- dq$list(set_code = "MH3")
  expect_equal(nrow(result), 2)
})

test_that("decks$list filters by deck_type", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  result <- dq$list(deck_type = "Commander Deck")
  expect_equal(nrow(result), 2)
})

test_that("decks$search by name", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  result <- dq$search(name = "Eldrazi")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Eldrazi Incursion")
})

test_that("decks$search is case-insensitive", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  result <- dq$search(name = "eldrazi")
  expect_equal(nrow(result), 1)
})

test_that("decks$count returns total", {
  cache <- create_test_cache()
  dq <- DeckQuery$new(cache)

  expect_equal(dq$count(), 3)
})
