skip_on_cran()

test_that("cards$get_by_uuid returns one-row df", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$get_by_uuid("uuid-bolt-1")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Lightning Bolt")
})

test_that("cards$get_by_uuid returns NULL for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  expect_null(cq$get_by_uuid("nonexistent"))
})

test_that("cards$get_by_uuids returns multiple", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$get_by_uuids(c("uuid-bolt-1", "uuid-counterspell"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("cards$get_by_uuids empty input returns empty", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$get_by_uuids(character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("cards$get_by_name returns all printings", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$get_by_name("Lightning Bolt")
  expect_true(nrow(result) >= 2)
})

test_that("cards$get_by_name with set_code filters", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$get_by_name("Lightning Bolt", set_code = "A25")
  expect_equal(nrow(result), 1)
  expect_equal(result$setCode[1], "A25")
})

test_that("cards$search by name pattern", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$search(name = "Lightning%")
  expect_true(nrow(result) > 0)
  expect_true(all(grepl("^Lightning", result$name)))
})

test_that("cards$search by rarity", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$search(rarity = "rare")
  expect_true(nrow(result) > 0)
  expect_true(all(result$rarity == "rare"))
})

test_that("cards$search by mana_value range", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$search(mana_value_lte = 1)
  expect_true(nrow(result) > 0)
  expect_true(all(result$manaValue <= 1))
})

test_that("cards$search with limit and offset", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  r1 <- cq$search(limit = 2L, offset = 0L)
  r2 <- cq$search(limit = 2L, offset = 2L)
  expect_equal(nrow(r1), 2)
  expect_true(nrow(r2) > 0)
  # Different results
  expect_false(identical(r1$uuid, r2$uuid))
})

test_that("cards$get_printings is alias for get_by_name", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  r1 <- cq$get_by_name("Lightning Bolt")
  r2 <- cq$get_printings("Lightning Bolt")
  expect_equal(nrow(r1), nrow(r2))
})

test_that("cards$random returns requested count", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$random(2L)
  expect_equal(nrow(result), 2)
})

test_that("cards$count returns total", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  expect_equal(cq$count(), 5L)
})

test_that("cards$count with filters", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  expect_equal(cq$count(setCode = "A25"), 2L)
  expect_equal(cq$count(rarity = "rare"), 2L)
})

test_that("cards$search by is_promo", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$search(is_promo = TRUE)
  expect_true(nrow(result) > 0)
  expect_true(all(result$isPromo == TRUE))
})

test_that("cards$search by text", {
  conn <- create_test_connection()
  on.exit(conn$close())
  cq <- CardQuery$new(conn)

  result <- cq$search(text = "damage")
  expect_true(nrow(result) > 0)
  expect_true(all(grepl("damage", result$text, ignore.case = TRUE)))
})
