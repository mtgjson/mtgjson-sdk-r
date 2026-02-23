test_that("enums$keywords returns keyword lists", {
  cache <- create_test_cache()
  eq <- EnumQuery$new(cache)

  result <- eq$keywords()
  expect_true(is.list(result))
  expect_true("abilityWords" %in% names(result))
  expect_true("keywordAbilities" %in% names(result))
  expect_true("Flying" %in% result$keywordAbilities)
})

test_that("enums$card_types returns type data", {
  cache <- create_test_cache()
  eq <- EnumQuery$new(cache)

  result <- eq$card_types()
  expect_true(is.list(result))
  expect_true("creature" %in% names(result))
  expect_true("Human" %in% result$creature$subTypes)
})

test_that("enums$enum_values returns enum data", {
  cache <- create_test_cache()
  eq <- EnumQuery$new(cache)

  result <- eq$enum_values()
  expect_true(is.list(result))
  expect_true("colors" %in% names(result))
  expect_true("R" %in% result$colors)
})
