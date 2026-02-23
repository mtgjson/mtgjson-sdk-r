test_that("MtgjsonSDK creates with offline cache", {
  cache <- create_test_cache()
  # Build SDK with the test cache dir
  sdk <- MtgjsonSDK$new(cache_dir = cache$cache_dir, offline = TRUE)
  on.exit(sdk$close())

  # meta should work (Meta.json exists in test cache)
  meta <- sdk$meta
  expect_true(is.list(meta))
  expect_equal(meta$data$version, "5.2.2+20240101")
})

test_that("MtgjsonSDK active bindings are lazy", {
  cache <- create_test_cache()
  sdk <- MtgjsonSDK$new(cache_dir = cache$cache_dir, offline = TRUE)
  on.exit(sdk$close())

  # Accessing enums should work (JSON-based, no parquet needed)
  kw <- sdk$enums$keywords()
  expect_true(is.list(kw))
  expect_true("Flying" %in% kw$keywordAbilities)
})

test_that("MtgjsonSDK$views starts empty", {
  cache <- create_test_cache()
  sdk <- MtgjsonSDK$new(cache_dir = cache$cache_dir, offline = TRUE)
  on.exit(sdk$close())

  expect_equal(length(sdk$views), 0)
})

test_that("MtgjsonSDK$close does not error", {
  cache <- create_test_cache()
  sdk <- MtgjsonSDK$new(cache_dir = cache$cache_dir, offline = TRUE)
  expect_no_error(sdk$close())
})
