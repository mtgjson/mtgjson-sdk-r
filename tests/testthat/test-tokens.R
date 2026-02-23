test_that("tokens$get_by_uuid works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  result <- tq$get_by_uuid("uuid-soldier")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Soldier")
})

test_that("tokens$get_by_uuid returns NULL for missing", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  expect_null(tq$get_by_uuid("nonexistent"))
})

test_that("tokens$get_by_name works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  result <- tq$get_by_name("Soldier")
  expect_true(nrow(result) > 0)
})

test_that("tokens$search by set_code", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  result <- tq$search(set_code = "MH3")
  expect_true(nrow(result) > 0)
  expect_true(all(result$setCode == "MH3"))
})

test_that("tokens$for_set works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  result <- tq$for_set("MH3")
  expect_true(nrow(result) > 0)
})

test_that("tokens$count works", {
  conn <- create_test_connection()
  on.exit(conn$close())
  tq <- TokenQuery$new(conn)

  expect_equal(tq$count(), 2L)
})
