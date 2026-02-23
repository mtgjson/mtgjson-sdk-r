test_that("SqlBuilder basic select", {
  b <- SqlBuilder$new("cards")$build()
  expect_equal(b$sql, "SELECT *\nFROM cards")
  expect_equal(b$params, list())
})

test_that("SqlBuilder select columns", {
  b <- SqlBuilder$new("cards")$select("name", "uuid")$build()
  expect_match(b$sql, "SELECT name, uuid")
})

test_that("SqlBuilder where_eq", {
  b <- SqlBuilder$new("cards")$where_eq("name", "Bolt")$build()
  expect_match(b$sql, "name = \\$1")
  expect_equal(b$params, list("Bolt"))
})

test_that("SqlBuilder where_like", {
  b <- SqlBuilder$new("cards")$where_like("name", "Lightning%")$build()
  expect_match(b$sql, "LOWER\\(name\\) LIKE LOWER\\(\\$1\\)")
  expect_equal(b$params, list("Lightning%"))
})

test_that("SqlBuilder where_in", {
  b <- SqlBuilder$new("cards")$where_in("uuid", list("a", "b", "c"))$build()
  expect_match(b$sql, "uuid IN \\(\\$1, \\$2, \\$3\\)")
  expect_equal(b$params, list("a", "b", "c"))
})

test_that("SqlBuilder where_in empty produces FALSE", {
  b <- SqlBuilder$new("cards")$where_in("uuid", list())$build()
  expect_match(b$sql, "FALSE")
})

test_that("SqlBuilder where_gte and where_lte", {
  b <- SqlBuilder$new("cards")$where_gte("manaValue", 2)$where_lte("manaValue", 5)$build()
  expect_match(b$sql, "manaValue >= \\$1")
  expect_match(b$sql, "manaValue <= \\$2")
  expect_equal(b$params, list(2, 5))
})

test_that("SqlBuilder where_regex", {
  b <- SqlBuilder$new("cards")$where_regex("text", "deals? \\d+")$build()
  expect_match(b$sql, "regexp_matches\\(text, \\$1\\)")
})

test_that("SqlBuilder where_fuzzy", {
  b <- SqlBuilder$new("cards")$where_fuzzy("name", "Bolt", threshold = 0.7)$build()
  expect_match(b$sql, "jaro_winkler_similarity\\(name, \\$1\\) > 0.7")
})

test_that("SqlBuilder where_fuzzy rejects bad threshold", {
  expect_error(SqlBuilder$new("cards")$where_fuzzy("name", "x", threshold = 1.5))
  expect_error(SqlBuilder$new("cards")$where_fuzzy("name", "x", threshold = -0.1))
})

test_that("SqlBuilder where_or", {
  b <- SqlBuilder$new("cards")$
    where_eq("setCode", "A25")$
    where_or(list("name = $1", "Bolt"), list("name = $1", "Counter"))$
    build()
  expect_match(b$sql, "\\(name = \\$2 OR name = \\$3\\)")
  expect_equal(b$params[[2]], "Bolt")
  expect_equal(b$params[[3]], "Counter")
})

test_that("SqlBuilder limit and offset", {
  b <- SqlBuilder$new("cards")$limit(10)$offset(5)$build()
  expect_match(b$sql, "LIMIT 10")
  expect_match(b$sql, "OFFSET 5")
})

test_that("SqlBuilder limit rejects negative", {
  expect_error(SqlBuilder$new("cards")$limit(-1))
})

test_that("SqlBuilder distinct", {
  b <- SqlBuilder$new("cards")$distinct()$build()
  expect_match(b$sql, "SELECT DISTINCT")
})

test_that("SqlBuilder join", {
  b <- SqlBuilder$new("cards")$
    join("JOIN sets s ON cards.setCode = s.code")$
    build()
  expect_match(b$sql, "JOIN sets s ON cards.setCode = s.code")
})

test_that("SqlBuilder group_by and having", {
  b <- SqlBuilder$new("cards")$
    select("setCode", "COUNT(*)")$
    group_by("setCode")$
    having("COUNT(*) > $1", 10)$
    build()
  expect_match(b$sql, "GROUP BY setCode")
  expect_match(b$sql, "HAVING COUNT\\(\\*\\) > \\$1")
})

test_that("SqlBuilder order_by", {
  b <- SqlBuilder$new("cards")$order_by("name ASC", "uuid DESC")$build()
  expect_match(b$sql, "ORDER BY name ASC, uuid DESC")
})

test_that("SqlBuilder chaining returns self", {
  q <- SqlBuilder$new("cards")
  result <- q$where_eq("name", "Bolt")
  expect_identical(result, q)
})

test_that("SqlBuilder parameter remapping", {
  b <- SqlBuilder$new("cards")$
    where_eq("a", 1)$
    where_eq("b", 2)$
    where("c = $1", 3)$
    build()
  expect_match(b$sql, "a = \\$1")
  expect_match(b$sql, "b = \\$2")
  expect_match(b$sql, "c = \\$3")
  expect_equal(b$params, list(1, 2, 3))
})
