context("Test queries")

test_that("basic project pageview queries work", {
  result <- project_pageviews()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 7)
})

test_that("Basic top-article queries work", {
  result <- top_articles()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1000)
  expect_true(ncol(result) == 8)
})

test_that("Basic per-article queries work", {
  result <- article_pageviews()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 8)
})

test_that("Basic per-article queries work (with date objects)", {
  result <- article_pageviews(start = Sys.Date() - 10, end = Sys.Date())
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 10)
  expect_true(ncol(result) == 8)
})

test_that("Timestamp conversion works", {
  expect_equal(pageview_timestamps(as.Date("2015-01-01")), "2015010100")
  expect_equal(pageview_timestamps(as.POSIXct("2015-01-01")), "2015010100")
  expect_equal(pageview_timestamps(as.POSIXlt("2015-01-01")), "2015010100")
})

test_that("Timestamps functions work with API", {
  result <- article_pageviews(project = "de.wikipedia"
      , article = "R_(Programmiersprache)"
      , start = pageview_timestamps(as.Date("2015-09-01"))
      , end = pageview_timestamps(as.Date("2015-09-30")))
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 30)
  expect_true(ncol(result) == 8)
})

test_that("'All' parameter is broken in wikimedia api", {
  expect_error(top_articles(day = "all", reformat = FALSE)
               , regexp = "The date\\(s\\) you used are valid")
})

test_that("User type and  parameters can be used", {
  result <- article_pageviews(start = Sys.Date() - 10,
  , end = Sys.Date()
  , user_type = c("all", "user", "spider", "bot")
  , platform = c("all", "desktop", "mobile-web", "mobile-app"))
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 160)
  expect_true(ncol(result) == 8)
})
