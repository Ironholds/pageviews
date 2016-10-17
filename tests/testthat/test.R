context("Test queries")

test_that("basic project pageview queries work", {
  result <- project_pageviews()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 7)
  expect_true(is.character(result$project))
  expect_true(is.character(result$language))
  expect_true(is.character(result$access))
  expect_true(is.character(result$agent))
  expect_true(is.character(result$granularity))
  expect_true("POSIXct" %in% class(result$date))
  expect_true(is.numeric(result$views))
})

test_that("Basic top-article queries work", {
  result <- top_articles()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1000)
  expect_true(ncol(result) == 8)
  expect_true(is.character(result$project))
  expect_true(is.character(result$language))
  expect_true(is.character(result$access))
  expect_true(is.character(result$granularity))
  expect_true("POSIXct" %in% class(result$date))
  expect_true(is.numeric(result$rank))
  expect_true(is.numeric(result$views))
})

test_that("Basic per-article queries work", {
  result <- article_pageviews()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 8)
  expect_true(is.character(result$project))
  expect_true(is.character(result$language))
  expect_true(is.character(result$article))
  expect_true(is.character(result$access))
  expect_true(is.character(result$granularity))
  expect_true("POSIXct" %in% class(result$date))
  expect_true(is.character(result$agent))
  expect_true(is.numeric(result$views))

  obama_pageviews <- article_pageviews(article = "Barack_Obama")
  expect_true(is.data.frame(obama_pageviews))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 8)
})

test_that("Basic per-article queries work (with date objects)", {
  result <- article_pageviews(start = Sys.Date() - 10, end = Sys.Date())
  expect_true(is.data.frame(result))
  testthat::expect_gte(nrow(result), 9)
  expect_true(ncol(result) == 8)
  expect_true(is.character(result$project))
  expect_true(is.character(result$language))
  expect_true(is.character(result$article))
  expect_true(is.character(result$access))
  expect_true(is.character(result$granularity))
  expect_true("POSIXct" %in% class(result$date))
  expect_true(is.character(result$agent))
  expect_true(is.numeric(result$views))
})

test_that("Timestamp conversion works", {
  expect_equal(pageview_timestamps(as.Date("2015-01-01")), "2015010100")
  expect_equal(pageview_timestamps(as.POSIXct("2015-01-01")), "2015010100")
  expect_equal(pageview_timestamps(as.POSIXlt("2015-01-01")), "2015010100")
})

test_that("Timestamps functions work with API", {
  result <- article_pageviews(project = "de.wikipedia",
      article = "R_(Programmiersprache)",
      start = pageview_timestamps(as.Date("2015-09-01")),
      end = pageview_timestamps(as.Date("2015-09-30")))
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 30)
  expect_true(ncol(result) == 8)
})


test_that("User type and platform can be used (pageviews).", {
  result <- article_pageviews(start = Sys.Date() - 10,
    end = Sys.Date(),
    user_type = c("all", "user", "spider", "bot"),
    platform = c("all", "desktop", "mobile-web", "mobile-app"))
  expect_true(is.data.frame(result))
  expect_gt(length(unique(result$agent)), 1)
  expect_true(ncol(result) == 8)
})

test_that("Platform can be used (top_articles)", {
  result <- top_articles(platform = c("all", "desktop",
                        "mobile-web", "mobile-app"))
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 3500 & nrow(result) <= 4000)
  expect_true(ncol(result) == 8)
})

test_that("Hourly granularity works for `project_pageviews`", {
  result <- project_pageviews(granularity = "hourly", end = "2015100123")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 24)
  expect_true(ncol(result) == 7)
})
