
context("Test queries")

test_that("basic project pageview queries work", {
  result <- project_pageviews()
  expect_true(is.data.frame(result))
  expect_true(nrow(result) == 1)
  expect_true(ncol(result) == 6)
})
