context("test spider function")

testthat::test_that("spider function works", {

  testthat::expect_error(spider(c(1:5)))
  testthat::expect_error(spider(c(-1:1)))
  testthat::expect_error(spider(c(1:5), maxValues = c(1:5)),NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5)), NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5), singlePanel = T), NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5), singlePanel = T, rectangular = T), NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5), singlePanel = T, rectangular = T, titles = "test"), NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5), x2 = matrix(runif(20,0,1), 4,5),singlePanel = T, rectangular = T, titles = "test"), NA)
  testthat::expect_error(spider(x1 = matrix(runif(20,0,1), 4,5), x2 = matrix(runif(20,0,1), 2,5),singlePanel = T, rectangular = T, titles = "test"))
})
