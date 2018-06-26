context("test multiSpider function")

testthat::test_that("multi spider function",{
  testthat::expect_error(multiSpider(c(1:5)))
  testthat::expect_error(multiSpider(x1 = matrix(runif(20,0,1), 4,5)), NA)
  testthat::expect_error(multiSpider(x1 = matrix(runif(20,0,1), 4,5), singlePanel = T))
  testthat::expect_error(multiSpider(x1 = matrix(runif(20,0,1), 4,5), rectangular = T), NA)
  testthat::expect_error(multiSpider(x1 = matrix(runif(20,0,1), 2,5), colRecBorder = c("blue", "green")), NA)
  testthat::expect_error(multiSpider(x1 = matrix(runif(20,0,1), 2,5), colRecBorder = c("blue", "green"), twist = 180), NA)

})
