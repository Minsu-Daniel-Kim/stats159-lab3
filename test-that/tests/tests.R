# load the source code of the functions to be tested
source("../functions/range-value.R")
source("../functions/missing-values.R")
source("../functions/center_measures.R")
source("../functions/spread_measures.R")
source("../functions/descriptive_stats.R")


# context with one test that groups expectations
context("Test for range value") 

test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(range_value(x), 4)
  expect_length(range_value(x), 1)
  expect_type(range_value(x), 'double')
  
  y <- c(1, 2, 3, 4, NA)
  expect_equal(range_value(y, na.rm = TRUE), 3)
  expect_length(range_value(y), 1)
  # expect_length(range_value(x), 1)

})


test_that("range value for numeric vectors with NAs", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(range_value(y), NA_real_)
  expect_length(range_value(y), 1)
})


test_that("range value for logical vectors", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(range_value(z), 1L)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), 'integer')
})


test_that("range value stops for character vectors", {
  w <- letters[1:5]
  
  expect_error(range_value(w))
})


context("Test for missing value") 


test_that("missing value", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, NA)
  z <- c(1, NA, 3, 4, NA)
  nas <- rep(NA, 10)
  
  expect_equal(missing_values(x), 0)
  expect_length(missing_values(x), 1)
  expect_gte(missing_values(x), 0)
  expect_equal(missing_values(y), 1)
  expect_length(missing_values(y), 1)
  expect_equal(missing_values(z), 2)
  expect_length(missing_values(z), 1)
  expect_equal(missing_values(nas), 10)
})

test_that("center measures", {
  
  x <- c(1, 2, 3, 4, 5)
  y <- c(12, 13, 11, 8, 3)
  expect_equal(center_measures(x)[1], 3)
  expect_equal(center_measures(x)[2], 3)
  expect_equal(center_measures(y)[1], 11)
  expect_equal(center_measures(y)[2], 9.4)
  
})

test_that("spread measures", {
  
  x <- c(1, 2, 3, 4, 5)
  y <- c(12, 13, 11, 8, 3)
  expect_equal(spread_measures(x)[1], 4)
  expect_equal(spread_measures(x)[2], 2)
  expect_equal(spread_measures(x)[3], round(1.581139, 1))
  
  expect_equal(spread_measures(y)[1], 10)
  expect_equal(spread_measures(y)[2], 4)
  expect_equal(spread_measures(y)[3], round(4.037326, 1))
  
})

test_that("descriptive stats", {
  
  x <- c(1, 2, 3, 4, 5)
  y <- c(12, 13, 11, 8, 3)
  expect_equal(descriptive_stats(x)[1], 3)
  expect_equal(descriptive_stats(x)[2], 3)
  expect_equal(descriptive_stats(x)[3], 4)
  expect_equal(descriptive_stats(x)[4], 2)
  expect_equal(descriptive_stats(x)[5], round(1.581139, 1))
  expect_equal(descriptive_stats(x)[6], 0)
  
  
  expect_equal(descriptive_stats(y)[1], 11)
  expect_equal(descriptive_stats(y)[2], 9.4)
  expect_equal(descriptive_stats(y)[3], 10)
  expect_equal(descriptive_stats(y)[4], 4)
  expect_equal(descriptive_stats(y)[5], round(4.037326, 1))
  expect_equal(descriptive_stats(y)[6], 0)
  
})