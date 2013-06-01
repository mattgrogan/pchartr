# Test Nelson Rule Three
# ----------------------

# Set up data
x <- c(0.0912, 0.0942, 0.1032, 0.1043, 0.1068,
      0.1068, 0.0876, 0.1108, 0.1050, 0.0887,
      0.0881, 0.0990, 0.1184, 0.1121, 0.0986, 
      0.0966, 0.0876, 0.0933, 0.0954, 0.1155)

mean <- rep(mean(x), length(x))

n <- 500
sd <- sqrt(mean(x) * (1 - mean(x)) / n)
ucl <- mean + 3 * sd
lcl <- mean - 3 * sd

#-----------------------------------------------------------

test_that("no six points are increasing or decreasing", {
  rslt <- nelson.rule3(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(FALSE))
  expect_that(length(rslt$matches), equals(0))
  expect_that(rslt$first, equals(NULL))
})

test_that("six points are increasing", {
  
  # Set up data
  i <- seq(0.01, 8)
  x[10:17] <- lcl[10] + i
  
  # Run test
  rslt <- nelson.rule3(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(TRUE))
  expect_that(length(rslt$which), equals(7))
  expect_that(rslt$which, equals(11:17))
  expect_that(rslt$first, equals(16))
})

test_that("six points are decreasing", {
  
  # Set up data
  i <- seq(0.01, 7)
  x[10:16] <- ucl[10] - i
  
  # Run test
  rslt <- nelson.rule3(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(TRUE))
  expect_that(length(rslt$which), equals(6))
  expect_that(rslt$which, equals(11:16))
  expect_that(rslt$first, equals(16))
})
