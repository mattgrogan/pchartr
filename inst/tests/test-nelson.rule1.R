# Test Nelson Rule One
# --------------------

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

test_that("no data points are outside the control limits", {
  rslt <- nelson.rule1(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(FALSE))
  expect_that(length(rslt$matches), equals(0))
  expect_that(rslt$first, equals(NULL))
})

test_that("two data points are outside the control limits", {
  x[10] <- 0.15
  x[11] <- 0.03
  rslt <- nelson.rule1(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(TRUE))
  expect_that(length(rslt$which), equals(2))
  expect_that(rslt$which[1], equals(10))
  expect_that(rslt$which[2], equals(11))
  
  # First is correctly identified
  expect_that(rslt$first, equals(10))
})