# Test Nelson Rule Seven
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

test_that("no fifteen points are within 1 SD", {
  rslt <- nelson.rule7(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(FALSE))
  expect_that(length(rslt$matches), equals(0))
  expect_that(rslt$first, equals(NULL))
})

test_that("fifteen points are within 1 SD", {
  
  # Set up data for test
  x[5:19] <- mean[5:19] - 0.01
  
  # Do test
  rslt <- nelson.rule7(x, mean, ucl, lcl)
  expect_that(rslt$violated, equals(TRUE))
  expect_that(length(rslt$which), equals(19))
  expect_that(rslt$which, equals(1:19))
  expect_that(rslt$first, equals(16))
})
