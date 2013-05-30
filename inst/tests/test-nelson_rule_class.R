# Test object types

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

test_that("Rule 1 returns correct object", {
  rslt <- nelson.rule1(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 2 returns correct object", {
  rslt <- nelson.rule2(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 3 returns correct object", {
  rslt <- nelson.rule3(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 4 returns correct object", {
  rslt <- nelson.rule4(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 5 returns correct object", {
  rslt <- nelson.rule5(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 6 returns correct object", {
  rslt <- nelson.rule6(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 7 returns correct object", {
  rslt <- nelson.rule7(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})

test_that("Rule 8 returns correct object", {
  rslt <- nelson.rule8(x, mean, ucl, lcl)
  expect_that(is.nelson_rule(rslt), is_true())
})