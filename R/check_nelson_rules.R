#' Check all nelson rules
#'
#' Checks all Nelson rules for violations.
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A nested list of length 8 containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
#' @export
check_nelson_rules <- function(x, mean, ucl, lcl) {
  r1 <- nelson.rule1(x, mean, ucl, lcl)
  r2 <- nelson.rule2(x, mean, ucl, lcl)
  r3 <- nelson.rule3(x, mean, ucl, lcl)
  r4 <- nelson.rule4(x, mean, ucl, lcl)
  r5 <- nelson.rule5(x, mean, ucl, lcl)
  r6 <- nelson.rule6(x, mean, ucl, lcl)
  r7 <- nelson.rule7(x, mean, ucl, lcl)
  r8 <- nelson.rule8(x, mean, ucl, lcl)
  retval <- list(x, mean, ucl, lcl, r1, r2, r3, r4, r5, r6, r7, r8)
  class(retval) <- 'nelson_rule'
  return(retval)
}
