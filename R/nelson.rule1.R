#' Nelson Rule One (any point outside the control limits)
#' 
#' Checks for any points outside the upper and lower control limits.
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A list containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
#' \item{first}{index of first violation}
#' @export
nelson.rule1 <- function(x, mean, ucl, lcl) { 
  
  retval <- list()

  # Rule Data
  retval$which <- which(x > ucl | x < lcl)
  retval$violated <- any(retval$which)
  
  retval$rule <- 1 
  retval$descr <- "Points outside the control limits"
  
  if(length(retval$which) > 0) {
     retval$first <- min(retval$which)
  }
  
  retval$x <- x
  retval$mean <- mean
  retval$ucl <- ucl
  retval$lcl <- lcl
  class(retval) <- 'nelson_rule'
  
  return(retval)
}
