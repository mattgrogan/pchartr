#' Nelson Rule Four (14+ points alternating)
#' 
#' Checks for fourteen or more points alternating in direction.
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A list containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
#' @export
nelson.rule4 <- function(x, mean, ucl, lcl) {
  
  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  tmp <- ifelse(diff(x) >= 0, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('(01){7,}|(10){7,}', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i]
      retval$which <- c(retval$which, seq(result[i]+1,m))
    }
  }
  
  retval$violated <- any(retval$which)

  retval$rule <- 4
  retval$descr <- "14+ points alternating"
  
  retval$x <- x
  retval$mean <- mean
  retval$ucl <- ucl
  retval$lcl <- lcl
  class(retval) <- 'nelson_rule'
  return(retval)  
  
}
