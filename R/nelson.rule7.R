#' Nelson Rule Seven (15 points within 1 SD from the mean)
#' 
#' Checks for fifteen consecutive points within one standard deviation of the mean.
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
nelson.rule7 <- function(x, mean, ucl, lcl) {
  
  retval <- list(violated=FALSE, which=NULL, first=NA)
  
  # Get a vector of differences
  sd <- (ucl - mean) / 3 # Assuming that ucl and lcl are both 3sd from mean
  
  # Find the ones above the mean
  tmp <- ifelse(x < mean + 1 * sd & x > mean - 1 * sd, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('1{15,}', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result[1] >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
    
    # Find the time that we first recognize the violation
    first.result <- gregexpr('1{15,}?', tmp)[[1]]
    retval$first <- first.result[1] + attr(first.result, 'match.length')[1]
  }
  
  retval$violated <- any(retval$which)
  
  retval$rule <- 7
  retval$descr <- "15 points within 1 SD from the mean"
  
  retval$x <- x
  retval$mean <- mean
  retval$ucl <- ucl
  retval$lcl <- lcl
  class(retval) <- 'nelson_rule'
  return(retval)  
  
}
