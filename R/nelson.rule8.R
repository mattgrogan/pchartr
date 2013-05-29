#' Nelson Rule Eight (8 points with none within 1 SD from the mean)
#' 
#' Checks for eight consecutive points with none within one standard deviation
#' of the mean and the points are in both directions from the mean.
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
nelson.rule8 <- function(x, mean, ucl, lcl) {
  
  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  sd <- (ucl - mean) / 3 # Assuming that ucl and lcl are both 3sd from mean
  
  # Find the ones above the mean
  tmp <- ifelse(x > mean + 1 * sd | x < mean - 1 * sd, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('1{8,}', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
  }
  
  retval$violated <- any(retval$which)
  
  retval$x <- x
  retval$mean <- mean
  retval$ucl <- ucl
  retval$lcl <- lcl
  class(retval) <- 'nelson_rule'
  return(retval)  
  
}  
