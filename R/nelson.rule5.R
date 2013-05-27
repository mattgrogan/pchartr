#' Nelson Rule Five (2 out of 3 points outside 2 SD from the mean)
#' 
#' Checks for two (or three) out of three points that are more than two standard
#' deviations from the mean, in the same direction.
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
nelson.rule5 <- function(x, mean, ucl, lcl) {
  
  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  sd <- (ucl - mean) / 3 # Assuming that ucl and lcl are both 3sd from mean
  
  # Find the ones above the mean
  tmp <- ifelse(x > mean + 2 * sd, 1, 0) # Must be in same direction
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('(111)|(11)|(101)', tmp)[[1]] # Removed 011 b/c redundant
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
  }
  
  # Find the ones below the mean
  tmp <- ifelse(x < mean - 2 * sd, 1, 0) # Must be in same direction
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('(111)|(11)|(101)', tmp)[[1]] # Removed 011 b/c redundant
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
  }    
  
  retval$violated <- any(retval$which)
  
  return(retval)  
  
}
