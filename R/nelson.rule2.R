#' Nelson Rule Two (9+ points on same side of mean)
#' 
#' Checks for nine or more points in a row on the same side of the mean.
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
nelson.rule2 <- function(x, mean, ucl, lcl) {
  retval <- list(violated=FALSE, which=NULL, first=NULL)
  
  # Get a vector with 1 = above mean and 2 = below mean
  tmp <- ifelse(x >= mean, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('0{9,}|1{9,}', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result[1] >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
    
    # Find the time that we first recognize the violation
    first.result <- gregexpr('0{9,}?|1{9,}?', tmp)[[1]]
    retval$first <- first.result[1] + attr(first.result, 'match.length')[1]
  }
  
  retval$violated <- any(retval$which)

  retval$rule <- 2
  retval$descr <- "9+ points on same side of mean"
  
  retval$x <- x
  retval$mean <- mean
  retval$ucl <- ucl
  retval$lcl <- lcl
  class(retval) <- 'nelson_rule'
  return(retval)  
}
