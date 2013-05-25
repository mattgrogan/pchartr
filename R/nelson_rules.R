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
nelson.rule1 = function(x, mean, ucl, lcl) { 
  retval <- list()
  retval$which <- which(x > ucl | x < lcl)
  retval$violated <- any(retval$which)
  return(retval)
}

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
nelson.rule2 = function(x, mean, ucl, lcl) {
  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector with 1 = above mean and 2 = below mean
  tmp <- ifelse(x >= mean, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('0{9,}|1{9,}', tmp)[[1]]
  
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

#' Nelson Rule Three (6+ points increasing or decreasing)
#' 
#' Checks for six or more consecutive points increasing or decreasing
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A list containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
nelson.rule3 = function(x, mean, ucl, lcl) {

  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  tmp <- ifelse(diff(x) >= 0, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('0{6,}|1{6,}', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i]
      retval$which <- c(retval$which, seq(result[i]+1,m))
    }
  }
  
  retval$violated <- any(retval$which)
  
  return(retval)  
  
}

#' Nelson Rule Four (14+ points alternating)
#' 
#' Checks for fourteen or more points alternating in direction, increasing or decreasing.
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A list containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
nelson.rule4 = function(x, mean, ucl, lcl) {

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
  
  return(retval)  
  
}

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
nelson.rule5 = function(x, mean, ucl, lcl) {

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

#' Nelson Rule Six (4 out of 5 points outside 1 SD from the mean)
#' 
#' Checks for four (or five) out of five points that are more than one standard
#' deviation from the mean, in the same direction.
#' 
#' @param x vector of control chart values
#' @param mean vector of control chart mean values
#' @param ucl vector of upper control limit values
#' @param lcl vector of lower control limit values
#' 
#' @return A list containing the following components:
#' \item{violated}{boolean indicating if the rule was violated}
#' \item{matches}{vector of indices which violate the rule}
nelson.rule6 = function(x, mean, ucl, lcl) {
  
  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  sd <- (ucl - mean) / 3 # Assuming that ucl and lcl are both 3sd from mean
  
  # Find the ones above the mean
  tmp <- ifelse(x > mean + 1 * sd, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('(11111)|(10111)|(11011)|(11101)|(1111)', tmp)[[1]]
  
  # Aggregate the results into the return value
  if (result >= 0) {
    for (i in 1:length(result)) {
      m <- result[i] + attr(result, 'match.length')[i] - 1
      retval$which <- c(retval$which, seq(result[i],m))
    }
  }
  
  # Find the ones below the mean
  tmp <- ifelse(x < mean - 1 * sd, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('(11111)|(10111)|(11011)|(11101)|(1111)', tmp)[[1]]
  
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
nelson.rule7 = function(x, mean, ucl, lcl) {

  retval <- list(violated=FALSE, which=NULL)
  
  # Get a vector of differences
  sd <- (ucl - mean) / 3 # Assuming that ucl and lcl are both 3sd from mean
  
  # Find the ones above the mean
  tmp <- ifelse(x < mean + 1 * sd & x > mean - 1 * sd, 1, 0)
  
  # Collapse into string
  tmp <- paste0(tmp, collapse='')
  
  result <- gregexpr('1{15,}', tmp)[[1]]
  
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
nelson.rule8 = function(x, mean, ucl, lcl) {

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
  
  return(retval)  
  
}  
