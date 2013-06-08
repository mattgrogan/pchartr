#' Control Chart Power
#' 
#' Estimate the power of control charts
#' 
#' @param n number of observations in each sample
#' @param p proportion defective
#' @param p.mean scalar value of the control chart mean
#' @param ucl scalar value of the upper control limit
#' @param lcl scalar value of the lower control limit
#' @param len length of each control chart 
#' @param nRep Number of repetitions
#' 
#' @export
pchart_power <- function(n, p, p.mean = p, ucl = NA, lcl = NA, len = 50, nRep = 200) {
  


  # ---------------------------------------------
  # Expand mean
  if (length(mean) == 1) {
    p.mean <- rep(p.mean, len)
  }

  # ----------------------------------------------
  # If ucl and lcl are NA, then calculate using the
  # normal approximation to the binomial
  
  # Calculate ucl
  if (is.na(ucl)) {
    p.sd <- sqrt(p * (1-p) / n)
    ucl <- p.mean + 3 * p.sd
  } else {
    ucl <- rep(ucl, len)    
  }
  
  # calculate the lcl
  if (is.na(lcl)) {
    p.sd <- sqrt(p * (1-p) / n)
    lcl <- p.mean - 3 * p.sd    
  } else {
    lcl <- rep(lcl, len)    
  }
  
  # ---------------------------------------------
  # Run the simulation
  
  # Set up data matrix
  firsts <- matrix(NA, nRep, 8)
  colnames(firsts) <- paste("Rule", 1:8, sep=".")
  
  for (i in 1:nRep) {
    
    # Sample from the binomial distribution
    x <- rbinom(len, n, p) / n   
    
    firsts[i, 1] <- nelson.rule1(x, p.mean, ucl, lcl)$first
    firsts[i, 2] <- nelson.rule2(x, p.mean, ucl, lcl)$first
    firsts[i, 3] <- nelson.rule3(x, p.mean, ucl, lcl)$first
    firsts[i, 4] <- nelson.rule4(x, p.mean, ucl, lcl)$first
    firsts[i, 5] <- nelson.rule5(x, p.mean, ucl, lcl)$first
    firsts[i, 6] <- nelson.rule6(x, p.mean, ucl, lcl)$first
    firsts[i, 7] <- nelson.rule7(x, p.mean, ucl, lcl)$first
    firsts[i, 8] <- nelson.rule8(x, p.mean, ucl, lcl)$first
  }
  
  firsts
  
}