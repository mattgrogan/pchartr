\name{check_nelson_rules}
\alias{check_nelson_rules}
\title{Check all nelson rules}
\usage{
  check_nelson_rules(x, mean, ucl, lcl)
}
\arguments{
  \item{x}{vector of control chart values}

  \item{mean}{vector of control chart mean values}

  \item{ucl}{vector of upper control limit values}

  \item{lcl}{vector of lower control limit values}
}
\value{
  A nested list of length 8 containing the following
  components: \item{violated}{boolean indicating if the
  rule was violated} \item{matches}{vector of indices which
  violate the rule}
}
\description{
  Checks all Nelson rules for violations.
}

