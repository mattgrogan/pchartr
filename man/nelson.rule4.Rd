\name{nelson.rule4}
\alias{nelson.rule4}
\title{Nelson Rule Four (14+ points alternating)}
\usage{
  nelson.rule4(x, mean, ucl, lcl)
}
\arguments{
  \item{x}{vector of control chart values}

  \item{mean}{vector of control chart mean values}

  \item{ucl}{vector of upper control limit values}

  \item{lcl}{vector of lower control limit values}
}
\value{
  A list containing the following components:
  \item{violated}{boolean indicating if the rule was
  violated} \item{matches}{vector of indices which violate
  the rule}
}
\description{
  Checks for fourteen or more points alternating in
  direction.
}

