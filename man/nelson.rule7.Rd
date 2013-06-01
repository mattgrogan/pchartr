\name{nelson.rule7}
\alias{nelson.rule7}
\title{Nelson Rule Seven (15 points within 1 SD from the mean)}
\usage{
  nelson.rule7(x, mean, ucl, lcl)
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
  the rule} \item{first}{index of first violation}
}
\description{
  Checks for fifteen consecutive points within one standard
  deviation of the mean.
}

