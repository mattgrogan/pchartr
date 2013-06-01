\name{nelson.rule8}
\alias{nelson.rule8}
\title{Nelson Rule Eight (8 points with none within 1 SD from the mean)}
\usage{
  nelson.rule8(x, mean, ucl, lcl)
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
  Checks for eight consecutive points with none within one
  standard deviation of the mean and the points are in both
  directions from the mean.
}

