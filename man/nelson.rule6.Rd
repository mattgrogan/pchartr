\name{nelson.rule6}
\alias{nelson.rule6}
\title{Nelson Rule Six (4 out of 5 points outside 1 SD from the mean)}
\usage{
  nelson.rule6(x, mean, ucl, lcl)
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
  Checks for four (or five) out of five points that are
  more than one standard deviation from the mean, in the
  same direction.
}

