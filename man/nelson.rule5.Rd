\name{nelson.rule5}
\alias{nelson.rule5}
\title{Nelson Rule Five (2 out of 3 points outside 2 SD from the mean)}
\usage{
  nelson.rule5(x, mean, ucl, lcl)
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
  Checks for two (or three) out of three points that are
  more than two standard deviations from the mean, in the
  same direction.
}

