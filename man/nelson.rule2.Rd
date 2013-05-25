\name{nelson.rule2}
\alias{nelson.rule2}
\title{Nelson Rule Two (9+ points on same side of mean)}
\usage{
  nelson.rule2(x, mean, ucl, lcl)
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
  Checks for nine or more points in a row on the same side
  of the mean.
}

