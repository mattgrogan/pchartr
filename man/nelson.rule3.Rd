\name{nelson.rule3}
\alias{nelson.rule3}
\title{Nelson Rule Three (6+ points increasing or decreasing)}
\usage{
  nelson.rule3(x, mean, ucl, lcl)
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
  Checks for six or more consecutive points increasing or
  decreasing
}

