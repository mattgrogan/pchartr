\name{nelson.rule1}
\alias{nelson.rule1}
\title{Nelson Rule One (any point outside the control limits)}
\usage{
  nelson.rule1(x, mean, ucl, lcl)
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
  Checks for any points outside the upper and lower control
  limits.
}

