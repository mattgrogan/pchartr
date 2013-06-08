\name{pchart_power}
\alias{pchart_power}
\title{Control Chart Power}
\usage{
  pchart_power(n, p, p.mean = p, ucl = NA, lcl = NA,
    len = 50, nRep = 200)
}
\arguments{
  \item{n}{number of observations in each sample}

  \item{p}{proportion defective}

  \item{p.mean}{scalar value of the control chart mean}

  \item{ucl}{scalar value of the upper control limit}

  \item{lcl}{scalar value of the lower control limit}

  \item{len}{length of each control chart}

  \item{nRep}{Number of repetitions}
}
\description{
  Estimate the power of control charts
}

