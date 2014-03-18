\name{average.rsqr}
\alias{average.rsqr}
\alias{reg.vars}
\alias{sig.store}
\title{average.rsqr}
\usage{
average.rsqr(y, x)

reg.vars(y, x)

sig.store(y, x)
}
\arguments{
  \item{y}{A numeric object with the same number of
  observations as rows in \code{x}}

  \item{x}{A matrix with a number of rows equal to
  dimensionality of \code{y}.}

  \item{y}{A numeric object with the same number of
  observations as rows in \code{x}}

  \item{x}{A matrix with a number of rows equal to
  dimensionality of \code{y}.}

  \item{y}{A numeric object with the same number of
  observations as rows in \code{x}}

  \item{x}{A matrix with a number of rows equal to
  dimensionality of \code{y}.}
}
\value{
A list with the elements \item{numeric}{Vector with R
average of the models including each covariate}

A list with the elements \item{matrix}{Matrix with the
coefficients for all regressions computed}
\item{numeric}{Vector with the R-squared of every
regression computed}

A list with the elements \item{matrix}{Matrix with 1 if the
coefficient is significant or 0 otherwise}
}
\description{
Compute the average R squared of the models including each
covariate

Calculate coefficients of all possible regressions from a
set of covariates

Determines if the coefficients of the regressions computed
by reg.vars are significant at the 95% level
}
\note{
Use it wisely

Use it wisely

Use it wisely
}
\examples{
myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4)
myY <- myX[,3]*3.5
average.rsqr(myY, myX)
myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4)
myY <- myX[,3]*3.5
reg.vars(myY, myX)
myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4)
myY <- myX[,3]*3.5
sig.store(myY, myX)
}
\author{
Michelle Torres

Michelle Torres

Michelle Torres
}
\seealso{
\code{\link{reg.vars}}

\code{\link{sig.store}}

\code{\link{reg.vars}}
}

