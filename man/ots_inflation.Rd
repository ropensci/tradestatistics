\name{ots_inflation}
\alias{ots_inflation}
\docType{data}
\title{
A table with world weigthed mean inflation since 2000
}
\description{
Provides year to year inflations value to be applied as a conversion rate
to express dollars of year Y1 as dollars of year Y2. This dataset is provided
to be used with \code{ots_inflation_adjustment} that converts units forwards and
backwards in time.
}
\usage{data("ots_inflation")}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{from}}{Integer values in the range 1976-2019}
    \item{\code{to}}{Integer values in the range 1977-2020}
    \item{\code{conversion_factor}}{Numeric value expressed as one plus 1-year 
     inflation}
  }
}
\examples{
ots_inflation
}
\keyword{datasets}
