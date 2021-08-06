\name{ots_commodities}
\alias{ots_commodities}
\title{A table of official commodity names from the Harmonized System (HS)}
\docType{data}
\description{
Provides official codes taken from the United Nations official sources.
This data is provided as a reference as it's not used by the functions
provided within this package, but it can be useful to subset the data
obtained from the API.
}
\usage{ots_commodities}
\format{
  A data frame with 1320 observations on the following 2 variables.
  \describe{
    \item{\code{commodity_code}}{Code of every commodity (e.g. 0101)}
    \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live')}
    \item{\code{group_code}}{Group code (e.g. 01)}
    \item{\code{group_fullname_english }}{Group name (e.g. Animals; live)}
  }
}
\examples{
ots_commodities
}
\keyword{datasets}
