\name{ots_commodities_short}
\alias{ots_commodities_short}
\title{A table of official commodity codes to ease visualization}
\docType{data}
\description{
Provides 1,225 official commodity codes taken from the United Nations official 
sources. The 'ots_commodities' table contains 5,206 codes that can be trimmed
from 6 to 4 digits to join this table and do aggregations.}
\usage{ots_commodities_short}
\format{
  A data frame with 1225 observations on the following 2 variables.
  \describe{
    \item{\code{commodity_code}}{Code of every commodity (e.g. '0101')}
    \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live')}
  }
}
\examples{
ots_commodities_short
}
\keyword{datasets}
