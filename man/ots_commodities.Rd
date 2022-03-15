\name{ots_commodities}
\alias{ots_commodities}
\title{A table of official commodity names from the Harmonized System rev 2012
 (HS12, also known as H4)}
\docType{data}
\description{
Provides official commodity and section codes and names taken from the 
United Nations official sources. This data is used by the functions provided
within this package to complement the data obtained from the API.
}
\usage{ots_commodities}
\format{
  A data frame with 5151 observations on the following 4 variables.
  \describe{
    \item{\code{commodity_code}}{Code of every commodity (e.g. 010110)}
    \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live, pure-bred breeding animals')}
    \item{\code{section_code}}{Section code (e.g. 01)}
    \item{\code{section_fullname_english }}{Section name (e.g. 'Live animals and animal products')}
  }
}
\examples{
ots_commodities
}
\keyword{datasets}
