\name{ots_countries_colors}
\alias{ots_countries_colors}
\title{A table of official country codes and unofficial colors to ease 
 visualization}
\docType{data}
\description{
Provides official section country codes taken from the United Nations official 
sources but the colors are absolutely unofficial and based of what I consider a 
good palette for 276 countries/areas (249 countries + 22 special areas + 5 continents). This 
data is not used by the functions provided within this package and is provided 
as reference.
}
\usage{ots_countries_colors}
\format{
  A data frame with 276 observations on the following 2 variables.
  \describe{
    \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
    \item{\code{country_color}}{Section hex color (e.g. "#74c0e2")}
  }
}
\examples{
ots_countries_colors
}
\keyword{datasets}
