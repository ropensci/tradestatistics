\name{ots_countries}
\alias{ots_countries}
\title{A table of official country names, ISO-3 codes and other metadata}
\docType{data}
\description{
Provides official codes taken from the United Nations official sources.
This data is used by the functions provided within this package to validate
user parameters and add both product and country text columns to the data,
therefore reducing the number of API calls and the time to generate the
requested data.
}
\usage{ots_countries}
\format{
  A data frame with 249 observations on the following 6 variables.
  \describe{
    \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
    \item{\code{country_name_english}}{Country name (e.g. Germany)}
    \item{\code{country_fullname_english}}{Country name with indications (e.g. Germany (former Federal Republic of Germany until 1990))}
    \item{\code{continent_id}}{Numeric id of the continent where the country belongs to}
    \item{\code{continent}}{Continent where the country belongs to}
    \item{\code{eu28_member}}{Dummy variable such that 1 means "belongs to EU-28 group" and 0 otherwise}
\  }
}
\examples{
ots_countries
}
\keyword{datasets}
