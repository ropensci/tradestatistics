\name{ots_products}
\alias{ots_products}
\title{A table of official product names from the Harmonized System (HS)}
\docType{data}
\description{
Provides official codes taken from the United Nations official sources.
This data is provided as a reference as it's not used by the functions
provided within this package, but it can be useful to subset the data
obtained from the API.
}
\usage{ots_products}
\format{
  A data frame with 1320 observations on the following 2 variables.
  \describe{
    \item{\code{product_code}}{Code of every product (e.g. 0101)}
    \item{\code{product_fullname_english}}{HS product names (e.g. 'Horses, asses, mules and hinnies; live')}
  }
}
\examples{
ots_products
}
\keyword{datasets}
