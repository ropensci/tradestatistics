\name{ots_attributes_products}
\alias{ots_attributes_products}
\title{A table of official product names, Harmonized System (HS) codes 
and corresponding groups}
\docType{data}
\description{
Provides official codes taken from the United Nations official sources.
This data is provided as a reference as it's not used by the functions
provided within this package, but it can be useful to subset the data
obtained from the API.
}
\usage{ots_attributes_products}
\format{
  A data frame with 6373 observations on the following 4 variables.
  \describe{
    \item{\code{product_code}}{Code of every product (e.g. 0101)}
    \item{\code{product_fullname_english}}{HS product names (e.g. 'Horses, asses, mules and hinnies; live')}
    \item{\code{group_code}}{HS group code (e.g. 01)}
    \item{\code{group_name}}{HS group name (e.g. 'Animals; live')}
  }
}
\examples{
ots_attributes_products
}
\keyword{datasets}
