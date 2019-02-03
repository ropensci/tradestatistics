\name{ots_attributes_products}
\alias{ots_attributes_products}
\title{A table of official product names, Harmonized System (HS) codes 
and corresponding groups}
\docType{data}
\description{
Provides official codes taken from the United Nations official sources.
This data is used by the functions provided within this package to
add product and country text columns to the data by taking the data from
the API that just provides official codes and numerical values.
}
\usage{ots_attributes_products}
\format{
  A data frame with 6373 observations on the following 4 variables.
  \describe{
    \item{\code{commodity_code}}{Code of every product (e.g. 0101 means horses)}
    \item{\code{commodity_fullname_english}}{HS product names (e.g. horses, bovine, pigs, etc)}
    \item{\code{group_code}}{HS group code (e.g. 01 means animal products)}
    \item{\code{group_name}}{HS group name (e.g. animal products, vegetable products, etc)}
  }
}
\examples{
ots_attributes_products
}
\keyword{datasets}
