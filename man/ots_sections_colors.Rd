\name{ots_sections_colors}
\alias{ots_sections_colors}
\title{A table of official section names from the Harmonized System rev 2007
 (HS07, also known as H3) and unofficial colors to ease visualization}
\docType{data}
\description{
Provides official section names taken from the United Nations official sources
but the colors are absolutely unofficial and based of what I consider a good
palette for 22 sections (21 + 1 for unspecified products). This data is not used
by the functions provided within this package and is provided as reference.
}
\usage{ots_sections_colors}
\format{
  A data frame with 5151 observations on the following 6 variables.
  \describe{
    \item{\code{commodity_code}}{Code of every commodity (e.g. 010110)}
    \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live, pure-bred breeding animals')}
    \item{\code{group_code}}{Group code (e.g. 01)}
    \item{\code{group_fullname_english }}{Group name (e.g. 'Animals; live')}
    \item{\code{section_code}}{Section code (e.g. 01)}
    \item{\code{section_fullname_english }}{Section name (e.g. 'Live animals and animal products')}
  }
}
\examples{
ots_sections_colors
}
\keyword{datasets}
