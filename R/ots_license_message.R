.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "
    Welcome to tradestatistics package. Visit 
    docs.tradestatistics.io/datasets.html to check the
    code of conduct and full-detail tables available in direct download.

    Commercial purposes are strictly out of the boundaries of what you 
    can do with this data according to UN Comtrade dissemination clauses.

    Our data is distributed under Creative Commons 
    Attribution-NonCommercial 4.0 International License.
    "
  )
}
