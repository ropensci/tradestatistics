.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "
    Welcome to tradestatistics package. If you are going to use this
    package, it means that you are ok with the usage conditions explained
    on tradestatistics.io/site/data2/index.html

    Commercial purposes are strictly out of the boundaries of what you 
    can do with this data according to UN Comtrade dissemination clauses.

    Our contents are distributed under Creative Commons 
    Attribution-NonCommercial 4.0 International License.
    "
  )
}
