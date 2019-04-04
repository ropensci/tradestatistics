---
title: 'Tradestatistics: Open Trade Statistics API Wrapper and Utility Program'
authors:
- affiliation: 1
  name: Mauricio Vargas
  orcid: 0000-0003-1017-7574
date: "4 April 2019"
output: pdf_document
bibliography: REFERENCES.bib
tags:
- R
- international trade
- data access
affiliations:
- index: 1
  name: Pontifical Catholic University of Chile
---

# Summary

The **tradestatistics** is a part of ROpenSci peer-reviewed packages. It provides an API Wrapper and Utility Program that eases data extraction from [Open Trade Statistics](https://tradestatistics.io) which is an independent project focused on reproducible research as defined in @Peng2011.

Provided that the package connects to a public API created by the author of the package, the added value of the package is to generate rectangular data from minified JSON inputs. By doing this, the package is aligned with the Tidy Data principles exposed in @Wickham2014 to ease reproducible research for both researchers and students.

The current version of this package relies heavily on the ``rlang`` package [@rlang] which provides tools to work with the core language features of base R and the ``tidyverse`` package [@tidyverse]. This design decision results in a package that shares the underlying design philosophy, grammar, and data structures from common R packages.

# References
