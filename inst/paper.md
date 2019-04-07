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

@open_trade_statistics_api_2019 is an independent project focused on reproducible research as defined in @Peng2011. It provides a curated version of datasets from @un_comtrade_2019 by using some of the results from both @GaulierZignago2010 and @AndersonVanWincoop2004.

The **tradestatistics** package is a part of ROpenSci peer-reviewed software. It provides an API Wrapper and Utility Program that eases data extraction from @open_trade_statistics_api_2019.

Without this package you could obtain the same data from @open_trade_statistics_api_2019 at the expense of additional time and effort for the same results. As an API wrapper and utility program this package makes data obtaining faster and easier for you.

The added value of the package is to generate rectangular data from minified JSON inputs. By doing this, the package is aligned with the Tidy Data principles exposed in @Wickham2014 to ease reproducible research for both researchers and students.

This package relies heavily on the ``rlang`` package [@rlang] which provides tools to work with the core language features of base R and the ``tidyverse`` meta-package [@tidyverse]. Hence, it uses already existing functions with a shared underlying design philosophy from the R ecosystem.

# References
