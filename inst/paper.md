---
title: 'tradestatistics: Open Trade Statistics API Wrapper and Utility Program'
tags:
  - R
  - database
  - trade
authors:
 - name: Mauricio Vargas
   orcid: 0000-0003-1017-7574
   affiliation: 1
affiliations:
 - name: Pontifical Catholic University of Chile
   index: 1
date: 30 Aug 2019
bibliography: references.bib
---

# Summary

Access to code and data is one of the conditions for full independent
replication [@Peng2011], and unfortunately many Latin American Universities have
limited or no access to @un_comtrade_2019. Is it under UN
authorization that I started @open_trade_statistics_api_2019 with the intention
to lower the barrier to working with international economic trade data and
promote a reproducibility culture.

Currently, we have a PostgreSQL [@postgresql] database that contains 56 years
(1962-2017) and 537 million records for 6,273 products traded by 241 countries
reported under  the Harmonized System of tariff nomenclature. By following a
part of results presented in @GaulierZignago2010 and the references therein,
our database contains records that correct missing or mismatching
reporter-partner flows by using mirrored flows as a key ingredient to provide
symmetric and consistent data.

To facilitate further analysis of this large database, the project involves a
REST API created by using `plumber` [@plumber]. Considering that our trivial base
of users has a social sciences background, we have to be aware that data formats
and datasets being are too complex to handle and use raises a task complexity
barrier that reduces Open Data benefits for the user [@Janssen2012]. Therefore,
providing just an API seems to be a partial solution to ease data access.

`tradestatistics` is an R package designed to support analysis of Open Trade
Statistics Database. Being this package a tool that facilitates data obtention
and analysis for users with some R programming language knowledge, we also have
to think about users with no programming knowledge but some basic spreadsheet
knowledge. The `tradestatistics` package was used by our team to create a
`shiny` dashboard that provides user tailored reports with interactive
visualization and option to download the rendered charts in different image
formats and the data besides the reports in different formats to conduct their
own analysis with Libre Office, Google Sheets, Python, or other software besides
R [@shiny;@shinydashboard].

# Acknowledgements

The author really appreciates the time spent by Amada Dobbyn, Emily Riederer,
Jorge Cimentada, Maelle Salmon and Mark Padgham during the rOpenSci Peer Review
process that involved changes to the DB schema, API and R package. Also Joshua 
Kunst help was greatly appreciated during early stages of the functions that 
evolved into the current `tradestatistics` package.

# References
