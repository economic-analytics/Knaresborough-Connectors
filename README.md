<!-- badges: start -->

<!-- badges: end -->

# EDD: Economic Data Dashboard

## About

### EDD
The Economic Data Dashboard (EDD) houses thousands of variables from hundreds of data sets of secondary and administrative data for multiple geographies within the United Kingdom.

It seeks to simplify the access, interrogation, visualisation and communication of complex data sources to accelerate research for policy design and evaluation. 

## Background
The main author of EDD has spent more than a decade working in regional economic policy and data analysis and became frustrated at how long it took to assemble economic data from various sources and produce even simple analyses. Different sources routinely use different date formats, non-standard geographic codes, different index dates and often have dimensions of the data stored in variable names. EDDIE seeks to simplify the analysis of economic data by providing a single interactive web-platform where economic data, through pre-processing, is available for easy charting with dimensions, dates, geographies and variable names are standardised, allowing for the viewing of data quickly and downloading both charts and raw data tables in a standardised format.

## Data

## Technical information

Built in R, EDD utilises the web architecture of the `shiny` package, visualisation from `ggplot2`, alongside back-end storage in SQL Server with data transfer handled natively within `dbplyr`.

## Data model

At the heart of EDD is a bespoke and flexible data structure that allows for dynamic realisation of datasets regardless of the number of dimensions within the data, different geography types, industry breakdowns, etc.

The data storage model is based on [Tidy Data](https://vita.had.co.nz/papers/tidy-data.html) principles and utilises R lists to provide rapid iteration of filtering and manipulation through functional programming. Data is manipulated in R and stored in both `.rds` format and also copied to and retrieved from a SQL Server database to reduce memory use. More information on the underlying data schema will be available shortly.


## Using EDD

EDD is currently deployed on the web at [https://christianspence.shinyapps.io/edd-app/](https://christianspence.shinyapps.io/edd-app/).
