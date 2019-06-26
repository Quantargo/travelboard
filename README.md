
<!-- README.md is generated from README.Rmd. Please edit that file -->

# travelboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The holiday season is around the corner and we are flooded with
information for possible destinations. The goal is to create a Shiny app
to extract useful information from differnt data sources to ultimately
answer the question which holiday destination to choose.

We have pre-selected the following destinations:

  - Crete
  - Rome
  - Vienna
  - Lisbon
  - Mallorca

We would now like to present information for each of these destinations
using different information sources:

  - AirBnB (for rooms, prices, ratings, etc.)
  - Google Places (nearby restaurants, museums, maps)
  - Statistics from OECD (PPP, Quality of living, demographics, etc.)
  - Weather Data (forecasts and historical, temperature, rain)
  - Twitter Data (analyzing Twitter stream for respective location,
    sentiment, important words, etc.)

Each team shall choose one information source and create a Shiny
Dashboard page presenting useful information. The respective destination
will serve as an input and can be changed interactively.

Each team will work on one of the prepared modules:

  - AirBnB: `R/mod_airbnb.R`
  - Google Places: `R/mod_places.R`
  - Statistics: `R/mod_statistics.R`
  - Weather: `R/mod_weather.R`
  - Twitter, NYTimes Data: `R/mod_news.R`

Data is already prepared with the workspace in the folder `~/workshop`
and can be hard-coded within the application if used directly.
Alternatively, data can also be integrated within the `/data` folder.

Note, that for deployment the workshop data folder should be placed
within the package `inst` folder (but not commited to Git).

## Installation

You can install the released version of travelboard from
[Github](https://github.com/Quantargo/travelboard) with:

``` r
remotes::install_github("travelboard")
```

You can run the app after package has been installed using

`travelboard::run_app()`

or within the directory `inst/app` by selecting either `server.R` or
`ui.R` within RStudio and hitting the button **Run App**.

## Project Template

  - Template has been created using **golem**
  - Settings applied using `dev/01_start.R` and `dev/02_dev.R`
  - Created five modules
  - Called by `inst/app_server.R`
  - Checkout the template from Github at
    <https://github.com/Quantargo/travelboard>
  - Each team should only work on one module - additional functions can
    be added to the package, as well.
