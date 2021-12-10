
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # <img src='man/figures/hydrofixr logo v2.png' align="centre" height="101.5" /> -->
<!-- badges: start -->
<!-- badges: end -->

# hydro<span style="color:dodgerblue">fixr</span>

## About

`hydrofixr` is a tool for creating hydropower data to drive Production
Cost Models (PCMs) and Capacity Expansion Models (CEMs).

**Contact:** [Sean Turner](sean.turner@pnnl.gov) or [Nathalie
Voisin](nathalie.voisin@pnnl.gov), Pacific Northwest National Laboratory

## Features

-   Monthly and weekly generation estimates for hundreds of hydropower
    plants throughout Western United States.
-   Monthly and weekly constraints (maximum and minimum generation) for
    individual plants.
-   Historical and future projected climate and water availability
    conditions.
-   Options to compute hydropower from large-scale regulated river
    simulations or USGS regulated flow observations.
-   Seamless coupling with output from the `mosartwmpy` model.
-   Flexible output formats for different power systems models
    (GridView, PLEXOS, ReEDS).

### Under development

-   Extension to hydropower plants in Canada and Mexico contributing to
    Western Interconnect.
-   Extension to entire CONUS domain (Eastern Interconnect, Texas).
-   New options for computing generation.

## Installation

Use `devtools` to install `hydrofixr` directly from Github:

`devtools::install_github("pnnl/hydrofixr")`

### Data requirements

-   [HydroSource Existing Hydropower Assets
    (EHA), 2021.](https://doi.org/10.21951/EHA_FY2021/1782791)
-   [EIA form 923](https://www.eia.gov/electricity/data/eia923/)

A pre-prepared input dataset can be found.

## Development history

…
