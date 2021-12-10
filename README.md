
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
-   Seamless coupling with output from the `MOSART-WM` and `mosartwmpy`
    models.

### Under development

-   Flexible output formats for different power systems models (e.g.,
    GridView, PLEXOS, ReEDS).
-   Extension to hydropower plants in Canada and Mexico contributing to
    Western Interconnect.
-   Extension to entire CONUS domain (Eastern Interconnect, Texas).
-   Alternative technical for computing generation.

## Installation

Use `devtools` to install `hydrofixr` directly from Github:

`devtools::install_github("pnnl/hydrofixr")`

### Data requirements

-   [HydroSource Existing Hydropower Assets
    (EHA), 2021.](https://doi.org/10.21951/EHA_FY2021/1782791)
-   [EIA form 923](https://www.eia.gov/electricity/data/eia923/)
-   Simulation results from the MOSART-WM or
    [mosartwmpy](https://github.com/IMMM-SFA/mosartwmpy) large scale
    hydrological and water management model.

A pre-prepared input dataset formatted for `hydrofixr` is available
[TBD](TBD).

## Development history

### Origins

The original code and ideas behind `hydrofixr` were conceived in 2015 by
the Pacific Northwest National Laboratory. Initial developments were
supported though Laboratory Directed Research and Development (LDRD)
funds, and included post-processing of river simulation data from the
`MOSART-WM` model, leading to the creation of the “Water Scarcity Grid
Impact Factor” (a metric indicating the impact of regional water
conditions on water-dependent power generating assets) ([Voisin et al.,
2016](https://www.sciencedirect.com/science/article/abs/pii/S0360544216311732))
and monthly hydropower inputs for a production cost model ([Voisin et
al.,
2018](https://www.sciencedirect.com/science/article/abs/pii/S0360544216311732)).
This capability to create monthly energy targets for production cost
models was applied further in subsequent studies funded by the
Department of Energy (DoE) Office of Science, including drought impacts
on fuel prices ([O’Connell et al.,
2019](https://www.sciencedirect.com/science/article/pii/S0306261919301254)),
climate change impacts on the Western US power grid ([Voisin et al.,
2020](https://www.sciencedirect.com/science/article/pii/S030626192030979X)).

#### `wmpp` R package

With growing research applications and demands for plant-level,
regional-scale hydropower generation scenarios, the above-described
`MOSART-WM` post-processing capabilities were translated to R and
formalized as the `wmpp` (“WM Post-Processing”) package. `wmpp` included
two key developments: a statistical method for converting simulated
river flow to monthly hydropower generation (the prior method involved
shifting of a base year hydropower profile) and creation of of output
for a capacity expansion model. The latter development was in support of
a DoE Office of Electricity study investigating effects of drought and
climate change on capacity expansion and grid reliability ([Tidwell et
al.,
2020](https://www.wecc.org/Administrative/Climate%20Influences%20on%20Expansion%20Planning%20in%20Western%20US%20-%2020201120%20-%20Clean.pdf)).

#### Creation of `hydrofixr`

Recently, `wmpp` was enhanced with weekly-resolution energy targets and
parameterization for maximum and minimum hourly generation to support a
the creation of new hydropower datasets applied in research funded by
DoE Office of Energy Efficiency and Renewable Energy
[hydroWIRES](https://www.energy.gov/eere/water/hydrowires-initiative)
initiative.

The model was re-branded as `hydrofixr` and released on 2021-12-XX.
