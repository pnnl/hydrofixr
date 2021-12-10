#' read_HydroSource
#'
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param NERC North American Electric Reliability Corporation region (e.g., WECC)
#' @description reads existing hydropower assets data from HydroSource
#' @importFrom vroom vroom
#' @importFrom dplyr filter
#' @return tibble of hydrosource plant data, filtered for chosen NERC
#' @export
#'
read_HydroSource <- function(data_dir, NERC = NULL){

  vroom(paste0(data_dir, "/HydroSource_HYC.csv"),
        col_types = c("plant" = "c",
                      "NERC" = "c",
                      "EHA_ID" = "c",
                      "EIA_ID" = "i",
                      "lat" = "d", "lon" = "d",
                      "state" = "c",
                      "county" = "c",
                      "HUC" = "c",
                      "bal_auth" = "c",
                      "mode" = "c",
                      "n_units" = "i",
                      "CH_MW" = "d", "CH_MWh" = "d")) ->
    hydrosource_HYC

  if(is.null(NERC)) return(hydrosource_HYC)

  return(
    hydrosource_HYC %>%
      filter(NERC == !!NERC)
  )

}


#' read_EIA_netgen
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @importFrom vroom vroom
#' @importFrom dplyr mutate
read_EIA_netgen <- function(data_dir){

  vroom(paste0(data_dir, "/EIA_NetGen_HYC.csv"),
        col_types = c("EIA_ID" = "i",
                      "year" = "i",
                      "month" = "c",
                      "netgen_MWh" = "d")) %>%
    mutate(month = factor(month, levels = month.abb))
}


#' read_gridview_plant_data
#'
#' @description gets gridview names and joins to EIA IDs for clean matching to
#' @importFrom vroom vroom
#' @importFrom dplyr select filter mutate left_join bind_rows
#' @importFrom stringr str_count
#' @importFrom tidyr separate
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
read_gridview_plant_data <- function(){

    data_dir <- system.file("extdata/PCM_plant_data/", package = "hydrofixr")

  vroom(paste0(data_dir, "/GridView_hydroplants.csv"),
        col_types = c("GeneratorKey" = "i",
                      "BusID" = "i",
                      "GeneratorName" = "c",
                      "LongName" = "c",
                      "State" = "c")) %>%
    filter(State %in% state.abb) ->
    plants_US_hydro_all

  # get easy matches
  plants_US_hydro_all %>%
    filter(paste0(State, "-") == substr(LongID, 1, 3),
           str_count(LongID, "-") == 2) %>%
    separate(LongID, into = c("State_", "EIA_ID", "unit"), sep = "-") %>%
    select(GenName = GeneratorName, BusID, EIA_ID) %>%
    filter(EIA_ID != "", EIA_ID != "??") %>%
    mutate(EIA_ID = as.integer(EIA_ID), BusID = as.integer(BusID)) -> gridview_EIA_1

  plants_US_hydro_all %>%
    filter(paste0(State, "-") == substr(LongID, 1, 3),
           stringr::str_count(LongID, "-") == 3) %>%
    separate(LongID, into = c("State_", "EIA_ID", "unit", "x"), sep = "-") %>%
    select(GenName = GeneratorName, BusID, EIA_ID) %>%
    filter(EIA_ID != "", EIA_ID != "??") %>%
    mutate(EIA_ID = as.integer(EIA_ID), BusID = as.integer(BusID)) -> gridview_EIA_2

  plants_US_hydro_all %>%
    filter(paste0(State, "-") != substr(LongID, 1, 3),
           stringr::str_count(LongID, "-") == 2) %>%
    separate(LongID, into = c("State_", "EIA_ID", "unit"), sep = "-") %>%
    filter(EIA_ID != "_") %>%
    select(GenName = GeneratorName, BusID, EIA_ID) %>%
    mutate(EIA_ID = as.integer(EIA_ID), BusID = as.integer(BusID)) -> gridview_EIA_3

  plants_US_hydro_all %>%
    filter(!GeneratorName %in% c(gridview_EIA_1[["GenName"]],
                                gridview_EIA_2[["GenName"]],
                                gridview_EIA_3[["GenName"]])) %>%
    left_join(gridview_to_NHD_join_helper, by = "GeneratorName") %>%
    select(GenName = GeneratorName, BusID, EIA_ID) %>%
    mutate(EIA_ID = as.integer(EIA_ID), BusID = as.integer(BusID)) -> gridview_EIA_4

  return(
    bind_rows(gridview_EIA_1, gridview_EIA_2, gridview_EIA_3, gridview_EIA_4)
  )
}

#' read_weekly_p_and_water
#'
#' @description gets pmax, pmin, and water availability (observed)
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param region "UC" (upper Colorado), "Col" (Columbia / PNW), "WECC"
#' @importFrom vroom vroom
#' @export
#'
read_weekly_p_and_water <- function(data_dir, region){

  if(!region %in% c("UC", "Col", "WECC")){
    stop("Region must be either UC, Col, or WECC")
  }

  # if(region == "UC"){
  #   return(
  #     vroom(
  #       paste0(system.file("extdata/", package = "hydrofixr"),
  #              "p_water_weekly_", region, ".csv"),
  #       col_types = cols()
  #     )
  #   )
  # }

  if(region == "Col"){
    return(
      vroom(paste0(data_dir, "/USACE_PNW_weekly_mmm.csv"),
                   col_types = c("EIA_ID" = "i",
                                 "year" = "i",
                                  "week" = "i",
                                  "mean" = "d",
                                  "min" = "d",
                                  "max" = "d")
            )
      )
  }

  if(region == "WECC"){
    return(
      vroom(paste0(data_dir, "/MISC_params_min_max_ador_97_weekly.csv"),
                   col_types = c("pseudo_ID" = "c",
                                 "year" = "c",
                                 "week" = "i",
                                 "min_param" = "d",
                                 "max_param" = "d",
                                 "ador_param" = "d",
                                 "state" = "c",
                                 "bal_auth" = "c")
            )
      )
  }


}

#' read_monthly_p_and_water
#'
#' @description gets pmax, pmin, and water availability (observed)
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param region "UC" (upper Colorado), "Col" (Columbia / PNW), "WECC"
#' @importFrom vroom vroom
#' @export
#'
read_monthly_p_and_water <- function(data_dir, region){

  if(!region %in% c("UC", "Col", "WECC")){
    stop("Region must be either UC, Col, or WECC")
  }


  if(region == "Col"){
    return(
      vroom(paste0(data_dir, "USACE_PNW_monthly_mmm.csv"),
                   col_types = c("EIA_ID" = "i",
                                 "year" = "i",
                                 "month" = "c",
                                 "mean" = "d",
                                 "min" = "d",
                                 "max" = "d")
            )
      )
  }

  if(region == "WECC"){
    return(
      vroom(paste0(data_dir, "MISC_params_min_max_ador_97_monthly/"),
                   col_types = c("pseudo_ID" = "c",
                                 "year" = "c",
                                 "week" = "i",
                                 "min_param" = "d",
                                 "max_param" = "d",
                                 "ador_param" = "d",
                                 "state" = "c",
                                 "bal_auth" = "c")
            )
      )
  }


}

#' read_EIA_capabilities
#'
#' @description reads EIA plant capabilities
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @importFrom vroom vroom cols col_factor
#' @export
#'
read_EIA_capabilities <- function(data_dir){

  vroom(paste0(data_dir, "/EIA_plant_capabilitiy.csv"),
        col_types = c("EIA_ID" = "i",
                      "month" = "c",
                      "nameplate" = "d",
                      "capability" = "d")) %>%
    mutate(month = factor(month, levels = month.abb))

}

