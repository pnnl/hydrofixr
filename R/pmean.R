#' get_pmean_models
#'
#' @param pcm e.g. "gridview", "plexos", ...leave as "none" for all CONUS plants, unformatted
#' @param NERC North American Electric Reliability Corporation region (e.g., WECC)
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param WM_case directory containing WM netcdfs (e.g., "WM_dev_base_case_cropped/")
#' @description gets pmean for all hydro plants
#' @importFrom raster brick extract
#' @importFrom dplyr select filter mutate mutate_if arrange left_join
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom purrr map_dfr map
#' @importFrom tidyr gather spread
#' @importFrom lubridate as_date year epiweek month
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
get_pmean_models <- function(pcm = "none", NERC = NULL,
                             data_dir, WM_case = "/WM_dev_base_case_cropped/"){

  WM_results_dir <- paste0(data_dir, WM_case)

  # read hydrosource plants and create filted for desired PCM
  read_HydroSource(data_dir = data_dir, NERC = NERC) ->
    hydrosource

  if(pcm == "none"){
    hydrosource %>%
      filter(!is.na(EIA_ID)) %>%
      .[["EIA_ID"]] %>% unique() ->
      EIA_IDs
  }

  if(pcm == "gridview"){
    read_gridview_plant_data() %>%
      filter(!is.na(EIA_ID)) %>% pull(EIA_ID) ->
      EIA_IDs
  }

  # get plant data and define MOSART grids
  hydrosource %>%
    filter(EIA_ID %in% EIA_IDs) %>%
    snap_to_MOSART_grid() %>%
    correct_lat_lon_for_MOSART_stream_network() %>%
    arrange(EIA_ID) ->
    plant_data

  # run through all result files to extract required flows for each cell
  list.files(WM_results_dir, full.names = T) %>%
    .[grep(".nc", .)] %>%
    map_dfr(function(slice){
      brick(slice, varname = "RIVER_DISCHARGE_OVER_LAND_LIQ") %>%
        extract(select(plant_data, lon_, lat_), df = T) %>%
        # ^^ extract using dam lat and lon
        as_tibble() %>%
        gather(date, value, -ID) %>% spread(ID, value) %>%
        mutate(date = as_date(substr(date, 2, nchar(date))))
    }) %>% arrange(date) %>%
    mutate_if(is.numeric, function(c) round(c, 3)) ->
    WM_flows_all_dams_daily

  # convert to monthly for model calibration
  WM_flows_all_dams_daily %>%
    mutate(year = as.integer(year(date)),
           month = month(date, label = T),
           month = factor(as.character(month), levels = month.abb)) %>%
    select(-date) %>%
    group_by(year, month) %>% summarise_if(is.double, mean) %>% ungroup() ->
    WM_all_flows_monthly

  # convert to training flows
  WM_all_flows_monthly %>%
    filter(year >= 2001) %>%
    gather(row_id, flow, -year, -month) %>%
    mutate(row_id = as.integer(row_id)) %>%
    left_join(plant_data %>% tibble::rowid_to_column("row_id"),
              by = "row_id") %>%
    select(year, month, EIA_ID, flow) ->
    training_flows

  # read EIA netgen (training target) and create data for calibration
  read_EIA_netgen(data_dir = data_dir) %>%
    left_join(n_hours, by = c("year", "month")) %>%
    mutate(av_gen = netgen_MWh / n_hours,
           month = factor(month, levels = c(month.abb))) %>%
    filter(EIA_ID %in% EIA_IDs) %>%
    left_join(training_flows,
              by = c("EIA_ID", "month", "year")) %>%
    filter(!is.na(flow)) %>%
    select(EIA_ID, year, month, av_gen, flow) ->
    data_for_calibration

  data_for_calibration %>%
    split(.$EIA_ID) %>%
    map_dfr(function(plant){

      # catch and remove full NA cases to avoid lm.fit failure
      if(all(is.na(plant[["av_gen"]]))) return(tibble())

      plant %>% split(.$month) %>%
        map_dfr(function(mth){

          # catch and remove full NA cases to avoid lm.fit failure
          if(all(is.na(mth[["av_gen"]]))) return(tibble())

          model_ <- lm(data = mth, av_gen ~ flow)

          mth %>% select(month, EIA_ID) %>%
            unique() %>%
            mutate(intercept = model_$coefficients[[1]],
                   slope = model_$coefficients[[2]]) ->
            month_model

          slp <- month_model[["slope"]]

          if(slp >= 0 & !is.na(slp)) return(month_model)

          # fix in case slope is negative
          # (monthly power will just be mean power, independent of flow)
          mth %>%
            mutate(intercept = mean(av_gen),
                   slope = 0) %>%
            select(month, EIA_ID, intercept, slope) %>%
            unique() %>% return()

        })
    }) -> calibrated_params

  training_flows %>%
    left_join(calibrated_params, by = c("EIA_ID", "month")) %>%
    left_join(plant_data %>% select(plant, EIA_ID),
              by = "EIA_ID") %>%
    mutate(av_gen_predicted = intercept + flow * slope) %>%
    rowwise() %>% mutate(av_gen_predicted = max(0, av_gen_predicted)) %>% ungroup() ->
    calibration_data

  calibration_data %>% filter(is.na(av_gen_predicted)) %>%
    .[["EIA_ID"]] %>% unique() -> plants_with_missing_data


  return(
    list(
      calibration_data = calibration_data %>% filter(!EIA_ID %in% plants_with_missing_data),
      WM_flows_all_dams_daily = WM_flows_all_dams_daily,
      plant_data = plant_data
      )
    )
}

#' get_pmean
#'
#' @param pcm e.g. "gridview", "plexos", ...leave as "none" for all CONUS plants
#' @param NERC North American Electric Reliability Corporation region (e.g., WECC)
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param WM_case directory containing WM netcdfs (e.g., "WM_dev_base_case/")
#' @param hyd_year hydrological year of pmean output
#' @param output three options: "weekly" (weekly resolution pmean), "monthly" (monthly resolution pmean), or "evaluation" (returns observed monthly generation and predicted monthly generation for all plants)
#' @param models the output of get_pmean_models()
#' @description gets pmean for all hydro plants
#' @importFrom raster brick extract
#' @importFrom dplyr select filter mutate mutate_if arrange left_join
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom purrr map_dfr map
#' @importFrom tidyr gather spread
#' @importFrom lubridate as_date year epiweek
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
get_pmean <- function(pcm = "none", NERC = NULL,
                      data_dir,
                      WM_case = "/WM_dev_base_case_cropped/",
                      mode = "monthly",
                      hyd_year = 2009){


  get_pmean_models(pcm = pcm,
                   WM_case = WM_case,
                   data_dir = data_dir) ->
    models

  models$calibration_data %>%
    select(month, EIA_ID, intercept, slope) %>%
    unique() -> calibrated_params

  if(mode == "weekly"){

    models$WM_flows_all_dams_daily %>%
      filter(year(date) %in% hyd_year) %>%
      mutate(epiweek = as.integer(epiweek(date)),
             year = year(date)) %>%
      group_by(epiweek, year) %>% summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      filter(epiweek != 53) %>%
      gather(row_id, flow, -epiweek, -year) %>%
      mutate(row_id = as.integer(row_id)) %>%
      left_join(models$plant_data %>% tibble::rowid_to_column("row_id"),
                by = "row_id") %>%
      select(year, epiweek, EIA_ID, flow) ->
      weekly_flows_all_plants

    weekly_flows_all_plants %>%
      left_join(gen_month_to_epiweek(), by = "epiweek") %>%
      left_join(calibrated_params,
                by = c("EIA_ID", "month")) %>%
      left_join(read_EIA_capabilities(data_dir = data_dir)) %>%
      mutate(capability = if_else(is.na(capability), nameplate, capability)) %>%
      rowwise() %>%
      mutate(pmean = min(intercept + flow * slope, capability),
             pmean = max(0, pmean)) %>%
      ungroup() %>%
      filter(!is.na(pmean)) %>%
      group_by(EIA_ID) %>%
      filter(!n() < 52) %>% ungroup() %>%
      select(EIA_ID, year, epiweek, pmean_MW = pmean) -> p_mean_all

    return(p_mean_all)

  }

  if(mode == "monthly"){

    models$calibration_data %>%
      filter(year == hyd_year) %>%
      left_join(read_EIA_capabilities(data_dir = data_dir),
                by = c("EIA_ID", "month")) %>%
      mutate(capability = if_else(is.na(capability), nameplate, capability)) %>%
      mutate(av_gen_predicted = if_else(av_gen_predicted > capability,
                                        capability, av_gen_predicted)) %>%
      select(EIA_ID, year, month, pmean_MW = av_gen_predicted) ->
      p_mean_all

    return(p_mean_all)

  }

}
