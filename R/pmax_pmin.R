#' get_pmax_pmin_params
#'
#' @param zone CRB or WECC
#' @param mode resolution of output for CRB
#' @param data_dir data directory containing hydrofixr consolidated inputs
#' @param smooth_params should weekly parameters (CRB only) be smoothed?
#' @description gets pmax and pmin estimates for all dams
#' @importFrom raster brick extract
#' @importFrom dplyr select filter mutate mutate_if arrange left_join
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom purrr map_dfr map
#' @importFrom tidyr gather spread fill
#' @importFrom lubridate as_date year
#' @importFrom modelr add_predictions
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
get_pmax_pmin_params <- function(zone, mode = "monthly", data_dir, smooth_params = TRUE){

  if(zone == "CRB"){

    if(mode == "weekly"){
      # read max, mean, min observations for CRB dams...
      # ... and join with HydroSource data to get capacities
      read_weekly_p_and_water(data_dir = data_dir, "Col") %>%
        left_join(read_HydroSource(data_dir), by = "EIA_ID") %>%
        select(EIA_ID, year, epiweek = week, max, mean, min, nameplate_HS = CH_MW) %>%
        append_capabilities(resolution = "weekly", data_dir = data_dir) %>%
        # constrain capacity (in case of error)
        mutate(
          max = if_else(max > capability, capability, max),
          min = if_else(min < 0, 0, min)
        ) -> max_min_mean_obs

      max_min_mean_obs %>%
        filter(epiweek != 53) %>%
        split(.$EIA_ID) %>%
        map_dfr(function(dam){
          dam %>%
            split(.$epiweek) %>%
            map_dfr(function(week){
              week %>%
                mutate(cap_minus_mean = capability - mean,
                       pmax_minus_mean = max - mean) -> x

              lm_max <- lm(pmax_minus_mean ~ cap_minus_mean + 0, data = x)
              lm_min <- lm(min ~ mean + 0, data = x)

              x %>%
                add_predictions(lm_max, "max_minus_mean_pred") %>%
                add_predictions(lm_min, "min_pred") %>%
                mutate(max_pred = max_minus_mean_pred + mean,
                       max_param = lm_max[[1]] %>% unname(),
                       min_param = lm_min[[1]] %>% unname()) %>%
                select(EIA_ID, year, epiweek, mean, min, max, capability,
                       max_pred, min_pred, max_param, min_param)
            }) -> pmax_pmin_week_x

          if(smooth_params == FALSE){
            return(
              pmax_pmin_week_x %>%
                select(EIA_ID, epiweek, min_param, max_param) %>%
                unique()
              )
          }

          pmax_pmin_week_x %>%
            select(epiweek, min_param, max_param) %>%
            unique() %>%
            add_predictions(loess(min_param ~ epiweek, .), var = "pred_min") %>%
            add_predictions(loess(max_param ~ epiweek, .), var = "pred_max") %>%
            mutate(min_param = pred_min,
                   max_param = pred_max) %>% select(-pred_min, -pred_max) ->
            smoothed_params

          pmax_pmin_week_x %>% select(-min_param, -max_param) %>%
            left_join(smoothed_params, by = "epiweek") %>%
            select(EIA_ID, epiweek, min_param, max_param) %>%
            unique()

        }) -> CRB_pmax_pmin_models

      return(CRB_pmax_pmin_models)
    }

    if(mode == "monthly"){
      # read max, mean, min observations for CRB dams...
      # ... and join with HydroSource data to get capacities
      read_monthly_p_and_water(data_dir = data_dir, "Col") %>%
        #left_join(dam_codes, by = "dam") %>%
        left_join(read_HydroSource(data_dir), by = "EIA_ID") %>%
        select(EIA_ID, year, month, max, mean, min, nameplate_HS = CH_MW) %>%
        append_capabilities("monthly", data_dir = data_dir) %>%
        # constrain capacity (in case of error)
        mutate(
          max = if_else(max > capability, capability, max),
          min = if_else(min < 0, 0, min)
        ) %>% left_join(tibble(m_ = 1:12, month = month.abb)) %>%
        mutate(month = m_) %>% select(-m_) ->
        max_min_mean_obs

      max_min_mean_obs %>%
        split(.$EIA_ID) %>%
        map_dfr(function(dam){
          dam %>%
            split(.$month) %>%
            map_dfr(function(month){
              month %>%
                mutate(cap_minus_mean = capability - mean,
                       pmax_minus_mean = max - mean) -> x

              lm_max <- lm(pmax_minus_mean ~ cap_minus_mean + 0, data = x)
              lm_min <- lm(min ~ mean + 0, data = x)

              x %>%
                add_predictions(lm_max, "max_minus_mean_pred") %>%
                add_predictions(lm_min, "min_pred") %>%
                mutate(max_pred = max_minus_mean_pred + mean,
                       max_param = lm_max[[1]] %>% unname(),
                       min_param = lm_min[[1]] %>% unname()) %>%
                select(EIA_ID, year, month, mean, min, max, capability,
                       max_pred, min_pred, max_param, min_param)
            }) -> pmax_pmin_month_x

          if(smooth_params == FALSE){
            return(pmax_pmin_month_x %>%
                     left_join(tibble(month = 1:12,
                                      month_abb = factor(month.abb, levels = month.abb)),
                               by = "month") %>%
                     select(EIA_ID, month = month_abb, min_param, max_param) %>%
                     unique())
          }

          pmax_pmin_month_x %>%
            select(month, min_param, max_param) %>%
            unique() %>%
            add_predictions(loess(min_param ~ month, .), var = "pred_min") %>%
            add_predictions(loess(max_param ~ month, .), var = "pred_max") %>%
            mutate(min_param = pred_min,
                   max_param = pred_max) %>% select(-pred_min, -pred_max) ->
            smoothed_params

          pmax_pmin_month_x %>% select(-min_param, -max_param) %>%
            left_join(smoothed_params, by = "month") %>%
            left_join(tibble(month = 1:12,
                             month_abb = factor(month.abb, levels = month.abb)),
                      by = "month") %>%
            select(EIA_ID, month = month_abb, min_param, max_param) %>%
            unique()

        }) -> CRB_pmax_pmin_models

      return(CRB_pmax_pmin_models)
    }

  }

  if(zone == "WECC"){

    read_weekly_p_and_water(data_dir = data_dir, "WECC") %>%
      left_join(read_HydroSource(), by = "EIA_ID") %>%
      select(EIA_ID, year, epiweek = week, max, mean, min, nameplate_HS = CH_MW) %>%
      append_capabilities("weekly") %>%
      mutate(capability = if_else(is.na(capability), nameplate_EIA, capability)) %>%
      filter(!is.na(capability)) ->
      WECC_max_min_data

    WECC_max_min_data %>%
      group_by(EIA_ID) %>%
      summarise(capability = mean(capability),
                mean = sum(mean)) %>%
      filter(!is.na(capability),
             mean > 0) %>%
      pull(EIA_ID) -> valid_dams

    WECC_max_min_data %>%
      filter(EIA_ID %in% valid_dams) %>%
      # constrain capacity (in case of error)
      mutate(
        max = if_else(max > capability, capability, max),
        min = if_else(min < 0, 0, min)
      ) -> max_min_mean_obs


    max_min_mean_obs %>%
      filter(epiweek != 53) %>%
      split(.$EIA_ID) %>%
      map_dfr(function(dam){
        dam %>%
          mutate(cap_minus_mean = capability - mean,
                 pmax_minus_mean = max - mean) %>%
          filter(mean > 0) -> x

        lm_max <- lm(pmax_minus_mean ~ cap_minus_mean + 0, data = x)
        lm_min <- lm(min ~ mean + 0,data = x)

        x %>%
          add_predictions(lm_max, "max_minus_mean_pred") %>%
          add_predictions(lm_min, "min_pred") %>%
          mutate(max_pred = max_minus_mean_pred + mean,
                 max_param = lm_max[[1]] %>% unname(),
                 min_param = lm_min[[1]] %>% unname()) %>%
          select(EIA_ID, year, epiweek, mean, min, max, capability,
                 max_pred, min_pred, max_param, min_param, nameplate_HS, nameplate_EIA) %>%
          bind_rows(dam %>% filter(mean <= 0)) %>%
          arrange(year, epiweek) %>%
          fill(max_param, min_param, .direction = "downup") %>%
          mutate(max_pred = if_else(is.na(max_pred), 0, max_pred),
                 min_pred = if_else(is.na(min_pred), 0, min_pred))

      }) -> WECC_pmax_pmin_models

    return(WECC_pmax_pmin_models)
  }


}

#' get_pmax_pmin_predictions
#'
#' @description gets a prediction of pmax and pmin parameters using categorical variable in HydroSource
#' @importFrom dplyr select right_join
#' @return
#' @export
#'
get_pmax_pmin_predictions <- function(){

  get_pmax_pmin_params("WECC") %>%
    select(EIA_ID, max_param, min_param) %>%
    unique() -> max_min_params

  read_HydroSource() %>%
    select(EIA_ID, mode, bal_auth, state) %>%
    right_join(max_min_params, by = "EIA_ID") ->
  max_min_params_by_BA_MODE_STATE

  max_min_params_by_BA_MODE_STATE %>%
    group_by(bal_auth) %>%
    summarise(max_param_BA = median(max_param),
              min_param_BA = median(min_param)) ->
    param_by_BA

  max_min_params_by_BA_MODE_STATE %>%
    group_by(state) %>%
    summarise(max_param_STATE = median(max_param),
              min_param_STATE = median(min_param)) ->
    param_by_state

  max_min_params_by_BA_MODE_STATE %>%
    group_by(mode) %>%
    summarise(max_param_MODE = median(max_param),
              min_param_MODE = median(min_param)) ->
    param_by_mode

  read_HydroSource() %>%
    select(EIA_ID, mode, bal_auth, state) %>%
    left_join(max_min_params, by = "EIA_ID") %>%
    left_join(param_by_BA, by = "bal_auth") %>%
    left_join(param_by_state, by = "state") %>%
    left_join(param_by_mode, by = "mode") %>%
    mutate(max_param =
             case_when(
               is.na(max_param) & !is.na(max_param_BA) ~ max_param_BA,
               is.na(max_param) & is.na(max_param_BA) & !is.na(max_param_STATE) ~ max_param_STATE,
               is.na(max_param) & is.na(max_param_BA) & is.na(max_param_STATE) ~ max_param_MODE
             ),
           min_param =
             case_when(
               is.na(min_param) & !is.na(min_param_BA) ~ min_param_BA,
               is.na(min_param) & is.na(min_param_BA) & !is.na(min_param_STATE) ~ min_param_STATE,
               is.na(min_param) & is.na(min_param_BA) & is.na(min_param_STATE) ~ min_param_MODE
             )) %>%
    mutate(max_param = if_else(is.na(max_param), median(max_param, na.rm = T), max_param),
           min_param = if_else(is.na(min_param), median(min_param, na.rm = T), min_param)) %>%
    select(EIA_ID, max_param, min_param)
  }

