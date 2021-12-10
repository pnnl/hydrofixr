# snap_to_MOSART_grid
#
snap_to_MOSART_grid <- function(x){

  seq(-180 + 0.125 / 2, 180 - 0.125 / 2, 0.125) -> lon_seq
  seq(-90 + 0.125 / 2, 90 - 0.125 / 2, 0.125) -> lat_seq

  x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(lon_ = lon_seq[which.min(abs(lon - lon_seq))],
                 lat_ = lat_seq[which.min(abs(lat - lat_seq))]) %>%
    dplyr::ungroup()

}


#' compute_violations
#'
#' @description computes violations
#' @import dplyr
#' @return time series with violations and other plant detail
#'
compute_violations <- function(x){



  if(length(x$head_m) == 0){
    message(paste0(x$plant, "... no head estimate"))
    return(tibble())
  }


  message(x$plant)

  left_join(x$WM_release, x$PCM_schedule,
            by = "date") %>%
    mutate(
      power_W = efficiency * release_cumecs * specific_weight_water * x$head_m,
      power_MW = power_W * W_to_MW,
      energy_MWh = power_MW * hrs_in_day,
      violation_MWh = if_else(energy_MWh < MWh, MWh - energy_MWh, 0),
      plant = x$plant,
      EHA_ID = x$EHA_ID,
      EIA_ID = x$EIA_ID,
      state = x$state,
      county = x$county,
      mode = x$mode,
      ) %>%
    #ggplot(aes(date, MWh)) + geom_line() + geom_line(aes(y = energy_MWh), col = "red")
    rename(PCM_energy_MWh = MWh, WM_energy_MWh = energy_MWh) %>%
    select(-power_MW, -power_W)

}



#' correct_lat_lon_for_MOSART_stream_network
#'
#' @description Uses updated lat/lon data (provided by Tian Zhou) to correct snapped lat-lon; ensures correct positioning of RoR dams on stream network
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom purrr map_dfr
#' @return time series with violations and other plant detail
#'
correct_lat_lon_for_MOSART_stream_network <- function(x){

  dir_ <- system.file("extdata/Plant_stream_network_corrections/", package = "hydrofixr")

  c("BPA", "SEPA", "SWPA", "WAPA") %>%
    map_dfr(function(region){
      vroom(paste0(dir_, region, ".csv"))
    }
    ) %>%
    select(EIA_ID = ID, lat_1_8_, lon_1_8_) ->
    lat_lon_corrections

  x %>%
    left_join(lat_lon_corrections,
              by = "EIA_ID") %>%
    mutate(lat_ = if_else(!is.na(lat_1_8_) & lat_ != lat,
                          lat_1_8_, lat_),
           lon_ = if_else(!is.na(lon_1_8_) & lon_ != lon,
                          lon_1_8_, lon_)) %>%
    select(-lon_1_8_, -lat_1_8_)
}


#' append_capabilities
#'
#' @description append EIA summer and winter capabilities to HydroSource
#' @param x dataset to append capabilties to
#' @param resolution if "weekly" then monthly capabilities will be converted to epiweek before join
#' @importFrom vroom vroom cols
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
append_capabilities <- function(x, resolution, data_dir){

  if(resolution != "weekly"){
    return(
      x %>%
        left_join(read_EIA_capabilities(data_dir = data_dir),
                  by = c("EIA_ID", "month"))
    )
  }else{
    x %>%
      left_join(
        gen_month_to_epiweek() %>%
          left_join(
            read_EIA_capabilities(data_dir = data_dir),
            by = "month"
          ) %>% select(EIA_ID, epiweek, nameplate, capability),
        by = c("EIA_ID", "epiweek")
      )
  }
}


#' gen_month_to_epiweek
#'
#' @description convert month to epiweek (approximate)
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when
#' @return table of month to epiweek conversion
#'
gen_month_to_epiweek <- function(){
  tibble(epiweek = 1:52) %>%
    mutate(month = case_when(
      epiweek %in% 1:4 ~ "Jan",
      epiweek %in% 5:8 ~ "Feb",
      epiweek %in% 9:13 ~ "Mar",
      epiweek %in% 14:17 ~ "Apr",
      epiweek %in% 18:22 ~ "May",
      epiweek %in% 23:26 ~ "Jun",
      epiweek %in% 27:30 ~ "Jul",
      epiweek %in% 31:35 ~ "Aug",
      epiweek %in% 36:39 ~ "Sep",
      epiweek %in% 40:44 ~ "Oct",
      epiweek %in% 45:48 ~ "Nov",
      epiweek %in% 49:52 ~ "Dec"
    ),
    month = factor(month, levels = month.abb))
}


