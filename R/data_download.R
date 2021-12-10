#' dl_usace_generation
#'
#' @description downloads hourly data from USACE Data Query and performs mean, min, max summarizing before updating relevant package files
#' @param save_to character where to save data ("library" will update package library, NULL will assume package editing)
#' @importFrom dplyr select mutate filter pull group_by summarise ungroup
#' @importFrom vroom vroom cols
#' @importFrom purrr map_dfr
#' @importFrom tidyr gather
#' @importFrom lubridate dmy_hm year month epiweek
#' @importFrom readr write_csv
#' @return two csvs of p_min p_max p_mean for monthly and weekly periods
#' @export

dl_usace_generation <- function(save_to = "library"){

  url <- "http://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
  units_power <- "%3Aunits%3DMW&headers=true&filename=&"
  period <- "timezone=PST&lookback=7d&startdate=01%2F01%2F2011+07%3A00&enddate=01%2F01%2F2020+08%3A00"

  dam_codes %>%
    pull(dam) %>%
    map_dfr(function(dam){

      data_type <- "CBT-RAW"

      if(dam %in% c("BCL", "CGR", "DET",
                    "DEX", "FOS", "GPR",
                    "LOS", "HCR", "HGH",
                    "LOP", "LOS")) data_type <- "CBT-REV"

      message(paste0("Downloading data for dam code ", dam))

      vroom(paste0(url, dam,
                   ".Power.Total.1Hour.1Hour.", data_type,
                   units_power, period), col_types = cols()) %>%
        mutate(`Date Time` = dmy_hm(`Date Time`)) -> raw_data

      names(raw_data) <- c("date_time", "power")
      raw_data %>%
        mutate(year = year(date_time),
               month = month(date_time),
               week = epiweek(date_time),
               dam = !!dam)
    }) -> raw_data_all_dams

  # monthly parameters
  raw_data_all_dams %>%
    filter(year %in% 2011:2019) %>%
    group_by(dam, year, month) %>%
    summarise(mean = mean(power),
              max = max(power),
              min = min(power)) %>%
    ungroup() %>%
    select(dam, year, month, mean, max, min) ->
    parameters_monthly

  # weekly parameters
  raw_data_all_dams %>%
    filter(year %in% 2011:2019) %>%
    group_by(dam, year, week) %>%
    summarise(mean = mean(power),
              max = max(power),
              min = min(power)) %>%
    ungroup() %>%
    select(dam, year, week, mean, max, min) ->
    parameters_weekly

  message("Writing monthly and weekly p_min, p_mean, p_max to package data...")

  if(save_to == "library"){
    write_csv(parameters_monthly,
              paste0(system.file("extdata/", package = "hydrofixr"),
                     "obs_gen_parameters_Columbia_Snake_monthly.csv")
    )

    write_csv(parameters_weekly,
              paste0(system.file("extdata/", package = "hydrofixr"),
                     "obs_gen_parameters_Columbia_Snake_weekly.csv")
    )
  }else{
    write_csv(parameters_monthly,
              "inst/extdata/obs_gen_parameters_Columbia_Snake_monthly.csv"
    )

    write_csv(parameters_weekly,
              "inst/extdata/obs_gen_parameters_Columbia_Snake_weekly.csv"
              )
  }

  message("Done!")

}





