# gridview manual join helper

gridview_to_NHD_join_helper <- tibble::tribble(
  ~GeneratorName, ~EIA_ID, ~EHA_ID,
  "B Hydro Aggregate",   NA_integer_, NA_character_,
  "Borden-QFAgg",        NA_integer_, NA_character_,
  "CEDR FL+_31850_1",    NA_integer_, NA_character_,
  "CEDR FL+_31850_2",    NA_integer_, NA_character_,
  "CloverCreek",         NA_integer_, "hc7114_p01",
  "Crane_Valley",        230, "hc1096_p01",
  "Division Creek",      395, "hc4014_p01",
  "ECKERT",              NA_integer_, "hc7176_p01",
  "Graegl69_38134_1",    NA_integer_, NA_character_,
  "Kokish_River_4",      NA_integer_, NA_character_,
  "KR3-1_24372_1",       NA_integer_, NA_character_,
  "KR3-1_24373_2",       NA_integer_, NA_character_,
  "LakeCreek_BPA1",      NA_integer_, "hc7190_p01",
  "LAKEGEN_29008_1",     NA_integer_, NA_character_,
  "LAKESISK",            50179, "hc1707_p01",
  "Lewiston_1",          977, "hc0027_p02",
  "LowLineMidway1",      NA_integer_, "hc7489_p01",
  "McCall_QF",           NA_integer_, NA_character_,
  "NEW_0_70535_H1",      NA_integer_, NA_character_,
  "NEW_QUINCY_70343_H1", NA_integer_, NA_character_,
  "NEW_SPFSHPRK_76502_1",NA_integer_, NA_character_,
  "NEW_SULLIVN_70417_H1",NA_integer_, NA_character_,
  "NEW_VICTORY_70510_S1",NA_integer_, NA_character_,
  "Ridgeway 1",          NA_integer_, NA_character_,
  "Ridgeway 2",          NA_integer_, NA_character_,
  "ROARING_31848_2",     NA_integer_, NA_character_,
  "ROARMONG_31847_1",    NA_integer_, NA_character_,
  "Roberts Tunnel Hydro",NA_integer_, NA_character_,
  "SHELTON",             NA_integer_, NA_character_,
  "WestSide",            NA_integer_, NA_character_)


# generate hours per month
gen_hrs_per_month <- function(){
  tibble(date = seq(ISOdate(2001,1,1), to = ISOdate(2021,12,31), by = "day")) %>%
    mutate(date = lubridate::date(date),
           year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(year, month) %>% summarise(n_days = n()) %>%
    ungroup() %>% mutate(n_hours = n_days * 24) %>%
    left_join(tibble(month = 1:12, month_abb = month.abb), by = "month") %>%
    mutate(month = month_abb) %>%
    select(year, month, n_hours)
}

n_hours <- gen_hrs_per_month()


