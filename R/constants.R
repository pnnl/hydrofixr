# constants

# unit conversion
ft_to_m <- 0.3048
W_to_MW <- 1e-06
hrs_in_day <- 24


# hydropower computation
efficiency <- 0.85
specific_weight_water <- 9810 # N/m3

# USACE Columbia/Snake codes to EIA
# these are dams with hourly power generation data available via USACE data query
dam_codes <- tribble(
  ~dam, ~EIA_ID,
  "BON", 3075L,
  "CHJ", 3921L,
  "GCL", 6163L,
  "IHR", 3925L,
  "JDA", 3082L,
  "LGS", 3926L,
  "LMN", 3927L,
  "LWG", 6175L,
  "MCN", 3084L,
  "PRD", 3887L,
  "TDA", 3895L,
  "ALF", 851L,
  "BCL", 3074L,
  "CGR", 3076L,
  "DET", 3077L,
  "DEX", 3078L,
  "DWR", 840L,
  "FOS", 6552L,
  "GPR", 3080L,
  "HCR", 3081L,
  "HGH", 2203L,
  "LOP", 3083L,
  "LOS", 6174L,
  "RIS", 6200L,
  "RRH", 3883L,
  "WAN", 3888L,
  "WEL", 3886L
)
