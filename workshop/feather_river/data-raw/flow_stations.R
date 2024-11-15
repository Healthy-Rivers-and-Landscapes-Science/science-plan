# set up
source(here::here("workshop", "feather_river", "globals.R"))

# create flow stations
flow_stations <- tibble::tribble(
  ~location_id, ~name,                     ~lon,        ~lat,
  "TFB",        "Thermalito Fish Barrier", -121.547896, 39.520428,
  "GRL",        "Gridley",                 -121.647369, 39.366577,
  "FSB",        "Star Bend",               -121.610435, 39.045498
)

flow_stations <- dplyr::mutate(
  flow_stations,
  location_id = factor(location_id, levels = c("TFB", "GRL", "FSB"))
)

flow_stations <- sf::st_as_sf(
  x = flow_stations,
  coords = c("lon", "lat"),
  crs = global_crs,
  dim = "XY"
)

# write out flow stations
save(
  flow_stations,
  file = here::here("workshop", "feather_river", "data", "flow_stations.rda")
)
