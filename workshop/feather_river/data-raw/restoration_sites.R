# set up
source(here::here("workshop", "feather_river", "globals.R"))

# create restoration sites from coordinates that Ryon Kurth shared
restoration_sites <- tibble::tribble(
  ~id, ~name,                                     ~lon,        ~lat,
  1,   "Prop 68 gravel restoration",              -121.566730, 39.514179,
  2,   "Robinson riffle side channel/floodplain", -121.600682, 39.464012,
  3,   "OWA floodplain",                          -121.631083, 39.452357,
  4,   "Star Bend levee setback floodplain",      -121.595290, 39.009252,
  5,   "Nelson Slough floodplain",                -121.614768, 38.891041
)

restoration_sites <- dplyr::mutate(
  restoration_sites,
  name = factor(name, levels = name[order(id)])
)

restoration_sites <- sf::st_as_sf(
  x = restoration_sites,
  coords = c("lon", "lat"),
  crs = global_crs
)

# write out restoration sites
save(
  restoration_sites,
  file = here::here("workshop", "feather_river", "data", "restoration_sites.rda")
)
