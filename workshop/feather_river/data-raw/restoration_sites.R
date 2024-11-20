# set up
source(here::here("workshop", "feather_river", "globals.R"))

# create restoration sites from coordinates that Ryon Kurth shared
restoration_sites <- tibble::tribble(
  ~id, ~name,                                     ~lon,        ~lat,
  1,   "Robinson riffle side channel/floodplain", -121.590682, 39.467000,
  2,   "OWA floodplain",                          -121.631083, 39.452357,
  3,   "Star Bend levee setback floodplain",      -121.595290, 39.009252,
  4,   "Nelson Slough floodplain",                -121.614768, 38.891041
)

restoration_sites <- dplyr::mutate(
  restoration_sites,
  name = factor(name, levels = name[order(id)])
)

restoration_sites <- sf::st_as_sf(
  x = restoration_sites,
  coords = c("lon", "lat"),
  crs = global_crs,
  dim = "XY"
)

# write out restoration sites
save(
  restoration_sites,
  file = here::here("workshop", "feather_river", "data", "restoration_sites.rda")
)
