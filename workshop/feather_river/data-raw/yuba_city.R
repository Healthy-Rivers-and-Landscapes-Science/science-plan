# set up
source(here::here("workshop", "feather_river", "globals.R"))

# create Yuba City point
yuba_city <- tibble::tribble(
  ~city,       ~label,       ~lon,        ~lat,
  "Yuba City", "Yuba\nCity", -121.617730, 39.140718
)

yuba_city <- sf::st_as_sf(
  x = yuba_city,
  coords = c("lon", "lat"),
  crs = global_crs
)

# write out Yuba City
save(
  yuba_city,
  file = here::here("workshop", "feather_river", "data", "yuba_city.rda")
)
