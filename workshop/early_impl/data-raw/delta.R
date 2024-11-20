# set up
source(here::here("workshop", "early_impl", "globals.R"))

# download simple spatial data capturing the Delta
url <- paste0(
  "https://geowebservices.stanford.edu/geoserver/wfs?",
  "service=WFS&version=1.0.0&request=GetFeature&typeName=",
  "druid:qh320kj0191",
  "&outputFormat=application/json"
)

delta <- sf::st_read(dsn = url)
delta <- sf::st_transform(x = delta, crs = global_crs)
delta <- sf::st_make_valid(x = delta)
delta <- dplyr::filter(delta, acres > 20000)

# write out the Delta
save(delta, file = here::here("workshop", "early_impl", "data", "delta.rda"))

