# set up
source(here::here("workshop", "habs", "globals.R"))

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
delta <- dplyr::filter(delta, acres > 100)

# create a polygon to crop the Delta
coords <- matrix(
  data = c(
    -122.41000, 37.67575,
    -122.41000, 38.65000,
    -121.26470, 38.65000,
    -121.26470, 37.67575,
    -122.41000, 37.67575
  ),
  ncol = 2,
  byrow = TRUE
)

crop_polygon <- sf::st_sfc(x = sf::st_polygon(list(coords)), crs = global_crs)

# crop the Delta
delta <- sf::st_intersection(x = delta, y = crop_polygon)

# write out the Delta
save(delta, file = here::here("workshop", "delta_smelt", "data", "delta.rda"))

