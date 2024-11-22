# set up
source(here::here("workshop", "early_impl", "globals.R"))
load(here::here("workshop", "early_impl", "data", "delta.rda"))
load(here::here("workshop", "early_impl", "data", "flowlines.rda"))
load(here::here("workshop", "early_impl", "data", "waterbodies.rda"))

# load California
california <- dplyr::filter(tidycensus::state_laea, GEOID == "06")
california <- sf::st_transform(x = california, crs = global_crs)
colnames(california) <- snakecase::to_snake_case(colnames(california))

# create a polygon to use for cropping
delta_bbox <- sf::st_as_sfc(x = sf::st_bbox(delta), crs = global_crs)
flowlines_bbox <- sf::st_as_sfc(x = sf::st_bbox(flowlines), crs = global_crs)
waterbodies_bbox <- sf::st_as_sfc(x = sf::st_bbox(waterbodies), crs = global_crs)

xmin <- min(
  sf::st_bbox(delta)["xmin"],
  sf::st_bbox(flowlines)["xmin"],
  sf::st_bbox(waterbodies)["xmin"]
) - 0.3

ymin <- min(
  sf::st_bbox(delta)["ymin"],
  sf::st_bbox(flowlines)["ymin"],
  sf::st_bbox(waterbodies)["ymin"]
) - 0.3

xmax <- max(
  sf::st_bbox(delta)["xmax"],
  sf::st_bbox(flowlines)["xmax"],
  sf::st_bbox(waterbodies)["xmax"]
) + 0.3

ymax <- max(
  sf::st_bbox(delta)["ymax"],
  sf::st_bbox(flowlines)["ymax"],
  sf::st_bbox(waterbodies)["ymax"]
) + 0.3

bbox_polygon <- sf::st_polygon(list(rbind(
  c(xmin, ymin),
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
)))

bbox_polygon <- sf::st_sf(sf::st_sfc(x = bbox_polygon, crs = global_crs))

# crop
california_cropped <- sf::st_crop(x = california, y = bbox_polygon)

# write out
save(
  california_cropped,
  file = here::here("workshop", "early_impl", "data", "california_cropped.rda")
)
