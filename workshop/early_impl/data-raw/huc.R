# set up
source(here::here("workshop", "early_impl", "globals.R"))

# download watersheds at a variety of different levels of organization
huc4_ids <- c("1802", "1804")

huc4 <- nhdplusTools::get_huc(
  id = huc4_ids,
  t_srs = global_crs,
  type = "huc04"
)

huc4_dissolved <- sf::st_union(huc4)

huc6 <- nhdplusTools::get_huc(
  AOI = huc4_dissolved,
  t_srs = global_crs,
  type = "huc06"
)

huc6 <- dplyr::filter(huc6, substr(huc6, 1, 4) %in% huc4_ids)

huc8 <- nhdplusTools::get_huc(
  AOI = huc4_dissolved,
  t_srs = global_crs,
  type = "huc08"
)

huc8 <- dplyr::filter(huc8, substr(huc8, 1, 4) %in% huc4_ids)

# write out watersheds
save(
  huc4,
  file = here::here("workshop", "early_impl", "data", "huc4.rda")
)

save(
  huc6,
  file = here::here("workshop", "early_impl", "data", "huc6.rda")
)

save(
  huc8,
  file = here::here("workshop", "early_impl", "data", "huc8.rda")
)
