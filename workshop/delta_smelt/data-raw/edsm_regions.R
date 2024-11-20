# set up
source(here::here("workshop", "delta_smelt", "globals.R"))

# grab the EDSM regions polygons for surveys from 2019 onward and clean
edsm_regions <- sf::st_transform(
  x = deltamapr::R_EDSM_Regions_1819P1,
  crs = global_crs
)

colnames(edsm_regions) <- tolower(colnames(edsm_regions))

edsm_regions <- dplyr::mutate(
  edsm_regions,
  region = tolower(region)
)

# write out EDSM regions
save(
  edsm_regions,
  file = here::here("workshop", "delta_smelt", "data", "edsm_regions.rda")
)
