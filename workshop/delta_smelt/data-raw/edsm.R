# set up
source(here::here("workshop", "delta_smelt", "globals.R"))

# specify the EDI package unique identifier
package_id <- "edi.415.11"

# grab entity names and convert to snakecase for later name assignment
entity_names <- EDIutils::read_data_entity_names(packageId = package_id)
entity_names$entityName <- snakecase::to_any_case(
  entity_names$entityName,
  case = "snake"
)

# import all entities associated with EDI package and assign to names (in case
# we are curious about these objects later)
for (i in seq_len(nrow(entity_names))) {
  entity_id <- entity_names$entityId[i]
  entity_name <- entity_names$entityName[i]

  data_entity <- EDIutils::read_data_entity(
    packageId = package_id,
    entityId = entity_id
  )

  data_read <- readr::read_csv(file = data_entity)

  assign(x = entity_name, value = data_read)
}

# clean data
colnames(edsm_kdtr_csv) <- snakecase::to_snake_case(colnames(edsm_kdtr_csv))

edsm_sample_locations <- dplyr::select(
  edsm_kdtr_csv,
  station_code,
  longitude,
  latitude
)

edsm_sample_locations <- unique(edsm_sample_locations)

# make data spatial
edsm_sample_locations <- sf::st_as_sf(
  edsm_sample_locations,
  coords = c("longitude", "latitude"),
  crs = global_crs
)

# write out data
save(
  edsm_sample_locations,
  file = here::here("workshop", "delta_smelt", "data", "edsm_sample_locations.eda")
)
