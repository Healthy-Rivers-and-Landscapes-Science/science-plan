# set up
source(here::here("workshop", "feather_river", "globals.R"))

# specify the EDI package unique identifier
package_id <- "edi.1511.1"

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

# filter to retain Feather River data
feather_hab_flow_sbrs <- dplyr::filter(
  habitat_flow_relationships,
  watershed == "Feather River",
  scenario == "VA additional acres",
  habitat_type == "Instream rearing"
)

# retain only necessary information for ease of analysis
sbrs_flow_area <- dplyr::select(
  feather_hab_flow_sbrs,
  flow_cfs,
  habitat_area_acres
)

# write out habitat-flow area data
save(
  sbrs_flow_area,
  file = here::here("workshop", "feather_river", "data", "sbrs_flow_area.rda")
)
