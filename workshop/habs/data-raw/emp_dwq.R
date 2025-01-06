# set up
source(here::here("workshop", "habs", "globals.R"))

# pull Environmental Monitoring Program Discrete Water Quality data from EDI
package_id <- "edi.458.12"

entities <- EDIutils::read_data_entity_names(packageId = package_id)
entities$entityName <- snakecase::to_snake_case(entities$entityName)
entities <- entities[!grepl("metadata", entities$entityName), ]

for (i in seq_len(nrow(entities))) {
  entity_id <- entities$entityId[i]
  entity_name <- entities$entityName[i]

  entity <- EDIutils::read_data_entity(
    packageId = package_id,
    entityId = entity_id
  )

  dat <- readr::read_csv(file = entity)

  assign(x = entity_name, value = dat)
}

# clean data
colnames(emp_dwq_stations_1975_2023) <- snakecase::to_snake_case(
  string = colnames(emp_dwq_stations_1975_2023),
  parsing_option = 1
)

colnames(emp_dwq_1975_2023) <- snakecase::to_snake_case(
  string = colnames(emp_dwq_1975_2023),
  parsing_option = 1
)

emp_dwq_1975_2023 <- dplyr::select(
  emp_dwq_1975_2023,
  station,
  date,
  microcystis
)

microcystis_all <- dplyr::filter(
  emp_dwq_1975_2023,
  !is.na(microcystis),
  date >= as.Date("2015-01-01")
)

# fix incorrect values, as per communication with Ted Flynn
microcystis_all <- dplyr::mutate(
  microcystis_all,
  microcystis = dplyr::if_else(station %in% c("D41", "D41A"), 1, microcystis)
)

# add columns needed for analysis
microcystis_all <- dplyr::mutate(
  microcystis_all,
  severity_cat = dplyr::case_when(
    microcystis >= 4 ~ "high",
    microcystis >= 2 ~ "low",
    microcystis == 1 ~ "absent"
  ) |>
    factor(levels = c("absent", "low", "high")),
  year = lubridate::year(date),
  month = lubridate::month(date),
  season = dplyr::case_when(
    month %in% c(12, 1, 2, 3, 4) ~ "winter",
    month %in% c(5, 6, 7, 8, 9, 10, 11) ~ "summer",
    TRUE ~ NA_character_
  )
)

# summarize sites by season
microcystis_summary <- microcystis_all |>
  dplyr::group_by(station, season) |>
  dplyr::summarize(
    microcystis_90th = {
      values <- na.omit(microcystis) |> sort()
      values[ceiling(0.9 * length(values))]
    },
    microcystis_max = max(microcystis, na.rm = TRUE),
    .groups = "drop"
  )

# create factors needed for visualization
microcystis_summary <- dplyr::mutate(
  microcystis_summary,
  season = factor(season, levels = c("winter", "summer")),
  microcystis_90th_bin = dplyr::case_when(
    microcystis_90th >= 4 ~ "high",
    microcystis_90th >= 2 ~ "low",
    microcystis_90th == 1 ~ "absent"
  ) |>
    factor(levels = c("absent", "low", "high")),
  microcystis_max_bin = dplyr::case_when(
    microcystis_max >= 4 ~ "high",
    microcystis_max >= 2 ~ "low",
    microcystis_max == 1 ~ "absent"
  ) |>
    factor(levels = c("absent", "low", "high"))
)

# add coordinates and convert microcystis dataframes to sf objects
microcystis_all <- dplyr::left_join(
  x = microcystis_all,
  y = dplyr::select(emp_dwq_stations_1975_2023, station, latitude, longitude),
  by = "station"
)

microcystis_all <- dplyr::filter(microcystis_all, !is.na(latitude))

microcystis_all <- sf::st_as_sf(
  x = microcystis_all,
  coords = c("longitude", "latitude"),
  crs = global_crs
)

microcystis_summary <- dplyr::left_join(
  x = microcystis_summary,
  y = dplyr::select(emp_dwq_stations_1975_2023, station, latitude, longitude),
  by = "station"
)

microcystis_summary <- dplyr::filter(microcystis_summary, !is.na(latitude))

microcystis_summary <- sf::st_as_sf(
  x = microcystis_summary,
  coords = c("longitude", "latitude"),
  crs = global_crs
)

# create a stations spatial object
emp_stations <- dplyr::filter(
  emp_dwq_stations_1975_2023,
  !is.na(latitude),
  !is.na(longitude)
)

emp_stations <- sf::st_as_sf(
  x = emp_stations,
  coords = c("longitude", "latitude"),
  crs = global_crs,
  dim = "XY"
)

# save all microcystis data
save(
  microcystis_all,
  file = here::here("workshop", "habs", "data", "microcystis_all.rda")
)

# save microcystis summary data
save(
  microcystis_summary,
  file = here::here("workshop", "habs", "data", "microcystis_summary.rda")
)

# save stations data
save(
  emp_stations,
  file = here::here("workshop", "habs", "data", "emp_stations.rda")
)

