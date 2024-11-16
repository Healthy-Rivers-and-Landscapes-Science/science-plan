# set up
library(ggplot2)
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
colnames(emp_dwq_1975_2023) <- snakecase::to_snake_case(
  string = colnames(emp_dwq_1975_2023),
  parsing_option = 1
)

colnames(emp_dwq_stations_1975_2023) <- snakecase::to_snake_case(
  string = colnames(emp_dwq_stations_1975_2023),
  parsing_option = 1
)

emp_dwq_1975_2023 <- dplyr::select(
  emp_dwq_1975_2023,
  station,
  date,
  microcystis
)

emp_dwq_1975_2023 <- dplyr::filter(emp_dwq_1975_2023, !is.na(microcystis))

# add columns needed for analysis
emp_dwq_1975_2023 <- dplyr::mutate(
  emp_dwq_1975_2023,
  year = lubridate::year(date),
  month = lubridate::month(date),
  season = dplyr::case_when(
    month %in% c(11, 12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5, 6, 7, 8, 9, 10) ~ "summer",
    TRUE ~ NA_character_
  )
)

# summarize sites by season
microcystis_summary <- emp_dwq_1975_2023 |>
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

# add coordinates and convert `microcystis_summary` to an `sf` object
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

# save `microcystis_summary`
save(
  microcystis_summary,
  file = here::here("workshop", "habs", "data", "microcystis_summary.rda")
)


