# set up
source(here::here("workshop", "feather_river", "globals.R"))
source(here::here("workshop", "feather_river", "functions", "get_clean_cdec_flow.R"))

# specify start and end dates; two of the stations don't have data before 2021,
# so going with a time period where all stations have essentially complete data
start_date <- "2021-10-01"
end_date <- "2024-09-30"

# pull CDEC data for gages on the Feather River
tfb_flows <- get_clean_cdec_flow(
  station = "TFB",
  sensor = 41, # daily discharge (CFS)
  dur_code = "D", # daily timestep
  start_date = start_date,
  end_date = end_date
)

grl_flows <- get_clean_cdec_flow(
  station = "GRL",
  sensor = 41, # daily discharge (CFS)
  dur_code = "D", # daily timestep
  start_date = start_date,
  end_date = end_date
)

fsb_flows <- get_clean_cdec_flow(
  station = "FSB",
  sensor = 20, # hourly discharge (CFS) - daily isn't available
  dur_code = "H", # hourly timestep
  start_date = start_date,
  end_date = end_date
)

# convert FSB hourly data to daily means
fsb_flows <- fsb_flows |>
  dplyr::group_by(datetime) |>
  dplyr::mutate(parameter_value = mean(parameter_value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  unique()

# create a single flows object
flows <- rbind(tfb_flows, grl_flows, fsb_flows)

flows <- dplyr::mutate(
  flows,
  location_id = factor(location_id, levels = c("TFB", "GRL", "FSB")),
  water_year = factor(water_year)
)

# write out flows
save(flows, file = here::here("workshop", "feather_river", "data", "flows.rda"))
