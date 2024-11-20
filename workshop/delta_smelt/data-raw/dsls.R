# set up
source(here::here("workshop", "delta_smelt", "globals.R"))
source(here::here("workshop", "delta_smelt", "functions", "import_mdb_tables.R"))
source(here::here("workshop", "delta_smelt", "functions", "get_mdb_table_names.R"))

# download DSLS from CDFW
url <- "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/DSLS.mdb"

mdb_file <- here::here("workshop", "delta_smelt", "data-raw", "dsls.mdb")

download.file(
  url = url,
  destfile = mdb_file
)

# read in data
table_names <- get_mdb_table_names(mdb_file = mdb_file)
import_mdb_tables(mdb_file = mdb_file, table_names = table_names)

# clean data
colnames(dsls_stations) <- snakecase::to_snake_case(
  colnames(dsls_stations),
  parsing_option = 1
)

dsls_stations <- dplyr::filter(
  dsls_stations,
  !stringr::str_detect(string = notes, pattern = "Only sampled once")
)

# make data spatial
dsls_stations <- dplyr::mutate(
  dsls_stations,
  lat = lat_d + lat_m / 60 + lat_s / 3600,
  lon = -(lon_d + lon_m / 60 + lon_s / 3600)
)

dsls_stations <- sf::st_as_sf(
  dsls_stations,
  coords = c("lon", "lat"),
  crs = global_crs
)

# write out data
save(
  dsls_stations,
  file = here::here("workshop", "delta_smelt", "data", "dsls_stations.rda")
)
