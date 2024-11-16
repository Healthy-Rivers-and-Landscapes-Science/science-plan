# set up
source(here::here("workshop", "habs", "globals.R"))

load(here::here("workshop", "habs", "data", "emp_stations.rda"))
load(here::here("workshop", "habs", "data", "microcystis.rda"))

# specify EMP DWQ monitoring stations in the POI
poi_existing_stations_list <- tibble::tribble(
  ~station, ~description,
  "C3A",    "Sacramento River @ Hood",
  "C9",     "mouth of Clifton Court Forebay Intake",
  "C10",    "San Joaquin River near Vernalis",
  "C13",    "Mokelumne River @ Terminus",
  "D4",     "Sacramento River above Point Sacramento",
  "D6",     "Suisun Bay @ Bulls Head Point near Martinez",
  "D7",     "Grizzly Bay @ Dolphin near Suisun Slough",
  "D8",     "Suisun Bay off Middle Point near Nichols",
  "D9",     "Honker Bay near Wheeler Point",
  "D10",    "Sacramento River @ Chipps Island",
  "D11",    "Sherman Island near Antioch",
  "D12",    "San Joaquin River @ Antioch Ship Canal",
  "D16",    "San Joaquin River @ Twitchell Island",
  "D19",    "Frank's Tract near Russo's Landing",
  "D24",    "Sacramento River below Rio Vista Bridge",
  "D26",    "San Joaquin River @ Potato Point",
  "D28A",   "Old River near Rancho Del Rio",
  "D29",    "San Joaquin River @ Prisoners Point",
  "D41",    "San Pablo Bay near Pinole Point",
  "D41A",   "San Pablo Bay near mouth of Petaluma River",
  "P8",     "San Joaquin River @ Buckley Cove",
  "MD10",   "Disappointment Slough near Bishop Cut",
  "S42",    "Suisun Slough 300' south of Volanti Slough"
)

poi_existing_stations <- dplyr::filter(
  emp_stations,
  station %in% poi_existing_stations_list$station
)

poi_existing_stations <- dplyr::mutate(
  poi_existing_stations,
  station_type = "existing"
)

# specify new monitoring stations proposed by the POI
poi_new_stations <- tibble::tribble(
  ~station, ~location,                                                      ~lat,     ~lon,
  "new_1",  "San Joaquin River (Turner Cut to Stockton) (RSAN050-RSAN061)", 37.97494, -121.38092,
  "new_2",  "Frank's Tract eastern/southern side",                          38.04521, -121.58660,
  "new_3",  "McLeod Lake at Stockton Waterfront",                           37.95372, -121.2967,
  "new_4",  "Victoria Canal near Byron",                                    37.87170, -121.52830,
  "new_5",  "Big Break Regional Shoreline at San Joaquin River",            38.01460, -121.728952,
  "new_6",  "Middle River at Ski Beach",                                    37.91561, -121.51416,
  "new_7",  "Mildred Island",                                               37.98280, -121.51980
)

poi_new_stations <- dplyr::mutate(
  poi_new_stations,
  station_type = "new"
)

poi_new_stations <- sf::st_as_sf(
  x = poi_new_stations,
  coords = c("lon", "lat"),
  crs = global_crs,
  dim = "XY"
)

# create a single POI stations object
poi_stations <- dplyr::bind_rows(
  dplyr::select(poi_existing_stations, station, location, station_type),
  dplyr::select(poi_new_stations, station, location, station_type)
)

poi_stations <- dplyr::mutate(
  poi_stations,
  new_microcystis_sampling = !station %in% microcystis$station
)

# write out stations
save(
  poi_stations,
  file = here::here("workshop", "habs", "data", "poi_stations.rda")
)
