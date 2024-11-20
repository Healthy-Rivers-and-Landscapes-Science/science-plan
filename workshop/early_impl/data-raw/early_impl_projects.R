# set up
source(here::here("workshop", "early_impl", "globals.R"))

# create a dataframe of early implementation projects
early_impl_projects_all <- tibble::tribble(
  ~code,  ~id,                            ~system,     ~lat,      ~lon,
  "1-1",  "Upper River Bend",             "American",   38.606479, -121.310875,
  "1-2",  "Ancil Hoffman",                "American",   38.616670, -121.304996,
  "1-3",  "Lower Sailor Bar",             "American",   38.635904, -121.248750,
  "1-4",  "Upper Sailor Bar",             "American",   38.634169, -121.230126,
  "2-1",  "River Bend",                   "American",   NA_real_,  NA_real_,
  "2-2",  "Nimbus Basin",                 "American",   38.636258, -121.222588,
  "2-3",  "Lower Elkhorn Basin",          "Delta",      38.633998, -121.603412,
  "2-4",  "Tide's End",                   "Delta",      38.407119, -121.620514,
  "2-5",  "Prospect Island",              "Delta",      38.269628, -121.654187,
  "2-6",  "McCormack-Williamson Tract",   "Delta",      38.263686, -121.477615,
  "2-7",  "Dutch Slough",                 "Delta",      38.000023, -121.650162,
  "3-1",  "Little Egbert Tract",          "Delta",      38.208293, -121.676653,
  "3-2",  "Grizzly Slough",               "Delta",      38.245629, -121.402508,
  "3-3",  "Gravel Supplementation",       "Feather",    NA_real_,  NA_real_,
  "3-4",  "Garden Highway",               "Feather",    38.955981, -121.583870,
  "3-5",  "Sunset Pumps",                 "Feather",    39.247669, -121.636453,
  "3-6",  "Nelson Slough",                "Feather",    38.909144, -121.606107,
  "3-7",  "Gravel Enhancement",           "Mokelumne",  38.221703, -121.036962,
  "3-8",  "Diversion Screens",            "Mokelumne",  38.211233, -121.354889,
  "4-1",  "Floodplain",                   "Mokelumne",  NA_real_, NA_real_,
  "4-2",  "Spawning and Rearing Habitat", "Mokelumne",  38.222987, -121.034954,
  "4-3",  "Spawning Habitat",             "Putah",      NA_real_, NA_real_,
  "4-4",  "Keswick",                      "Sacramento", 40.608937, -122.445967,
  "4-5",  "Middle Creek",                 "Sacramento", 40.596874, -122.439219,
  "4-6",  "Market Street",                "Sacramento", 40.592055, -122.392155,
  "4-7",  "Redding Riffle",               "Sacramento", 40.591403, -122.381190,
  "4-8",  "Turtle Bay",                   "Sacramento", 40.589755, -122.365369,
  "4-9",  "Olney Creek",                  "Sacramento", NA_real_, NA_real_,
  "4-10", "Bonneyview Ranch Island",      "Sacramento", NA_real_, NA_real_,
  "5-1",  "Kapusta Island",               "Sacramento", 40.496223, -122.334251,
  "5-2",  "Kapusta Island 1B",            "Sacramento", 40.497671, -122.343743,
  "5-3",  "Battle Creek",                 "Sacramento", NA_real_, NA_real_,
  "5-4",  "Elks Lodge",                   "Sacramento", 40.170498, -122.225654,
  "5-5",  "South Sand Slough",            "Sacramento", 40.155135, -122.208218,
  "5-6",  "Blackberry Island",            "Sacramento", 40.141826, -122.144809,
  "5-7",  "McClure Creek",                "Sacramento", 39.992576, -122.112492,
  "5-8",  "Indian Fishery",               "Sacramento", 39.728841, -121.951818,
  "5-9",  "Willow Bend",                  "Sacramento", 39.349725, -122.013420,
  "6-1",  "La Grange Bridge",             "Tuolumne",   37.665846, -120.460516,
  "6-2",  "Hallwood Side Channel",        "Yuba",       39.201807, -121.458588,
  "6-3",  "Lower Long Bar",               "Yuba",       39.221514, -121.386381,
  "6-4",  "Upper Rose Bar",               "Yuba",       39.219266, -121.300318
)

early_impl_projects_all$system <- factor(
  early_impl_projects_all$system,
  levels = c("Sacramento", "Feather", "Yuba", "Delta", "American", "Mokelumne",
             "Tuolumne")
)

early_impl_projects_sf <- sf::st_as_sf(
  x = dplyr::filter(early_impl_projects_all, !is.na(lat)),
  coords = c("lon", "lat"),
  crs = global_crs,
  dim = "XY"
)

# write out
save(
  early_impl_projects_all,
  file = here::here("workshop", "early_impl", "data", "early_impl_projects_all.rda")
)

save(
  early_impl_projects_sf,
  file = here::here("workshop", "early_impl", "data", "early_impl_projects_sf.rda")
)
