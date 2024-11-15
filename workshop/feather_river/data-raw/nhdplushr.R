# set up
source(here::here("workshop", "feather_river", "globals.R"))

# download NHDPlusHR spatial data for the Sacramento
nhdplushr_dir <- here::here("workshop", "feather_river", "data-raw", "nhdplushr")

if (!dir.exists(nhdplushr_dir)) {dir.create(nhdplushr_dir)}

nhdplushr_dir_path <- nhdplusTools::download_nhdplushr(
  nhd_dir = nhdplushr_dir,
  hu_list = 1802,
  download_files = TRUE
)

# read in and clean flowlines
file_paths <- list.files(nhdplushr_dir_path, full.names = TRUE)

gdb_file_path <- file_paths[stringr::str_ends(file_paths, "\\.gdb$")]

flowlines <- sf::st_read(dsn = gdb_file_path, layer = "NHDFlowline")
flowlines <- sf::st_transform(x = flowlines, crs = global_crs)
flowlines <- sf::st_zm(flowlines)
sf::st_geometry(x = flowlines) <- global_geom_name

names(flowlines) <- tolower(names(flowlines))

# read in and clean value-added attributes
vaa <- sf::st_read(dsn = gdb_file_path, layer = "NHDPlusFlowlineVAA")
names(vaa) <- tolower(names(vaa))

# make a single flowlines object enriched with value-added attributes
flowlines <- dplyr::left_join(
  x = flowlines,
  y = vaa,
  by = c("nhdplusid", "reachcode", "vpuid")
)

# clean names to work with low-level topology tools
flowlines <- dplyr::select(
  flowlines,

  id = nhdplusid,

  fromnode = fromnode,
  tonode = tonode,
  divergence = divergence,
  wbid = wbarea_permanent_identifier,

  total_da_sqkm = totdasqkm,
  da_sqkm = areasqkm,
  length_km = lengthkm,
  pathlength_km = pathlength,
  arbolate_sum = arbolatesu,

  topo_sort = hydroseq,
  up_topo_sort = uphydroseq,
  dn_topo_sort = dnhydroseq,
  dn_minor_topo_sort = dnminorhyd,

  terminal_topo_sort = terminalpa,
  terminal_flag = terminalfl,
  start_flag = startflag,

  levelpath = levelpathi,
  up_levelpath = uplevelpat,
  dn_levelpath = dnlevelpat,

  stream_level = streamleve,
  dn_stream_level = dnlevel,
  stream_order = streamorde,
  stream_calculator = streamcalc,

  feature_type = ftype,
  feature_type_code = fcode,
  vector_proc_unit = vpuid,

  aggregate_id = reachcode,
  aggregate_id_from_measure = frommeas,
  aggregate_id_to_measure = tomeas
)

flowlines <- dplyr::select(
  flowlines,

  id,

  fromnode,
  tonode,
  divergence,
  wbid,

  total_da_sqkm,
  da_sqkm,
  length_km,
  pathlength_km,
  arbolate_sum,

  topo_sort,
  up_topo_sort,
  dn_topo_sort,
  dn_minor_topo_sort,

  terminal_topo_sort,
  terminal_flag,
  start_flag,

  levelpath,
  up_levelpath,
  dn_levelpath,

  stream_level,
  dn_stream_level,
  stream_order,
  stream_calculator,

  feature_type,
  feature_type_code,
  vector_proc_unit,

  aggregate_id,
  aggregate_id_from_measure,
  aggregate_id_to_measure
)

# write out flowlines
save(
  flowlines,
  file = here::here("workshop", "feather_river", "data", "flowlines.rda")
)

# read in waterbodies
waterbodies <- sf::st_read(dsn = gdb_file_path, layer = "NHDWaterbody")
waterbodies <- sf::st_transform(x = waterbodies, crs = global_crs)
waterbodies <- sf::st_zm(x = waterbodies)
waterbodies <- sf::st_make_valid(x = waterbodies)
sf::st_geometry(x = waterbodies) <- global_geom_name

names(waterbodies) <- tolower(names(waterbodies))

waterbodies <- dplyr::select(
  waterbodies,

  id = nhdplusid,
  gnis_id = gnis_id,
  gnis_name = gnis_name,

  area_sqkm = areasqkm,

  feature_type = ftype,
  feature_type_code = fcode,
  vector_proc_unit = vpuid,

  aggregate_id = reachcode
)

# write out waterbodies
save(
  waterbodies,
  file = here::here("workshop", "feather_river", "data", "waterbodies.rda")
)
