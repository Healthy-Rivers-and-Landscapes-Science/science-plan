# set up
source(here::here("workshop", "early_impl", "globals.R"))

# download NHDPlusHR spatial data
nhdplushr_dir <- here::here("workshop", "early_impl", "data-raw", "nhdplushr")

if (!dir.exists(nhdplushr_dir)) {dir.create(nhdplushr_dir)}

nhdplushr_dir_path <- nhdplusTools::download_nhdplushr(
  nhd_dir = nhdplushr_dir,
  hu_list = c(1802, 1804),
  download_files = TRUE
)

# read in and clean flowlines
file_paths <- list.files(nhdplushr_dir_path, full.names = TRUE)

gdb_file_paths <- file_paths[stringr::str_ends(file_paths, "\\.gdb$")]

flowlines <- list()
vaa <- list()

for (i in 1:length(gdb_file_paths)) {
  file_path <- gdb_file_paths[i]

  fl <- sf::st_read(dsn = file_path, layer = "NHDFlowline")
  fl <- sf::st_transform(x = fl, crs = global_crs)
  fl <- sf::st_zm(fl)
  sf::st_geometry(fl) <- global_geom_name
  names(fl) <- tolower(names(fl))

  v <- sf::st_read(dsn = file_path, layer = "NHDPlusFlowlineVAA")
  names(v) <- tolower(names(v))

  flowlines[[i]] <- fl
  vaa[[i]] <- v
}

flowlines <- do.call(what = "rbind", args = flowlines)
vaa <- do.call(what = "rbind", args = vaa)

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

flowlines <- dplyr::filter(flowlines, stream_order >= 7)

# write out flowlines
save(
  flowlines,
  file = here::here("workshop", "early_impl", "data", "flowlines.rda")
)

# read in waterbodies
waterbodies <- list()

for (i in 1:length(gdb_file_paths)) {
  file_path <- gdb_file_paths[i]

  wb <- sf::st_read(dsn = file_path, layer = "NHDWaterbody")
  wb <- sf::st_transform(x = wb, crs = global_crs)
  wb <- sf::st_zm(wb)
  wb <- sf::st_make_valid(wb)
  sf::st_geometry(wb) <- global_geom_name
  names(wb) <- tolower(names(wb))

  waterbodies[[i]] <- wb
}

waterbodies <- do.call(what = "rbind", args = waterbodies)

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

intersecting_indices <- sf::st_intersects(
  x = waterbodies,
  y = flowlines,
  sparse = TRUE
)

waterbodies <- waterbodies[lengths(intersecting_indices) > 0, ]

# write out waterbodies
save(
  waterbodies,
  file = here::here("workshop", "early_impl", "data", "waterbodies.rda")
)


