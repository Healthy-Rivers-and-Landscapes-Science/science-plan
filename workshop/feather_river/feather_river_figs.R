### set up ---------------------------------------------------------------------

# attach packages
library(ggplot2)

# specify global analytic parameters
global_crs <- 4269
global_geom_name <- "geometry"

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "feather_river", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### select waterbodies ---------------------------------------------------------

# keep only waterbodies associated with Oroville and Thermalito
oroville_thermalito <- dplyr::filter(
  waterbodies,
  grepl("Oroville|Thermalito", gnis_name)
)





### select flowlines -----------------------------------------------------------

# specify start points and arguments for flow network tracing
trace_start_pts <- tibble::tribble(
  ~place,                        ~lon,        ~lat,      ~mode, ~distance,
  "thermalito_forebay_inflow",   -121.580195, 39.531312, "UM",  30,
  "hatchery_outtake",            -121.547511, 39.520641, "DM",  30,
  "thermalito_afterbay_release", -121.639103, 39.457570, "DM",  0.1,
  "feather_yuba_confluence",     -121.596702, 39.129015, "UM",  25,
  "feather_bear_confluence",     -121.577792, 38.939316, "UM",  25,
  "feather_sac_confluence",      -121.624698, 38.788601, "UM",  120,
  "sac_smf",                     -121.634237, 38.690365, "UM",  75
)

trace_start_pts <- sf::st_as_sf(
  x = trace_start_pts,
  coords = c("lon", "lat"),
  crs = global_crs
)

# identify flowlines nearest to trace start points
trace_start_indices <- hydroloom::index_points_to_lines(
  x = flowlines,
  points = trace_start_pts
)

trace_start_pts <- cbind(dplyr::select(trace_start_indices,id), trace_start_pts)

# trace network from start points
trace_ids <- c()

for (i in 1:nrow(trace_start_pts)) {
  pt <- trace_start_pts[i, ]
  trace_result <- hydroloom::navigate_hydro_network(
    x = flowlines,
    start = pt$id,
    mode = pt$mode,
    distance = pt$distance
  )
  trace_ids <- c(trace_ids, trace_result)
}

# retain desired flowlines
feather_flowlines <- dplyr::filter(flowlines, id %in% trace_ids)





### create color schemes -------------------------------------------------------

mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

restoration_colors <- c("#6a00b8", "#0000ff", "#00ced1", "#00ff7f", "#32cd32")
flow_station_colors <- c("#ff6347", "#ffa500", "#ffd700")

water_year_color <- unlist(lapply(flow_station_colors, function(color) {
  c(
    colorspace::lighten(color, amount = 0.3),
    color,
    colorspace::darken(color, amount = 0.3)
  )
}))

water_year_colors <- cbind(
  dplyr::select(flows, location_id, water_year) |> unique(),
  water_year_color
)

flows <- dplyr::left_join(
  flows,
  water_year_colors,
  by = c("location_id", "water_year")
)





### make a map of the system ---------------------------------------------------

ggplot() +
  geom_sf(
    data = oroville_thermalito,
    color = black,
    fill = black,
    linewidth = 1
  ) +
  geom_sf(data = feather_flowlines, color = black, linewidth = 1) +
  geom_sf(data = yuba_city, color = black, fill = black, shape = 22, size = 5) +
  geom_sf_text(data = yuba_city, aes(label = label), nudge_x = -0.05, size = 4) +
  geom_sf(
    data = restoration_sites,
    aes(fill = name),
    color = black,
    shape = 21,
    size = 5
  ) +
  scale_fill_manual(
    name = "planned restoration sites",
    values = restoration_colors
  ) +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = flow_stations,
    aes(fill = location_id),
    color = black,
    shape = 24,
    size = 5
  ) +
  scale_fill_manual(name = "gages", values = flow_station_colors) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )





### create faceted hydrographs -------------------------------------------------

ggplot(
  data = flows,
  aes(
    x = water_year_day,
    y = parameter_value,
    color = water_year_color,
    group = water_year
  )) +
  annotate(
    "rect",
    xmin = 93,
    xmax = 274,
    ymin = -Inf,
    ymax = Inf,
    fill = mid_grey,
    alpha = 0.45
  ) +
  geom_line(linewidth = 1) +
  geom_hline(
    yintercept = min(sbrs_flow_area$flow_cfs),
    color = dark_grey,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = max(sbrs_flow_area$flow_cfs),
    color = dark_grey,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  facet_wrap(~ location_id, ncol = 1) +
  scale_x_continuous(
    breaks = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336),
    labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(from = 0, to = 50000, by = 10000),
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  scale_color_identity() +
  labs(
    y = "dischage (cfs)",
    color = "water year"
  ) +
  guides(color = guide_legend(label.hjust = 0.5)) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )
