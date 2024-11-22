### set up ---------------------------------------------------------------------

# source global variables and attach packages
source(here::here("workshop", "feather_river", "globals.R"))

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "feather_river", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### select waterbodies ---------------------------------------------------------

# keep only waterbodies associated with Oroville and Thermalito
oroville_thermalito <- waterbodies[grepl("Oroville|Thermalito", waterbodies$gnis_name), ]





### select flowlines -----------------------------------------------------------

# specify start points and arguments for flow network tracing
trace_start_pts <- tibble::tribble(
  ~place,                        ~lon,        ~lat,      ~mode, ~distance,
  "thermalito_forebay_inflow",   -121.580195, 39.531312, "UM",  30,
  "hatchery_outtake",            -121.547511, 39.520641, "DM",  30,
  "thermalito_afterbay_release", -121.639103, 39.457570, "DM",  0.1,
  "feather_yuba_confluence",     -121.596702, 39.129015, "UM",  40,
  "feather_bear_confluence",     -121.577792, 38.939316, "UM",  35,
  "feather_sac_confluence",      -121.624698, 38.788601, "UM",  200,
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





### create polygons to represent restoration sites as though the are parcels ---

scale_factor <- 0.04

restoration_polygons <- list(
  sf::st_polygon(list(matrix(c(
    # L shape
    2.12, 0.71,
    2.83, 1.41,
    3.54, 0.71,
    2.83, 0.00,
    3.54, -0.71,
    2.83, -1.41,
    2.12, -0.71,
    1.41, 0.00,
    2.12, 0.71
  ) * scale_factor * 0.5, ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(
    # rectangle
    0.00, 0.00,
    0.71, -0.71,
    1.06, -0.35,
    0.35, 0.35,
    0.00, 0.00
  ) * scale_factor, ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(
    # rectangle
    0.00, 0.00,
    0.91, 0.42,
    0.06, 2.18,
    -0.85, 1.76,
    0.00, 0.00
  ) * scale_factor * 0.8, ncol = 2, byrow = TRUE))),
  sf::st_polygon(list(matrix(c(
    0.00, 0.00,
    -0.71, 0.71,
    -0.35, 1.06,
    -0.71, 1.41,
    -0.35, 1.77,
    -0.18, 1.59,
    0.53, 2.30,
    1.41, 1.41,
    0.00, 0.00
  ) * scale_factor * 0.9, ncol = 2, byrow = TRUE)))
)

restoration_polygons <- sf::st_sf(
  geometry = st_sfc(restoration_polygons, crs = global_crs),
  shape = c("l_shape", "rect_1", "rect_2", "funky"),
  id = 1:4
)

restoration_polygons <- restoration_polygons |>
  dplyr::rowwise() |>
  dplyr::mutate(
    geometry = sf::st_sfc(
      sf::st_geometry(geometry) +
        (sf::st_coordinates(restoration_sites[restoration_sites$id == id, ])[1, ] -
           sf::st_coordinates(sf::st_centroid(geometry))[1, ]),
      crs = global_crs
    )
  )





### create color schemes -------------------------------------------------------

# create basic colors
white <- "#ffffff"
mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

red <- "#f4684e"
yellow <- "#f1ee00"

# create colors for flow stations
flow_station_colors <- c("#f4684e", "#4042ef", "#f4a611")

# create colors for hydrographs that match flow stations
water_year_color <- unlist(lapply(flow_station_colors, function(color) {
  c(
    colorspace::lighten(color, amount = 0.4),
    color,
    colorspace::darken(color, amount = 0.7)
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

# create colors for visualizing flow-habitat area curves
interpolation_colors <- scale_colour_hue()$palette(5)

interpolation_colors_light <- colorspace::lighten(
  interpolation_colors,
  amount = 0.3
)

interpolation_colors_dark <- colorspace::darken(
  interpolation_colors,
  amount = 0.3
)





### create other visualization parameters --------------------------------------

flow_area_x_lower <- 500
flow_area_x_upper <- 3500
flow_area_y_lower <- 2.5
flow_area_y_upper <- 5.0





### create a figure for the SBRS flow-habitat area data points -----------------

ggplot(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black
  ) +
  geom_vline(
    xintercept = min(sbrs_linear_interpolation$flow_cfs),
    color = dark_grey,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = max(sbrs_linear_interpolation$flow_cfs),
    color = dark_grey,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_point(size = 8) +
  scale_x_continuous(
    limits = c(flow_area_x_lower, flow_area_x_upper),
    labels = scales::comma,
    breaks = seq(from = flow_area_x_lower, to = flow_area_x_upper, by = 500),
    expand = c(0, 200)
  ) +
  scale_y_continuous(
    limits = c(flow_area_y_lower, flow_area_y_upper),
    breaks = seq(from = flow_area_y_lower, to = flow_area_y_upper, by = 0.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "SBRS flow-habitat area data",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 24,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 18,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 18,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 16, color = black)
  )

# write out SBRS data plot
ggsave(
  filename = "sbrs_flow_area_points.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)





### make a map of the system ---------------------------------------------------

# create a map
ggplot() +
  ggspatial::annotation_map_tile(
    type = "cartolight",
    zoom = 9,
    zoomin = -5,
    forcedownload = TRUE
  ) +
  geom_sf(
    data = oroville_thermalito,
    color = black,
    fill = black,
    linewidth = 1
  ) +
  geom_sf(data = feather_flowlines, color = black, linewidth = 1) +
  geom_sf(
    data = yuba_city,
    color = black,
    fill = black,
    shape = 22,
    size = 5
  ) +
  geom_sf(
    data = restoration_polygons,
    aes(shape = "potential\nrestoration site"),
    fill = yellow,
    color = black,
    linewidth = 1,
    size = 5
  ) +
  geom_sf(
    data = flow_stations,
    aes(shape = "flow gage"),
    fill = red,
    color = black,
    stroke = 1.5,
    size = 5
  ) +
  scale_shape_manual(
    values = c("flow gage" = 24, "potential\nrestoration site" = 22),
    guide = guide_legend(byrow = TRUE, override.aes = list(size = 5))
  ) +
  labs(title = "Feather River gaging and\nproposed habitat restoration") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = black, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20, color = black),
    legend.key.size = unit(0.5, "cm"),
    legend.key.spacing.y = unit(0.5, "cm"),
    panel.grid = element_blank()
  )

# write out the map
ggsave(
  filename = "feather_river_map_gaging_restoration.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 6400,
  units = "px",
  bg = white
)





### create faceted hydrographs -------------------------------------------------

# prep the data
y_limit <- 20000
flows_clipped <- dplyr::mutate(
  flows,
  exceed = parameter_value > y_limit,
  parameter_value = ifelse(exceed, y_limit, parameter_value)
)

# create the hydrographs
hydrographs <- ggplot(
  data = dplyr::filter(
    flows_clipped,
    water_year_day >= 93 & water_year_day <= 305
  ),
  aes(
    x = water_year_day,
    y = parameter_value,
    color = water_year_color,
    group = water_year
  )) +
  geom_vline(
    xintercept = c(93, 124, 152, 183, 213, 244, 274),
    color = "#d9d9d9",
    linewidth = 0.5
  ) +
  annotate(
    "rect",
    xmin = 124,
    xmax = 274,
    ymin = -Inf,
    ymax = Inf,
    fill = mid_grey,
    alpha = 0.45
  ) +
  ggplot2::geom_line(linewidth = 0.75) +
  geom_hline(
    yintercept = c(min(sbrs_flow_area$flow_cfs), max(sbrs_flow_area$flow_cfs)),
    color = black,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = c(108.5, 138, 167.5, 198, 228.5, 259, 289.5),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"),
    limits = c(93, 304),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(from = 0, to = y_limit, by = 5000),
    limits = c(0, y_limit),
    expand = c(0, 0)
  ) +
  scale_color_identity() +
  facet_wrap(~ location_id, ncol = 1) +
  labs(
    title = "Feather River hydrographs",
    y = "dischage (cfs)",
    color = "water year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = black, face = "bold"),
    strip.text = element_text(
      size = 18,
      color = black,
      face = "bold",
      margin = margin(t = 20, b = 10)
    ),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16, color = black),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 10)
    ),
    axis.text.y = element_text(size = 14, color = black),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# create a standalone legend for the water years in shades of grey and black
hydrograph_legend_data <- data.frame(
  water_year = rep(c("2021-2022", "2022-2023", "2023-2024"), each = 2),
  x = rep(c(1, 2), times = 3),
  y = rep(1, 6)
)

hydrograph_legend_colors <- setNames(
  c(mid_grey, dark_grey, black),
  levels(hydrograph_legend_data$water_year)
)

hydrographs_legend <- ggplot(
  data = hydrograph_legend_data,
  aes(x = x, y = y, color = water_year, group = water_year)
) +
  geom_line(linewidth = 2) +
  scale_color_manual(
    values = hydrograph_legend_colors,
    name = "water year"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 18)
  )

hydrographs_legend <- cowplot::get_legend(hydrographs_legend)

# combine hydrographs plot and standalone legend
cowplot::plot_grid(
  hydrographs,
  hydrographs_legend,
  ncol = 2,
  rel_widths = c(4, 1)
)

# write out the hydrographs
ggsave(
  filename = "feather_river_hydrographs.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 6800,
  width = 6000,
  units = "px",
  bg = white
)





### plot interpolations --------------------------------------------------------

# create a plot showing a linear interpolation
ggplot() +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out linear interpolation plot
ggsave(
  filename = "interpolation_1_linear.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# create a plot showing a linear interpolation with a margin of error
ggplot() +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out linear interpolation plot
ggsave(
  filename = "interpolation_2_linear_w_error.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# add an anchored cubic spline interpolation
ggplot() +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[2],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[2],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out cubic spline interpolation plot
ggsave(
  filename = "interpolation_3_cubic_spline.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# add a sine curve interpolation
ggplot() +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[2],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[2],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_sine_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[3],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_sine_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[3],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out sine interpolation plot
ggsave(
  filename = "interpolation_4_sine.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# add a convex ellipse interpolation
ggplot() +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[2],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[2],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_sine_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[3],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_sine_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[3],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_ellipse_convex_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[4],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_ellipse_convex_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[4],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out convex ellipse interpolation plot
ggsave(
  filename = "interpolation_5_ellipse_convex.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# add a concave ellipse interpolation to create a plot of all interpolations
ggplot() +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[2],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[2],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_sine_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[3],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_sine_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[3],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_ellipse_convex_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[4],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_ellipse_convex_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[4],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_ellipse_concave_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[5],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_ellipse_concave_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[5],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 3250),
    labels = scales::comma,
    breaks = seq(from = 500, to = 3000, by = 500),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )


# write out convex ellipse interpolation plot
ggsave(
  filename = "interpolation_6_all.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 5200,
  units = "px",
  bg = white
)

# create a plot with an extended x axis matching flow range
ggplot() +
  annotate(
    "rect",
    xmin = 3000,
    xmax = 10500,
    ymin = -Inf,
    ymax = Inf,
    fill = mid_grey,
    alpha = 0.45
  ) +
  annotate(
    "text",
    x = (3000 + 10500) / 2,
    y = 3.5,
    label = "?",
    size = 48,
    fontface = "bold",
    color = black
  ) +
  geom_ribbon(
    data = sbrs_linear_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[1],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_linear_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[1],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[2],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_anchored_cubic_spline_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[2],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_sine_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[3],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_sine_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[3],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_ellipse_convex_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[4],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_ellipse_convex_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[4],
    linewidth = 1
  ) +
  geom_ribbon(
    data = sbrs_ellipse_concave_interpolation,
    aes(
      x = flow_cfs,
      ymin = habitat_area_acres_lower,
      ymax = habitat_area_acres_upper
    ),
    fill = interpolation_colors_light[5],
    alpha = 0.45
  ) +
  geom_line(
    data = sbrs_ellipse_concave_interpolation,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = interpolation_colors_dark[5],
    linewidth = 1
  ) +
  geom_point(
    data = sbrs_flow_area,
    aes(x = flow_cfs, y = habitat_area_acres),
    color = black,
    size = 5
  ) +
  scale_x_continuous(
    limits = c(500, 10500),
    labels = scales::comma,
    breaks = seq(from = 0, to = 10000, by = 1000),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1.5, 5.5),
    breaks = seq(from = 2, to = 5, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Interpolated flow-area relationships",
    x = "discharge (cfs)",
    y = "habitat area (acres)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 16,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 14, color = black)
  )

# write out extended axis interpolation plot
ggsave(
  filename = "interpolation_7_extended_flows.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 12000,
  units = "px",
  bg = white
)





### plot interpolations summary ------------------------------------------------

# create y intercept values for plotting horizontal lines
hline_intercepts <- c(
  dplyr::filter(sbrs_interpolation_areas, interpolation_method == "linear") |>
    dplyr::pull(area_lower),
  dplyr::filter(sbrs_interpolation_areas, interpolation_method == "linear") |>
    dplyr::pull(area_upper)
)

# create plots that sequentially add info
plot_list <- lapply(1:nrow(sbrs_interpolation_areas), function(i) {

  # modify data to "hide" bars beyond the current step
  plot_data <- sbrs_interpolation_areas %>%
    mutate(
      area = ifelse(row_number() <= i, area, NA),
      area_lower = ifelse(row_number() <= i, area_lower, NA),
      area_upper = ifelse(row_number() <= i, area_upper, NA)
    )

  # dynamically set x-axis labels for the revealed bars
  x_labels <- sapply(1:nrow(sbrs_interpolation_areas), function(j) {
    if (j <= i) {
      c("linear", "cubic spline\n(anchored)", "sine\n(transformed)", "ellipse arc\n(convex)", "ellipse arc\n(concave)")[j]
    } else {
      ""
    }
  })

  # generate the plot
  ggplot(
    data = plot_data,
    aes(x = interpolation_method, y = area, fill = interpolation_method)
  ) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = area_lower, ymax = area_upper),
      color = "darkgrey",
      linewidth = 1,
      width = 0.2,
      na.rm = TRUE
    ) +
    geom_hline(
      yintercept = hline_intercepts,
      linetype = "dashed",
      color = "darkgrey",
      linewidth = 1
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_y_continuous(
      labels = scales::comma,
      breaks = seq(from = 0, to = 12000, by = 2000),
      expand = c(0, 0),
      limits = c(0, 11000)
    ) +
    scale_fill_manual(values = interpolation_colors_light) +
    labs(
      title = "Integrated flow-habitat area curves",
      x = "interpolation method",
      y = "area under the curve"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        color = "black",
        size = 22,
        face = "bold",
        margin = margin(b = 20)
      ),
      legend.position = "none",
      axis.line = element_line(color = "darkgrey"),
      axis.title.x = element_text(size = 16, margin = margin(t = 20)),
      axis.title.y = element_text(
        size = 16,
        color = "black",
        margin = margin(r = 20)
      ),
      axis.text.x = element_text(
        size = 14,
        color = "black",
        angle = 45,
        hjust = 0.5,
        vjust = 0.6
      ),
      axis.text.y = element_text(size = 14, color = "black"),
      panel.grid.major.x = element_blank()
    )
})

# write out area plots
lapply(seq_along(plot_list), function(i) {
  ggsave(
    plot = plot_list[[i]],
    filename = paste0("area_bar_chart_", i, ".png"),
    path = here::here("workshop", "feather_river", "figs"),
    device = "png",
    dpi = 700,
    height = 4800,
    width = 4800,
    units = "px",
    bg = white
  )
})





### plot a dummy example of best available science-based accounting ------------

# create additional curves

bas_example_curves <- tibble::tribble(
  ~curve,         ~x,    ~y,
  "sbrs",         650,   4.2,
  "sbrs",         725,   4.725,
  "sbrs",         3000,  2.625,
  "lower_river",  1000,  2.9,
  "lower_river",  3600,  4.1,
  "lower_river",  5200,  4.9,
  "lower_river",  9200,  4.6,
  "lower_river",  10400, 2.0,
  "side_channel", 700,   5.2,
  "side_channel", 950,   4.7,
  "side_channel", 1500,  4.2,
  "side_channel", 2000,  2.5,
  "side_channel", 3000,  2.5,
  "side_channel", 4000,  2.5,
  "middle_river", 600,   4.0,
  "middle_river", 1800,  4.7,
  "middle_river", 5200,  3.1,
  "middle_river", 6300,  2.9,
  "middle_river", 7400,  2.3
)

bas_example_curves$curve <- factor(
  bas_example_curves$curve,
  levels = c("sbrs", "side_channel", "middle_river", "lower_river")
)

ggplot(data = bas_example_curves) +
  ggforce::geom_mark_hull(
    aes(x = x, y = y),
    fill = mid_grey,
    color = NA,
    alpha = 0.5
  ) +
  geom_line(
    aes(x = x, y = y, color = curve),
    linewidth = 1
  ) +
  geom_point(
    aes(x = x, y = y, color = curve),
    size = 4
  ) +
  scale_color_manual(
    values = c(black, flow_station_colors),
    labels = c("Scientific Basis\nReport Supplement", "side channel",
               "middle river", "lower river")
  ) +
  scale_x_continuous(
    name = "discharge (cfs)",
    labels = scales::comma,
    limits = c(0, 11000),
    breaks = seq(from = 0, to = 10000, by = 1000),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "habitat area (acres)",
    expand = c(0.1, 0.1)
  ) +
  labs(title = "what should I call this?") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = "red",
      size = 22,
      face = "bold",
      margin = margin(b = 20)
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = black),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(size = 16, margin = margin(t = 20)),
    axis.title.y = element_text(
      size = 16,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text.x = element_text(
      size = 14,
      color = black,
      angle = 45,
      hjust = 0.5,
      vjust = 0.6
    ),
    axis.text.y = element_text(size = 14, color = black),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = "bas_curves.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 6000,
  units = "px",
  bg = white
)
