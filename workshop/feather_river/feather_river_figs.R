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

# create basic colors
white <- "#ffffff"
mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

# create colors for restoration sites
restoration_colors <- c("#6a00b8", "#0000ff", "#00ced1", "#00ff7f", "#32cd32")

# create colors for flow stations
flow_station_colors <- c("#e23d3d", "#ff8700", "#ffd700")

# create colors for hydrographs that match flow stations
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

# create colors for visualizing flow-habitat area curves
interpolation_colors <- scale_colour_hue()$palette(5)
interpolation_colors_light <- colorspace::lighten(interpolation_colors, amount = 0.3)
interpolation_colors_dark <- colorspace::darken(interpolation_colors, amount = 0.3)





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
  geom_point(size = 5) +
  geom_vline(
    xintercept = max(sbrs_linear_interpolation$flow_cfs),
    color = dark_grey,
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    limits = c(600, 3200),
    labels = scales::comma,
    breaks = seq(from = 600, to = 3200, by = 400),
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
      size = 16,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(size = 14, color = black, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, color = black, margin = margin(r = 20)),
    axis.text = element_text(size = 12, color = black)
  )

# write out SBRS data plot
ggsave(
  filename = "sbrs_flow_area_points.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 320,
  height = 1800,
  width = 2400,
  units = "px",
  bg = white
)





### make a map of the system ---------------------------------------------------

# create a map
ggplot() +
  geom_sf(
    data = oroville_thermalito,
    color = black,
    fill = black,
    linewidth = 1
  ) +
  geom_sf(data = feather_flowlines, color = black, linewidth = 1) +
  geom_sf(data = yuba_city, color = black, fill = black, shape = 22, size = 5) +
  geom_sf_text(data = yuba_city, aes(label = label), nudge_x = -0.08, size = 5) +
  geom_sf(
    data = restoration_sites,
    aes(fill = name),
    color = black,
    shape = 21,
    size = 5
  ) +
  scale_fill_manual(
    name = "Planned restoration sites",
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
  scale_fill_manual(name = "Flow gages", values = flow_station_colors) +
  labs(title = "Feather River gaging and proposed habitat restoration") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, color = black, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 14, color = black, face = "bold"),
    legend.text = element_text(size = 14, color = black),
    panel.grid = element_blank()
  )

# write out the map
ggsave(
  filename = "feather_river_map_gaging_restoration.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 320,
  height = 2400,
  width = 3200,
  units = "px",
  bg = white
)





### create faceted hydrographs -------------------------------------------------

# create a hydrographs plot
hydrographs <- ggplot(
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
    title = "Feather River hydrographs",
    y = "dischage (cfs)",
    color = "water year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, color = black, face = "bold"),
    strip.text = element_text(size = 14, color = black, face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, color = black, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 14, color = black, margin = margin(r = 10)),
    axis.text.y = element_text(size = 12, color = black),
    panel.grid.minor = element_blank()
  )

# create a standalone legend for the water years in shades of grey and black
hydrograph_legend_data <- data.frame(water_year = factor(unique(flows$water_year)))

hydrograph_legend_colors <- setNames(
  c(mid_grey, dark_grey, black),
  levels(hydrograph_legend_data$water_year)
)

hydrographs_legend <- ggplot(
  data = hydrograph_legend_data,
  aes(x = water_year, color = water_year)
  ) +
  geom_line(aes(y = 1), linewidth = 2) +
  scale_color_manual(
    values = hydrograph_legend_colors,
    name = "Water year"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14)
  )

hydrographs_legend <- cowplot::get_legend(hydrographs_legend)

# combine hydrographs plot and standalone legend
hydrographs_w_legend <- cowplot::plot_grid(
  hydrographs,
  hydrographs_legend,
  ncol = 2,
  rel_widths = c(4, 0.8)
)

# write out the hydrographs
ggsave(
  plot = hydrographs_w_legend,
  filename = "feather_river_hydrographs.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 320,
  height = 3200,
  width = 2800,
  units = "px",
  bg = white
)





### plot interpolations --------------------------------------------------------

# create a plot of interpolated curves
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
    limits = c(600, 3200),
    labels = scales::comma,
    breaks = seq(from = 600, to = 3200, by = 400),
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
      size = 16,
      face = "bold",
      margin = margin(b = 20)
    ),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(size = 14, color = black, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, color = black, margin = margin(r = 20)),
    axis.text = element_text(size = 12, color = black)
  )

# write out interpolated curves plot
ggsave(
  filename = "sbrs_interpolation_curves.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 320,
  height = 1800,
  width = 2400,
  units = "px",
  bg = white
)





### plot interpolations summary ------------------------------------------------

# create a plot of the interpolations summary
ggplot(
  data = sbrs_interpolation_areas,
  aes(x = interpolation_method, y = area, fill = interpolation_method)
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = area_lower, ymax = area_upper),
    color = dark_grey,
    linewidth = 1,
    width = 0.1
  ) +
  geom_hline(
    yintercept = min(sbrs_interpolation_areas$area_lower),
    linetype = "dashed",
    color = dark_grey,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = max(sbrs_interpolation_areas$area_upper),
    linetype = "dashed",
    color = dark_grey,
    linewidth = 1
  ) +
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
    y = "area under the curve",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold",
      margin = margin(b = 20)
    ),
    legend.position = "none",
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(size = 14, margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, color = black, margin = margin(r = 20)),
    axis.text.x = element_text(size = 12, color = black, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, color = black),
    panel.grid.major.x = element_blank()
  )

# write out interpolation summary plot
ggsave(
  filename = "sbrs_interpolation_areas.png",
  path = here::here("workshop", "feather_river", "figs"),
  device = "png",
  dpi = 320,
  height = 1800,
  width = 1800,
  units = "px",
  bg = white
)
