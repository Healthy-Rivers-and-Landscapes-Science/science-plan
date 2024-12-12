### set up ---------------------------------------------------------------------

# source global variables and attach packages
source(here::here("workshop", "habs", "globals.R"))

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "habs", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### create colors --------------------------------------------------------------

white <- "#ffffff"
mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

light_blue <- "#6ecfff"

mvi_colors <- c(mid_grey, dark_grey, "#ffd700", "#ff8c00", "#ff2a00")
mvi_binned_colors <- c(mid_grey, "#ffd700", "#ff8c00")





### make faceted maps of the microcystis monitoring data -----------------------

# create a function to generate custom facet labels
facet_labeller <- function(value) {
  labels <- list(
    "summer" = "**summer**<br><br>*May to November*",
    "winter" = "**winter**<br><br>*December to April*"
  )

  value <- as.character(value)

  sapply(value, function(v) labels[[v]])
}

legend_label_labeller <- function(value) {
  labels <- list(
    "absent" = "absent<br><span style='font-size:10px;display:block;'>*MVI = 1*</span>",
    "low" = "low<br><span style='font-size:10px;display:block;'>*MVI = 2 or 3*</span>",
    "high" = "high<br><span style='font-size:10px;display:block;'>*MVI = 4 or 5*</span>"
  )

  value <- as.character(value)

  sapply(value, function(v) labels[[v]])
}

# make maps
ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis_summary,
    aes(fill = microcystis_90th_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "90th percentile observation, 2015-2023"
  ) +
  guides(fill = guide_legend(
    override.aes = list(size = 4),
    label.position = "bottom",
    title.position = "left",
    title.vjust = 0.7
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 10,
      color = black,
      face = "italic",
      margin = margin(b = 20)
    ),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(t = 20)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(size = 12, color = black, hjust = 0.5),
    legend.position = "bottom",
    legend.key.spacing.x = unit(x = 1, units = "cm"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = mid_grey,
      fill = NA,
      linewidth = 1
    )
  )

ggsave(
  filename = "habs_90th_map.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4400,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis_summary,
    aes(fill = microcystis_max_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "maximum single observation, 2015-2023"
  ) +
  guides(fill = guide_legend(
    override.aes = list(size = 4),
    label.position = "bottom",
    title.position = "left",
    title.vjust = 0.7
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 10,
      color = black,
      face = "italic",
      margin = margin(b = 20)
    ),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(t = 20)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(size = 12, color = black, hjust = 0.5),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = mid_grey,
      fill = NA,
      linewidth = 1
    )
  )

ggsave(
  filename = "habs_max_map.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4400,
  units = "px",
  bg = white
)





### make faceted maps of the microcystis monitoring data with new stations -----

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis_summary,
    aes(fill = microcystis_90th_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  geom_sf(
    data = dplyr::filter(poi_stations, station_type == "new"),
    aes(color = station_type),
    size = 2
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  scale_color_manual(
    name = NULL,
    values = black,
    labels = "proposed new station"
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "90th percentile observation, 2015-2023"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(size = 4)
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(size = 4)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 10,
      color = black,
      face = "italic",
      margin = margin(b = 20)
    ),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(t = 20)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(size = 12, color = black, hjust = 0.5),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = mid_grey,
      fill = NA,
      linewidth = 1
    )
  )

ggsave(
  filename = "habs_90th_map_w_new.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4400,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis_summary,
    aes(fill = microcystis_max_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  geom_sf(
    data = dplyr::filter(poi_stations, station_type == "new"),
    aes(color = station_type),
    size = 2
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  scale_color_manual(
    name = NULL,
    values = black,
    labels = "proposed new station"
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "maximum single observation, 2015-2023"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(size = 4)
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(size = 4)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 10,
      color = black,
      face = "italic",
      margin = margin(b = 20)
    ),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(t = 20)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(size = 12, color = black, hjust = 0.5),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = mid_grey,
      fill = NA,
      linewidth = 1
    )
  )

ggsave(
  filename = "habs_max_map_w_new.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4400,
  units = "px",
  bg = white
)





### make charts of the distribution of observations for each site --------------

# order the data and create a factor to sort x axis from west to east
microcystis_all <- microcystis_all |>
  dplyr::mutate(longitude = sf::st_coordinates(microcystis_all)[, 1]) |>
  dplyr::arrange(longitude) |>
  dplyr::mutate(station = factor(station, levels = unique(station)))

# compute proportions for proportional stacked bar charts
microcystis_all_props <- microcystis_all |>
  dplyr::group_by(station, severity_cat) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(station) |>
  dplyr::mutate(prop = count / sum(count)) |>
  dplyr::ungroup()

microcystis_all_season_props <- microcystis_all |>
  dplyr::group_by(station, season, severity_cat) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(station, season) |>
  dplyr::mutate(season_prop = count / sum(count)) |>
  dplyr::ungroup()

# create a stacked bar chart for all observations
ggplot(microcystis_all, aes(x = station, fill = severity_cat)) +
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(from = 0, to = 100, by = 20),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = "Microcystis",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  labs(
    title = "Microcystis observations by station",
    subtitle = "2015-2023",
    x = "station (ordered west to east)",
    y = "number of observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold",
    ),
    plot.subtitle = element_text(
      color = black,
      size = 14,
      face = "italic",
      margin = margin(t = 5, b = 20)
    ),
    legend.title = element_text(size = 12, color = black, face = "bold", margin = margin(r = 40)),
    legend.text = ggtext::element_markdown(size = 12, color = black, hjust = 0.5),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 14,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 14,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(size = 12, color = black)
  )

ggsave(
  filename = "habs_bar.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 4800,
  units = "px",
  bg = white
)

# create a proportional stacked bar chart for all observations
ggplot(microcystis_all_props, aes(x = station, y = prop, fill = severity_cat)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  labs(
    title = "Microcystis observations by station",
    subtitle = "2015-2023",
    x = "station (ordered west to east)",
    y = "proportion of observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = black,
      size = 14,
      face = "italic",
      margin = margin(t = 5, b = 20)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(
      size = 12,
      color = black,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 14,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 14,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(
      size = 12,
      color = black
    )
  )

ggsave(
  filename = "habs_bar_prop.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 3600,
  width = 4800,
  units = "px",
  bg = white
)

# create a stacked bar chart faceted by season
ggplot(
  microcystis_all,
  aes(x = station, fill = severity_cat)
) +
  geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 60, by = 20),
    expand = c(0, 1)
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  facet_wrap(
    ~ season,
    nrow = 2,
    strip.position = "top",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "Microcystis observations by station",
    subtitle = "2015-2023",
    x = "station (ordered west to east)",
    y = "number of observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = black,
      size = 14,
      face = "italic",
      margin = margin(t = 5, b = 20)
    ),
    panel.spacing = unit(1, "cm"),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(b = 10)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(
      size = 12,
      color = black,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 14,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 14,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(
      size = 12,
      color = black
    )
  )

ggsave(
  filename = "habs_bar_season.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 4800,
  units = "px",
  bg = white
)

# create a proportional stacked bar chart faceted by season
ggplot(
  microcystis_all_season_props,
  aes(x = station, y = season_prop, fill = severity_cat)
) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = "Microcystis\nseverity",
    labels = legend_label_labeller(c("absent", "low", "high")),
    values = mvi_binned_colors
  ) +
  facet_wrap(
    ~ season,
    nrow = 2,
    strip.position = "top",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "Microcystis observations by station and season",
    subtitle = "2015-2023",
    x = "station (ordered west to east)",
    y = "proportion of observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = black,
      size = 14,
      face = "italic",
      margin = margin(t = 5, b = 20)
    ),
    panel.spacing = unit(1, "cm"),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(b = 10)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = ggtext::element_markdown(
      size = 12,
      color = black,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 14,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 14,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(
      size = 12,
      color = black
    )
  )

ggsave(
  filename = "habs_bar_prop_season.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 4800,
  units = "px",
  bg = white
)

# create a proportional stacked bar chart faceted by season without binning MVI
microcystis_all_season_props_unbinned <- microcystis_all |>
  sf::st_drop_geometry() |>
  dplyr::group_by(station, season, microcystis) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(station, season) |>
  dplyr::mutate(season_prop = count / sum(count)) |>
  dplyr::ungroup()

dummy_row <- tibble::tibble(
  station = factor(
    "D41A",
    levels = levels(microcystis_all_season_props_unbinned$station)
  ),
  season = "summer",
  microcystis = 5,
  count = NA_integer_,
  season_prop = 0
)

microcystis_all_season_props_unbinned <- dplyr::bind_rows(
  microcystis_all_season_props_unbinned,
  dummy_row
)

microcystis_all_season_props_unbinned <- dplyr::mutate(
  microcystis_all_season_props_unbinned,
  microcystis = factor(
    as.character(microcystis),
    levels = c("1", "2", "3", "4", "5")
  )
)

names(mvi_colors) <- levels(microcystis_all_season_props_unbinned$microcystis)

ggplot(
    microcystis_all_season_props_unbinned,
    aes(x = station, y = season_prop, fill = microcystis)
  ) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 1, by = 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = "MVI",
    values = mvi_colors
  ) +
  facet_wrap(
    ~ season,
    nrow = 2,
    strip.position = "top",
    labeller = as_labeller(facet_labeller)
  ) +
  labs(
    title = "Microcystis observations by station and season",
    subtitle = "2015-2023",
    x = "station (ordered west to east)",
    y = "proportion of observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      color = black,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = black,
      size = 14,
      face = "italic",
      margin = margin(t = 5, b = 20)
    ),
    panel.spacing = unit(1, "cm"),
    strip.text = ggtext::element_markdown(
      size = 12,
      color = black,
      margin = margin(b = 10)
    ),
    legend.title = element_text(
      size = 12,
      color = black,
      face = "bold",
      hjust = 0.5,
      margin = margin(r = 40)
    ),
    legend.text = element_text(
      size = 12,
      color = black,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.key.spacing.x = unit(1, "cm"),
    axis.line = element_line(color = dark_grey),
    axis.title.x = element_text(
      size = 14,
      color = black,
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      size = 14,
      color = black,
      margin = margin(r = 20)
    ),
    axis.text = element_text(
      size = 12,
      color = black
    )
  )

ggsave(
  filename = "habs_bar_prop_season_unbinned.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 4800,
  units = "px",
  bg = white
)
