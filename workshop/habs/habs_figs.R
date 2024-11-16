### set up ---------------------------------------------------------------------

# attach packages
library(ggplot2)

# source global variables
source(here::here("workshop", "habs", "globals.R"))

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "habs", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### create colors --------------------------------------------------------------

white <- "#ffffff"
black <- "#000000"

light_blue <- "#6ecfff"

microcystis_colors <- c("#2eb22e", "#ffd700", "#ff8c00")





### make faceted maps of the microcystis monitoring data -----------------------

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis,
    aes(fill = microcystis_90th_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  scale_fill_manual(
    name = "Microcystis",
    values = microcystis_colors
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom"
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "90th percentile observation"
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
    strip.text = element_text(size = 12, color = black, face = "bold"),
    legend.title = element_text(size = 12, color = black, face = "bold"),
    legend.text = element_text(size = 12, color = black),
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  filename = "habs_90th_map.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 320,
  height = 1200,
  width = 2000,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis,
    aes(fill = microcystis_max_bin),
    color = black,
    size = 2,
    shape = 21
  ) +
  scale_fill_manual(
    name = "Microcystis",
    values = microcystis_colors
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom"
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "maximum single observation"
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
    strip.text = element_text(size = 12, color = black, face = "bold"),
    legend.title = element_text(size = 12, color = black, face = "bold"),
    legend.text = element_text(size = 12, color = black),
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  filename = "habs_max_map.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 320,
  height = 1200,
  width = 2000,
  units = "px",
  bg = white
)





### make faceted maps of the microcystis monitoring data with new stations -----

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis,
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
    name = "Microcystis",
    values = microcystis_colors
  ) +
  scale_color_manual(
    name = NULL,
    values = black,
    labels = "proposed new station"
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom"
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "90th percentile observation"
  ) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
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
    strip.text = element_text(size = 12, color = black, face = "bold"),
    legend.title = element_text(size = 12, color = black, face = "bold"),
    legend.text = element_text(size = 12, color = black),
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  filename = "habs_90th_map_w_new.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 320,
  height = 1200,
  width = 2000,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = microcystis,
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
    name = "Microcystis",
    values = microcystis_colors
  ) +
  scale_color_manual(
    name = NULL,
    values = black,
    labels = "proposed new station"
  ) +
  facet_wrap(
    ~ season,
    ncol = 2,
    strip.position = "bottom"
  ) +
  labs(
    title = "EMP discrete water quality monitoring: microcystis data",
    subtitle = "maximum single observation"
  ) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
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
    strip.text = element_text(size = 12, color = black, face = "bold"),
    legend.title = element_text(size = 12, color = black, face = "bold"),
    legend.text = element_text(size = 12, color = black),
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  filename = "habs_max_map_w_new.png",
  path = here::here("workshop", "habs", "figs"),
  device = "png",
  dpi = 320,
  height = 1200,
  width = 2000,
  units = "px",
  bg = white
)
