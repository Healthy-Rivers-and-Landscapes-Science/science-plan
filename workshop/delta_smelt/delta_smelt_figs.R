### set up ---------------------------------------------------------------------

# source global variables and attach packages
source(here::here("workshop", "delta_smelt", "globals.R"))

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "delta_smelt", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### create colors --------------------------------------------------------------

white <- "#ffffff"
mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

light_blue <- "#6ecfff"

point_colors <- c("#005585", "#d39200")





### create a single object for mapping =----------------------------------------

dsls_stations <- dplyr::mutate(
  dsls_stations,
  location_type = "dsls"
)

edsm_sample_locations <- dplyr::mutate(
  edsm_sample_locations,
  location_type = "edsm"
)

monitoring_locations <- rbind(
  dplyr::select(dsls_stations, location_type),
  dplyr::select(edsm_sample_locations, location_type)
)





### create a map =--------------------------------------------------------------

facet_labels <- c(
  "dsls" = "IEP spring Kodiak trawl",
  "edsm" = "USFWS enhanced Delta smelt\nmonitoring program"
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = monitoring_locations,
    aes(color = location_type, size = location_type)
  ) +
  scale_color_manual(values = point_colors, guide = "none") +
  scale_size_manual(values = c(2, 0.2), guide = "none") +
  facet_wrap(
    ~ location_type,
    ncol = 2,
    labeller = labeller(location_type = facet_labels)
  ) +
  labs(title = "Delta smelt monitoring programs") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    strip.text = element_text(
      size = 12,
      color = black,
      face = "bold",
      margin = margin(t = 20, b = 10)
    ),
    axis.text = element_blank(),
    panel.spacing = unit(1, "cm"),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = mid_grey,
      fill = NA,
      linewidth = 1
    )
  )

ggsave(
  filename = "delta_smelt_monitoring_map.png",
  path = here::here("workshop", "delta_smelt", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4400,
  units = "px",
  bg = white
)
