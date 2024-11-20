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

month_colors <- c("#d62828", "#7209b7", "#228b22", "#ffba08", "#ff4d6d")

region_colors <- c("#f6d9d3", "#f3e8f5", "#fde6c5", "#e3f1e8")
region_pt_colors <- colorspace::darken(region_colors, amount = 0.5)





### add necessary field and create objects for mapping -------------------------

dsls_stations <- dplyr::mutate(
  dsls_stations,
  location_type = "dsls",
  year = NA_integer_,
  month = NA_integer_,
  region_code = NA_character_
)

edsm_sample_locations <- dplyr::mutate(
  edsm_sample_locations,
  location_type = "edsm"
)

monitoring_locations <- rbind(
  dplyr::select(dsls_stations, location_type, year, month, region_code),
  dplyr::select(edsm_sample_locations, location_type, year, month, region_code)
)

edsm_regions <- dplyr::mutate(
  edsm_regions,
  location_type = "edsm"
)



### create maps without regions ------------------------------------------------

facet_labels <- c(
  "dsls" = "IEP spring Kodiak trawl",
  "edsm" = "USFWS enhanced Delta smelt\nmonitoring program"
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = monitoring_locations,
    aes(color = factor(month), size = location_type)
  ) +
  scale_color_manual(
    values = month_colors,
    labels = c("January", "February", "March", "December", "year-round (fixed)"),
    na.value = black
  ) +
  scale_size_manual(values = c(1.5, 0.5), guide = "none") +
  facet_wrap(
    ~ location_type,
    ncol = 2,
    labeller = labeller(location_type = facet_labels)
  ) +
  labs(
    title = "Delta smelt monitoring programs",
    subtitle = "2022",
    color = "month"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(size = 12, color = black, face = "italic"),
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
  width = 4800,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = dplyr::filter(monitoring_locations, month == 1 | is.na(month)),
    aes(color = factor(month), size = location_type)
  ) +
  scale_color_manual(
    name = "time period",
    values = month_colors,
    labels = c("January", "year-round"),
    na.value = black
  ) +
  scale_size_manual(values = c(1.5, 0.5), guide = "none") +
  facet_wrap(
    ~ location_type,
    ncol = 2,
    labeller = labeller(location_type = facet_labels)
  ) +
  labs(
    title = "Delta smelt monitoring programs",
    subtitle = "2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(size = 12, color = black, face = "italic"),
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
  filename = "delta_smelt_monitoring_map_jan.png",
  path = here::here("workshop", "delta_smelt", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4800,
  units = "px",
  bg = white
)





### create maps with regions ---------------------------------------------------

ggplot() +
  geom_sf(data = edsm_regions, aes(fill = region), color = NA) +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = monitoring_locations,
    aes(color = factor(month), size = location_type)
  ) +
  scale_fill_manual(
    name = "region",
    values = region_colors,
  ) +
  scale_color_manual(
    name = "month",
    values = month_colors,
    labels = c("January", "February", "March", "December", "December to May\n(fixed)"),
    na.value = black
  ) +
  scale_size_manual(values = c(1.5, 0.5), guide = "none") +
  facet_wrap(
    ~ location_type,
    ncol = 2,
    labeller = labeller(location_type = facet_labels)
  ) +
  labs(
    title = "Delta smelt monitoring programs",
    subtitle = "2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(size = 12, color = black, face = "italic"),
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
  filename = "delta_smelt_monitoring_map_regions_months.png",
  path = here::here("workshop", "delta_smelt", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 5200,
  units = "px",
  bg = white
)

ggplot() +
  geom_sf(data = edsm_regions, aes(fill = region), color = NA) +
  geom_sf(data = delta, fill = light_blue, color = light_blue) +
  geom_sf(
    data = monitoring_locations,
    aes(color = region_code, size = location_type)
  ) +
  scale_fill_manual(
    name = "region",
    values = region_colors
  ) +
  scale_color_manual(
    values = region_pt_colors,
    na.value = black,
    guide = "none"
  ) +
  scale_size_manual(values = c(1.5, 0.5), guide = "none") +
  facet_wrap(
    ~ location_type,
    ncol = 2,
    labeller = labeller(location_type = facet_labels)
  ) +
  labs(
    title = "Delta smelt monitoring programs",
    subtitle = "2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, color = black, face = "bold"),
    plot.subtitle = element_text(size = 12, color = black, face = "italic"),
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
  filename = "delta_smelt_monitoring_map_regions.png",
  path = here::here("workshop", "delta_smelt", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4800,
  units = "px",
  bg = white
)





### make a bar chart of samples binned by month --------------------------------

ggplot(data = edsm_samples) +
  geom_bar(mapping = aes(x = month, fill = factor(year))) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(
    name = "count of observations",
    breaks = seq(from = 0, to = 40000, by = 5000),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  labs(
    title = "Count of EDSM observations by month",
    fill = "year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "delta_smelt_monthly_bars.png",
  path = here::here("workshop", "delta_smelt", "figs"),
  device = "png",
  dpi = 700,
  height = 3000,
  width = 4800,
  units = "px",
  bg = white
)
