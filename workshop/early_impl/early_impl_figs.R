### set up ---------------------------------------------------------------------

# source global variables and attach packages
source(here::here("workshop", "early_impl", "globals.R"))

# load objects
objects_to_load <- list.files(
  path = here::here("workshop", "early_impl", "data"),
  full.names = TRUE
)

for(object in objects_to_load) {load(object)}





### create colors --------------------------------------------------------------

white <- "#ffffff"
light_grey <- "#f7f7f7"
mid_grey <- "#d3d3d3"
dark_grey <- "#898989"
black <- "#000000"

system_colors <- c("#e94849", "#5f98c6", "#ffff5c", "#ad71b5", "#ff9933",
                   "#71bf6e", dark_grey)

watershed_colors <- colorspace::lighten(
  c("#e94849", "#5f98c6", "#ffff5c", "#ad71b5", "#ff9933", "#71bf6e", dark_grey),
  amount = 0.3
)





### create a map of projects as points -----------------------------------------

# make the map
ggplot() +
  geom_sf(data = california_cropped, fill = light_grey, color = mid_grey) +
  geom_sf(data = delta, fill = black, color = black) +
  geom_sf(data = waterbodies, fill = black, color = black) +
  geom_sf(data = flowlines, color = black) +
  geom_sf(
    data = early_impl_projects_sf,
    aes(fill = system),
    color = black,
    shape = 21,
    stroke = 1,
    size = 3
  ) +
  scale_fill_manual(
    name = "HRL system",
    values = system_colors
  ) +
  labs(title = "Early implementation projects") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 14,
      color = black,
      face = "italic",
      margin = margin(t = 5)
    ),
    axis.text = element_blank(),
    legend.title = element_text(size = 16, color = black, face = "bold"),
    legend.text = element_text(size = 14, color = black),
    panel.grid = element_blank()
  )

# write out the map
ggsave(
  filename = "early_impl_projects.png",
  path = here::here("workshop", "early_impl", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 3600,
  units = "px",
  bg = white
)





### create a map of watersheds that host projects ------------------------------

huc8 <- sf::st_join(
  x = huc8,
  y = dplyr::select(early_impl_projects_sf, system)
)

huc8 <- unique(huc8)
huc8 <- dplyr::filter(
  huc8,
  !(name == "Upper Mokelumne" & system == "Delta")
)

ggplot() +
  geom_sf(data = huc8, aes(fill = system), color = dark_grey) +
  geom_sf(data = delta, fill = black, color = black) +
  geom_sf(data = waterbodies, fill = black, color = black) +
  geom_sf(data = flowlines, color = black) +
  scale_fill_manual(
    name = "HRL system",
    values = watershed_colors,
    na.value = white,
    na.translate = FALSE
  ) +
  labs(
    title = "Early implementation watersheds",
    subtitle = "watersheds organized at the HUC8 level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = black, face = "bold"),
    plot.subtitle = element_text(
      size = 14,
      color = black,
      face = "italic",
      margin = margin(t = 5)
    ),
    axis.text = element_blank(),
    legend.title = element_text(size = 16, color = black, face = "bold"),
    legend.text = element_text(size = 14, color = black),
    panel.grid = element_blank()
  )

# write out the map
ggsave(
  filename = "early_impl_watersheds.png",
  path = here::here("workshop", "early_impl", "figs"),
  device = "png",
  dpi = 700,
  height = 4800,
  width = 4000,
  units = "px",
  bg = white
)
