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





### create a map ---------------------------------------------------------------

# make the map
ggplot() +
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
    name = "River",
    values = system_colors
  ) +
  labs(title = "Early implementation projects") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, color = black, face = "bold"),
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

