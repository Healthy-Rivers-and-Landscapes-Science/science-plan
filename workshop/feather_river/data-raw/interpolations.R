### set up ---------------------------------------------------------------------

source(here::here("workshop", "feather_river", "globals.R"))
source(here::here("workshop", "feather_river", "functions", "interpolate_ellipse.R"))
source(here::here("workshop", "feather_river", "functions", "interpolate_sine.R"))

load(here::here("workshop", "feather_river", "data", "sbrs_flow_area.rda"))





### create a linear interpolation ----------------------------------------------
sbrs_linear_interpolation <- dplyr::mutate(
  sbrs_flow_area,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

# write out linear interpolation
save(
  sbrs_linear_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_linear_interpolation.rda")
)





### create an unanchored cubic spline interpolation ----------------------------
sbrs_unanchored_cubic_spline_interpolation <- as.data.frame(stats::spline(
  x = sbrs_flow_area$flow_cfs,
  y = sbrs_flow_area$habitat_area_acres,
  n = 1000
))

colnames(sbrs_unanchored_cubic_spline_interpolation) <- c("flow_cfs", "habitat_area_acres")

sbrs_unanchored_cubic_spline_interpolation <- dplyr::mutate(
  sbrs_unanchored_cubic_spline_interpolation,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

sbrs_unanchored_cubic_spline_interpolation <- dplyr::arrange(
  sbrs_unanchored_cubic_spline_interpolation,
  flow_cfs
)

# write out unanchored cubic spline interpolation
save(
  sbrs_unanchored_cubic_spline_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_unanchored_cubic_spline_interpolation.rda")
)





### create an anchored cubic spline interpolation ------------------------------

# add slightly arbitrary anchor points between the second two pairs of coordinates
anchor_rows <- tibble::tribble(
  ~flow_cfs, ~habitat_area_acres, ~habitat_area_acres_lower, ~habitat_area_acres_upper,
  800,       4.9,                 5 * (1 - moe_prop),        5 * (1 + moe_prop),
  1030,      5.0,                 5 * (1 - moe_prop),        5 * (1 + moe_prop),
  1400,      4.75,                5 * (1 - moe_prop),        5 * (1 + moe_prop)
)

sbrs_anchored_cubic_spline_interpolation <- dplyr::bind_rows(
  sbrs_flow_area,
  anchor_rows
)

# create the anchored cubic spline curve values
sbrs_anchored_cubic_spline_interpolation <- as.data.frame(stats::spline(
  x = sbrs_anchored_cubic_spline_interpolation$flow_cfs,
  y = sbrs_anchored_cubic_spline_interpolation$habitat_area_acres,
  n = 1000
))

colnames(sbrs_anchored_cubic_spline_interpolation) <- c("flow_cfs", "habitat_area_acres")

sbrs_anchored_cubic_spline_interpolation <- dplyr::mutate(
  sbrs_anchored_cubic_spline_interpolation,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

sbrs_anchored_cubic_spline_interpolation <- dplyr::arrange(
  sbrs_anchored_cubic_spline_interpolation,
  flow_cfs
)

# write out anchored cubic spline interpolation
save(
  sbrs_anchored_cubic_spline_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_anchored_cubic_spline_interpolation.rda")
)





### create a sine interpolation ------------------------------------------------

# create a half-cycle sine interpolation between first two coordinate pairs
first_sine_interpolation <- interpolate_sine(
  x_start = as.numeric(sbrs_flow_area[1, "flow_cfs"]),
  x_end   = as.numeric(sbrs_flow_area[2, "flow_cfs"]),
  y_start = as.numeric(sbrs_flow_area[1, "habitat_area_acres"]),
  y_end   = as.numeric(sbrs_flow_area[2, "habitat_area_acres"]),
  n = 100
)

# create a half-cycle sine interpolation between second two coordinate pairs
second_sine_interpolation <- interpolate_sine(
  x_start = as.numeric(sbrs_flow_area[2, "flow_cfs"]),
  x_end   = as.numeric(sbrs_flow_area[3, "flow_cfs"]),
  y_start = as.numeric(sbrs_flow_area[2, "habitat_area_acres"]),
  y_end   = as.numeric(sbrs_flow_area[3, "habitat_area_acres"]),
  n = 900
)

# combine into a single sine interpolation
sbrs_sine_interpolation <- rbind(
  first_sine_interpolation,
  second_sine_interpolation
)
colnames(sbrs_sine_interpolation) <- c("flow_cfs", "habitat_area_acres")

sbrs_sine_interpolation <- dplyr::mutate(
  sbrs_sine_interpolation,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

sbrs_sine_interpolation <- dplyr::arrange(sbrs_sine_interpolation, flow_cfs)

# write out sine interpolation
save(
  sbrs_sine_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_sine_interpolation.rda")
)





### create a convex elliptical interpolation -----------------------------------

# create an elliptical arc between the first two coordinate pairs
first_ellipse_interpolation <- interpolate_ellipse(
  x_start = as.numeric(sbrs_flow_area[1, "flow_cfs"]),
  x_end   = as.numeric(sbrs_flow_area[2, "flow_cfs"]),
  y_start = as.numeric(sbrs_flow_area[1, "habitat_area_acres"]),
  y_end   = as.numeric(sbrs_flow_area[2, "habitat_area_acres"]),
  n = 100,
  quadrant = "ul"
)

# create a convex elliptical arc between the second two coordinate pairs
second_ellipse_convex_interpolation <- interpolate_ellipse(
  x_start = as.numeric(sbrs_flow_area[2, "flow_cfs"]),
  x_end   = as.numeric(sbrs_flow_area[3, "flow_cfs"]),
  y_start = as.numeric(sbrs_flow_area[2, "habitat_area_acres"]),
  y_end   = as.numeric(sbrs_flow_area[3, "habitat_area_acres"]),
  n = 900,
  quadrant = "ur"
)

# combine into a single elliptical arc interpolation
sbrs_ellipse_convex_interpolation <- rbind(
  first_ellipse_interpolation,
  second_ellipse_convex_interpolation
)

colnames(sbrs_ellipse_convex_interpolation) <- c("flow_cfs", "habitat_area_acres")

sbrs_ellipse_convex_interpolation <- dplyr::mutate(
  sbrs_ellipse_convex_interpolation,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

sbrs_ellipse_convex_interpolation <- dplyr::arrange(
  sbrs_ellipse_convex_interpolation,
  flow_cfs
)

# write out convex elliptical interpolation
save(
  sbrs_ellipse_convex_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_ellipse_convex_interpolation.rda")
)





### create a concave elliptical interpolation -----------------------------------

# create a concave elliptical arc between the second two coordinate pairs
second_ellipse_concave_interpolation <- interpolate_ellipse(
  x_start = as.numeric(sbrs_flow_area[2, "flow_cfs"]),
  x_end   = as.numeric(sbrs_flow_area[3, "flow_cfs"]),
  y_start = as.numeric(sbrs_flow_area[2, "habitat_area_acres"]),
  y_end   = as.numeric(sbrs_flow_area[3, "habitat_area_acres"]),
  n = 900,
  quadrant = "bl"
)

# combine into a single elliptical arc interpolation
sbrs_ellipse_concave_interpolation <- rbind(
  first_ellipse_interpolation,
  second_ellipse_concave_interpolation
)

colnames(sbrs_ellipse_concave_interpolation) <- c("flow_cfs", "habitat_area_acres")

sbrs_ellipse_concave_interpolation <- dplyr::mutate(
  sbrs_ellipse_concave_interpolation,
  habitat_area_acres_lower = habitat_area_acres * (1 - moe_prop),
  habitat_area_acres_upper = habitat_area_acres * (1 + moe_prop)
)

sbrs_ellipse_concave_interpolation <- dplyr::arrange(
  sbrs_ellipse_concave_interpolation,
  flow_cfs
)

# write out concave elliptical interpolation
save(
  sbrs_ellipse_concave_interpolation,
  file = here::here("workshop", "feather_river", "data", "sbrs_ellipse_concave_interpolation.rda")
)




### summarize area of each interpolation ---------------------------------------

# calculate areas under the curves; areas are approximate since they are
# computed with an integral using the trapezoidal rule over x and y values, but
# all interpolated series have 1,000 observations so it should be close enough
area_linear <- pracma::trapz(
  x = sbrs_linear_interpolation$flow_cfs,
  y = sbrs_linear_interpolation$habitat_area_acres
)

area_anchored_cubic_spline <- pracma::trapz(
  x = sbrs_anchored_cubic_spline_interpolation$flow_cfs,
  y = sbrs_anchored_cubic_spline_interpolation$habitat_area_acres
)

area_sine <- pracma::trapz(
  x = sbrs_sine_interpolation$flow_cfs,
  y = sbrs_sine_interpolation$habitat_area_acres
)

area_ellipse_convex <- pracma::trapz(
  x = sbrs_ellipse_convex_interpolation$flow_cfs,
  y = sbrs_ellipse_convex_interpolation$habitat_area_acres
)

area_ellipse_concave <- pracma::trapz(
  x = sbrs_ellipse_concave_interpolation$flow_cfs,
  y = sbrs_ellipse_concave_interpolation$habitat_area_acres
)

# summarize areas under the curves
sbrs_interpolation_areas <- tibble::tribble(
  ~interpolation_method,     ~area,
  "linear",                  area_linear,
  "cubic spline (anchored)", area_anchored_cubic_spline,
  "sine curve",              area_sine,
  "ellipse arc (convex)",    area_ellipse_convex,
  "ellipse arc (concave)",   area_ellipse_concave
)

sbrs_interpolation_areas <- dplyr::mutate(
  sbrs_interpolation_areas,
  interpolation_method = factor(
    interpolation_method,
    levels = c(
      "linear",
      "cubic spline (anchored)",
      "sine curve",
      "ellipse arc (convex)",
      "ellipse arc (concave)"
    )
  ),
  area_lower = area * (1 - moe_prop),
  area_upper = area * (1 + moe_prop)
)

# write out interpolation areas
save(
  sbrs_interpolation_areas,
  file = here::here("workshop", "feather_river", "data", "sbrs_interpolation_areas.rda")
)
