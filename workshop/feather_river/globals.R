# packages
library(CDECRetrieve)
library(colorspace)
library(cowplot)
library(dplyr)
library(EDIutils)
library(ggnewscale)
library(ggplot2)
library(here)
library(hydroloom)
library(lubridate)
library(nhdplusTools)
library(pracma)
library(readr)
library(scales)
library(sf)
library(snakecase)
library(stats)
library(stringr)
library(tibble)

# spatial global variables
global_crs <- 4269
global_geom_name <- "geometry"

# analytic global variables
moe_prop <- 0.05
