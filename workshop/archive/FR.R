# library
library(EDIutils)
library(readr)
library(MESS)

# load data from https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1511.1
dat <- read_data_entity_names(packageId = "edi.1511.1")

sbrs <- read_data_entity(packageId = "edi.1511.1", entityId = dat$entityId[3])

flow_to_hab <- readr::read_csv(file = sbrs)

head(flow_to_hab)

# find Feather River commitments for instream rearing
fr_VA <- subset(flow_to_hab, scenario == "VA additional acres" & watershed == "Feather River" & habitat_type == "Instream rearing")

# plot
quantile(fr_VA$flow_cfs)

plot(fr_VA$flow_cfs, fr_VA$habitat_area_acres, type ='b',
     main = "Flow-habitat relationship in final SBR Supplement",
     xlab = "Flow (CFS)",
     ylab = "Habitat Area (acres)",
     legend = "Feather River instream rearing")
abline(v = 1237.5, lty = 3)
abline(v = 1825, lty = 3)
abline(v = 2412.5, lty = 3)

# AUC
auc(fr_VA$flow_cfs, fr_VA$habitat_area_acres, from = 650, to = 3000, type = "linear")
auc(fr_VA$flow_cfs, fr_VA$habitat_area_acres, from = 650, to = 3000, type = "spline", subdivisions = 4)
# spline = 13657.76 (numerical integral)
# linear = 8695.312 (computes the area under the curve using the composite trapezoid rule)

