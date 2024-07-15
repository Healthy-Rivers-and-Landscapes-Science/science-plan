##########################################################
# Created by: Rosemary Hartman (Rosemary.Hartman@water.ca.gov) & Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 07/08/2024
# Description: This script creates the figures in the Science Plan (except Fig. 7, which is published...)
# Plots were drafted by Rosemary Hartman and revised by Pascale Goertler
#########################################################

# library ----
library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(devtools)
devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)
library(readxl)
library(ggspatial)
remotes::install_github("flowwest/CDECRetrieve")
library(CDECRetrieve)
library(dataRetrieval)

# data ----
load("data/wetlands.RData")

FRP = FRP%>%
  filter(Project_na != "Prospect Island",  Project_na != "Protero", Project_na != "Rush Ranch") %>%
  mutate(site_type = case_when(Project_na == "Webb Tract Islands and Berms" ~ "Reference",
                               TRUE ~ site_type))
FRP2 = filter(FRP, site_type != "Reference", Project_na != "Prospect Island",  Project_na != "Protero")


## points for fish map ----
FRPstasFish = st_centroid(FRP) %>%
  mutate(Source = "FRP") %>%
  filter(!Project_na %in% c("Rush Ranch", "Prospect Island", "Protero")) %>%
  st_transform(crs = 4326)

## fish surveys ----
fishpoints = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1075.1&entityid=79240c490fe74543da6b86a1c7c751b9")

fishpts = filter(fishpoints, Date > as.Date("2020-01-01")) %>%
  select(Source, Station, Latitude, Longitude) %>%
  filter(Source != "EDSM", !is.na(Latitude)) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

## Yolo ----
Yolo = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc")

Yolopts = filter(Yolo, PeriodOfRecordTo == "Present") %>%
  rename(Station = StationCode) %>%
  mutate(Source = "Yolo") %>%
  select(Source, Station, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

## extra 20mm and sls stations ----
Extra = read_excel("data/SPB extra stations + Map VM.xlsx", sheet = "Cleanstations")%>%
  mutate(Station = as.character(Station)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

fishpts = bind_rows(fishpts, Yolopts, Extra, FRPstasFish)

## invertebrates ----
zooperstas = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.539.4&entityid=343cb43b41eb112ac36b605f1cff1f92")
#Yoloinverts = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.2&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc")
benthicinverts = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.3&entityid=4e6948186ad756dc2b6de4de41b601f3")

zooperstas = mutate(zooperstas, type = "Zooplankton") %>%
  filter(!is.na(Longitude), Source != "FRP") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
EMPbenth = rename(benthicinverts, Station = StationCode) %>%
  filter(Period_of_Record_To == "Present") %>%
  mutate(Source = "EMP", type = "Benthic")%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


FRPstas = st_centroid(FRP) %>%
  mutate(type = "Zooplankton", Source = "FRP") %>%
  filter(!Project_na %in% c("Rush Ranch", "Prospect Island", "Protero"))%>%
  st_transform(crs = 4326)
FRPstas2 = st_centroid(FRP) %>%
  mutate(type = "Benthic", Source = "FRP") %>%
  st_jitter(factor = 0.02)%>%
  filter(!Project_na %in% c("Rush Ranch", "Prospect Island", "Protero")) %>%
  st_transform(crs = 4326)

inverts = bind_rows(zooperstas, EMPbenth, FRPstas, FRPstas2)

## cdec stations ----
counties = c("Sacramento", "San Joaquin", "Yolo", "Solano", "Contra Costa", "San Joaquin",
             "Napa", "Marin", "Sonoma", "San Francisco", "San Mateo", "Alameda")

cdec = cdec_stations(county = "Sacramento") %>%
  select(-`elevation`)

for(i in 2:length(counties)){
  cdecx = cdec_stations(county = counties[i]) %>%
    select(-`elevation`)

  cdec = bind_rows(cdec, cdecx)
}

cdec = filter(cdec, longitude !=0, longitude > -130, latitude <40)

cdecsf = st_as_sf(cdec, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_intersection(st_buffer(WW_Delta, 20))  %>%
  mutate(agency = case_when(operator == "US Geological Survey" ~ "USGS",
                            operator == "USGS/DWR" ~ "USGS/DWR",
                            TRUE ~ "DWR"))

## flow stations ----
flowstas = c("FAL", "JTR", "MDM", "OBI", "ODM", "OH4", "OLD", "OMR",
             "OSJ", "TOE", "ULC", "VCU", "BDT", "DAR", "DLC", "DSJ", "DWS",
             "FAL", "FCT", "FPT", "GES", "GLC", "CLE", "CSS", "HLT", "HOL",
             'HSP', "HWB", "JTR", "LIB", "LIS", "LPS", "M13", "MAB", "MAL", "MFR",
             "MIR", "MOK", "MRB", "MRU", "MSD", "NAP", "NMR", "NSL", "OBI",
             "ODM", "OH1", "ORI", "ORM", "ORX", "ORQ", "PDC","PIL",
             "PRF", "PRI", "PTF", "RRI", "RYI", "SDC", "SDI",
             "SGA", "SGG", "SJC")

flows = cdec_stations(station_id =flowstas[1]) %>%
  select(-`elevation`)

for(i in 2:length(flowstas)){
  cdecx = cdec_stations(station_id =flowstas[i]) %>%
    select(-`elevation`)

  flows = bind_rows(flows, cdecx)
}

flowssf = st_as_sf(flows, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_intersection(st_buffer(WW_Delta, 20)) %>%
  mutate(agency = case_when(operator == "US Geological Survey" ~ "USGS",
                            operator == "USGS/DWR" ~ "USGS/DWR",
                            TRUE ~ "DWR"))

#are there any other stations on NWIS that aren't on here?
NWISstations = whatNWISsites(bBox = c(-122.5, 37.5, -121.0, 38.5))
NWISsta = whatNWISdata(siteNumber = NWISstations$site_no, service = "iv")


NWISsf = filter(NWISsta, site_tp_cd %in%c("ES", "LK", "ST",
                                          "ST-CA", "ST-DCH",
                                          "ST-TS")) %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"),
           crs = 4326) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_intersection(st_buffer(WW_Delta, 20))

# figure 5 ----
ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
   geom_sf(data = FRP2, aes(fill = site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#E69F00","#009E73"),
                    labels = c("Open water","Planned tidal wetland \nrestortion (BiOp Mitigation)",
                               "Completed tidal \nwetland restoration",  "Existing tidal wetlands"),
                    name = NULL)+
 geom_sf(data = fishpts, aes(shape = Source))+
  scale_shape_manual(values = c(16:18, 20:25, 10), name = "Sampling program")+
 # scale_color_brewer(palette = "Set3")+
  coord_sf(ylim = c(37.45, 38.55), xlim = c(-122.55, -121.2))+
  #scalebar(transform = T, dist = 10, dist_unit = "km", y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  #north(y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill = guide_legend(byrow = TRUE))+
  annotation_scale()+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))+
  guides(shape = guide_legend(order = 1))

ggsave("figures/figure_5.png", device = "png", width = 8, height = 7, dpi=700)

# figure 4 ----
ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
  geom_sf(data = FRP2, aes(fill = site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#E69F00","#009E73"),
                    labels = c("Open water","Planned tidal wetland \nrestortion (BiOp Mitigation)",
                               "Completed tidal \nwetland restoration",  "Existing tidal wetlands"),
                    name = NULL)+
  geom_sf(data = inverts, aes(shape = Source, color = type))+
  scale_shape_manual(values = c(16:18, 21:26), name = "Sampling program")+
  scale_color_manual(values = c("#D55E00", "#CC79A7"), labels =c("Zooplankton", "Benthic" ), name = "Type of sampling")+
  coord_sf(ylim = c(37.45, 38.55), xlim = c(-122.55, -121.2))+
  #scalebar(transform = T, dist = 10, dist_unit = "km", y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  #north(y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill = guide_legend(byrow = TRUE))+
  annotation_scale()+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))+
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))


ggsave("figures/figure_4.png", device = "png", width = 8, height = 7, dpi=700)


# figure 3 ----
## suisun (c) ----
plot_c <- ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
  geom_sf(data = FRP, aes(fill = site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#56B4E9", "#E69F00","#009E73"),
                    labels = c("Open water","Planned tidal wetland \nrestoration (FRP BiOp Mitigation)",
                               "FRP Reference Sites",
                               "Completed tidal \nwetland restoration",  "Other existing tidal wetlands"),
                    name = NULL)+
  coord_sf(ylim = c(38, 38.25), xlim = c(-122.2, -121.8))+
  #scalebar(transform = T, dist = 10, dist_unit = "km", y.min = 38, y.max = 38.3, x.min = -122.2, x.max = -121.8)+
  #north(y.min = 38, y.max = 38.3,  x.min = -122.2, x.max = -121.8)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill = guide_legend(byrow = TRUE))+
  annotation_scale() +
  #ggspatial::annotation_north_arrow(
  #  location = "tr", which_north = "true",
  # pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in")) +
  annotate("text", x = -122.2, y = 38.25, label= "C") +
  theme(legend.position = "none")

#ggsave("VASuisunmap.tiff", device = "tiff", width = 8, height = 7, dpi=700)

## confluence (b) ----
plot_b <- ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
  geom_sf(data = FRP, aes(fill =  site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#56B4E9", "#E69F00","#009E73"),
                    labels = c("Open water","Planned tidal wetland \nrestoration (FRP BiOp Mitigation)",
                               "FRP Reference Sites",
                               "Completed tidal \nwetland restoration",  "Other existing tidal wetlands"),
                    name = NULL)+
  coord_sf(ylim = c(38, 38.15), xlim = c(-121.95, -121.55))+
  #scalebar(transform = T, dist = 5, dist_unit = "km", y.min = 38, y.max = 38.3, x.min = -121.95, x.max = -121.55, st.size = 3)+
  #north(y.min = 38, y.max = 38.3,  x.min = -121.95, x.max = -121.5)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill = guide_legend(byrow = TRUE))+
    annotation_scale()+
  #ggspatial::annotation_north_arrow(
  #  location = "tr", which_north = "true",
  #  pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in")) +
  annotate("text", x = -121.95, y = 38.15, label= "B") +
  theme(legend.position = "none")

#ggsave("VAConfmap.tiff", device = "tiff", width = 8, height = 7, dpi=700)

## north delta (a) ----
plot_a <- ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
  geom_sf(data = FRP, aes(fill =  site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#56B4E9", "#E69F00","#009E73"),
                    labels = c("Open water","Planned tidal wetland \nrestoration (FRP BiOp Mitigation)",
                               "FRP Reference Sites",
                               "Completed tidal \nwetland restoration",  "Other existing tidal wetlands"),
                    name = NULL)+
  coord_sf(ylim = c(38.2, 38.4), xlim = c(-121.85, -121.55))+
  #scalebar(transform = T, dist = 5, dist_unit = "km", y.min = 38.2, y.max = 38.4, x.min = -121.95, x.max = -121.55, st.size = 3)+
  #north(y.min = 38.2, y.max = 38.4,  x.min = -121.85, x.max = -121.5)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill = guide_legend(byrow = TRUE))+
  annotation_scale() +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in")) +
  annotate("text", x = -121.85, y = 38.4, label= "A")

#ggsave("VANorthmap.tiff", device = "tiff", width = 8, height = 7, dpi=700)

## stack plots ----
png("figures/figure_3.png", width = 8, height = 11, units = "in", pointsize = 18,
    bg = "white", res = 350)

plot_a + plot_b + plot_c +
  plot_layout(ncol = 1)

dev.off()

# figure 8 ----
ggplot()+
  geom_sf(data = WW_Delta,  color = "#0072B2", aes(fill = "Open Water"))+
  geom_sf(data = DARItw, fill = "#009E73", color = "#009E73")+
  geom_sf(data = BARItw, color = "#009E73", aes(fill = "Tidal Wetlands"))+
  geom_sf(data = FRP2, aes(fill = site_type))+
  scale_fill_manual(values = c("#0072B2","#F0E442", "#E69F00","#009E73", "#D55E00", "#000000"),
                    labels = c("Open water","Planned tidal wetland \nrestortion (BiOp Mitigation)",
                               "Completed tidal \nwetland restoration",  "Existing tidal wetlands", "Flow", "Water Quality"),
                    name = NULL)+
  geom_sf(data = NWISsf, shape = 24, fill = "#000000") +
  geom_sf(data = cdecsf, aes(shape = agency), fill = "#000000") +
  geom_sf(data =flowssf, aes(shape = agency), fill = "#D55E00")+
  scale_shape_manual(values = c(21, 24, 25), name = NULL)+
  #scale_color_manual(values = c("#D55E00", "#000000"), labels =c("Flow", "Water Quality" ), name = NULL)+
  coord_sf(ylim = c(37.45, 38.55), xlim = c(-122.55, -121.2))+
  #scalebar(transform = T, dist = 10, dist_unit = "km", y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  #north(y.min = 37.5, y.max = 38.5, x.min = -122.5, x.max = -121.2)+
  theme_bw()+ theme(legend.spacing.y = unit(.3, "cm")) + ylab(NULL) + xlab(NULL)+
  guides(fill=guide_legend(override.aes=list(shape=21)), color = "none")+
  annotation_scale() +
  #guides(fill = guide_legend(override.aes = list(shape = 21)),
  #       shape = guide_legend(override.aes = list(fill = "black") ) ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))+
  guides(shape = guide_legend(override.aes = list(fill = "black"), order = 1))

ggsave("figures/figure_8.png", device = "png", width = 8, height = 7, dpi=700)
