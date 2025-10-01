##### CODE 5: EPIC matiching

#### Author: Michael Wögerer
#### Date: 03/02/2025
source(paste0(input.path,"/mappings/FAO_eurostat_EPIC_GLOBIOM_crop_mapping.R"))

# # Read the shapefile (slow, but only once)
# EPIC_shp <- read_sf("./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.shp")
#
# # Save it as an RDS file
# saveRDS(EPIC_shp, file = "./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.rds")

EPIC_shp.tmp <- st_crop(EPIC_shp,ext(temp.SimU.shp))

# Ensure CRS is the same
temp.SimU.shp <- st_transform(temp.SimU.shp, st_crs(EPIC_shp.tmp))
# Perform spatial join (assign CELLCODE to each point in EPIC_shp)
EPIC_mapping <- st_join(EPIC_shp.tmp, temp.SimU.shp, left = FALSE) %>%
  select(simuID, SimUID) %>% st_drop_geometry()


YLDG_data <- readRDS(file="output/EPIC_veget_yields.rds")
YLDG_data <- YLDG_data %>% filter(SimUID %in% unique(EPIC_mapping$SimUID))

final_yields <- EPIC_mapping %>% full_join(YLDG_data %>% dplyr::select(SimUID, SYS, CROP, YLD, IRGA))
final_yields.simu <-  final_yields %>% left_join(eurostat_GLOBIOM_mapping2) %>% ungroup() %>% group_by(simuID, SYS, GLOBIOM) %>%  summarise(YLD=mean(YLD, na.rm=T), IRGA=mean(IRGA, na.rm=T)) %>% drop_na()


# temp.SimU.shp <- temp.SimU.shp %>% left_join(final_yields.simu %>% filter(GLOBIOM=="CaBr"))
# library(ggplot2)
# library(sf)
#
# # Quick map of YLD
# ggplot(temp.SimU.shp) +
#   geom_sf(aes(fill = YLD), color = NA) +
#   scale_fill_viridis_c(option = "plasma") +  # nice continuous color scale
#   theme_minimal() +
#   labs(title = "Quick check: YLD values",
#        fill = "YLD")
