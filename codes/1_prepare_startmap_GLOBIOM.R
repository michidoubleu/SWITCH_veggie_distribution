##### CODE 1: prepare start areas from Linda map!

#### Author: Michael Wögerer
#### Date: 22/09/2025

full.mapping <- read.csv(paste0(input.path,"/mappings/full_mapping.csv"))

input.cols <- read_lines(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), n_max = 5)[2]
# Split by any number of spaces using regex
input.cols <- unlist(strsplit(input.cols, "\\s+"))
# Remove any empty elements (if needed)
input.cols <- input.cols[input.cols != ""]
old.LU.dat <- read_fwf(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), skip = 2, fwf_empty(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), skip = 1, n=267771, col_names = input.cols))
old.LU.dat <- old.LU.dat %>%
  separate(ALLCOUNTRY.AllColRow.AltiClass.SlpClass.SoilClass, into = c("Country", "ColRow", "Altitude", "Slope", "Soil"), sep = "\\.")


old.LU.dat <- full.mapping %>% left_join(old.LU.dat %>% rename("ALLCOUNTRY"="Country", "ColRow30"="ColRow", "AltiClass"="Altitude", "SlpClass"="Slope", "SoilClass"="Soil")) %>% dplyr::select(-c(ALLCOUNTRY, ColRow30, AltiClass, SlpClass, SoilClass)) %>% mutate(SimUID=as.character(SimUID)) %>% pivot_longer(cols = where(is.numeric), names_to = "LC.class", values_to = "value") %>% na.omit()

SimU.map <- read_sf(paste0(input.path,"/SIMU_LAEA/5arcmin_simu_world_ETRS_1989_LAEA_wsimuID.shp"))


prior <- paste0(input.path,"/priors/new/EU_expected_crop_shares_2000.tif")
prior.rast <- terra::rast(prior)