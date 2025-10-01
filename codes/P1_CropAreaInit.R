source("codes/functions/S2_write_gms.R")
source("codes/functions/S3_read_gms.R")


simuAEZmap <- read_gms_data(paste0(input.path,"/mappings/set_simuAEZmap.gms"),header.line = 1, data.line = c(3,211672))
SimUID_map <- read_gms_data(paste0(input.path,"/mappings/SimUID_map.gms"),header.line = 30, data.line = c(32,212738))

LUID_map <- read_gms_data(paste0(input.path,"/mappings/SimUIDlu_map.gms"),header.line = 31, data.line = c(33,425446)) %>% dplyr::select(SimUID, ALLCOLROW)
LUID_map <- unique(LUID_map)


#### replace with old value based on true input
input.cols <- read_lines(paste0(input.path,"/GLOBIOM/data_CropAreaInit.gms"), n_max = 6)[2]
# Split by any number of spaces using regex
input.cols <- unlist(strsplit(input.cols, "\\s+"))
# Remove any empty elements (if needed)
input.cols <- input.cols[input.cols != ""]
old.Crop.dat <- read_fwf(paste0(input.path,"/GLOBIOM/data_CropAreaInit.gms"), skip = 2, fwf_empty(paste0(input.path,"/GLOBIOM/data_CropAreaInit.gms"), skip = 1, n=1168179, col_names = input.cols))
# Split FirstColumn into multiple new columns
old.Crop.dat <- old.Crop.dat %>%
  separate(ALLCOUNTRY.AllColRow.AltiClass.SlpClass.SoilClass.SPECIES, into = c("Country", "ColRow", "Altitude", "Slope", "Soil", "SPECIES" ), sep = "\\.")


old.Crop.dat <- old.Crop.dat %>% filter(substr(ColRow,1,2)=="CR")



new.crop <- final.res.rounded %>% pivot_longer(-ns, names_to = "SPECIES", values_to = "HI") %>% mutate(ns=as.integer(ns)) %>% rename("SimUID"="ns") %>% left_join(full.mapping) %>% filter(HI>0)

new.crop <- new.crop %>%
  # Step 3: Rename columns to match the target structure
  rename(
    Country  = ANYREGION,
    ColRow   = ColRow30,
    Altitude = AltiClass,
    Slope    = SlpClass,
    Soil     = SoilClass,
    HI       = HI
  ) %>%

  # Step 4: Reorder columns
  select(Country, ColRow, Altitude, Slope, Soil, SPECIES, HI)


full.new <-  old.Crop.dat %>% bind_rows(new.crop %>% filter(SPECIES!="OthAgr"))
replace.crops <- full.new %>% arrange(Country, ColRow, Altitude)
simu.cropland <- full.new %>% left_join(full.mapping %>% rename("Country"="ALLCOUNTRY",
                                                      "ColRow"="ColRow30",
                                                      "Altitude"="AltiClass",
                                                      "Slope"="SlpClass",
                                                      "Soil"="SoilClass")) %>% group_by(SimUID) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm=T)))


new.otheragri <- new.crop %>% filter(SPECIES=="OthAgr") %>% arrange(Country, ColRow, Altitude) %>%
  left_join(full.mapping %>% rename("Country"="ALLCOUNTRY",
                                    "ColRow"="ColRow30",
                                    "Altitude"="AltiClass",
                                    "Slope"="SlpClass",
                                    "Soil"="SoilClass")) %>% dplyr::select(-SPECIES) %>% rename("OthAgr"="HI") %>% group_by(SimUID) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm=T)))



simu.cropland <- simu.cropland %>% left_join(new.otheragri) %>% filter(SimUID %in% unique(final.res.rounded$ns))

saveRDS(simu.cropland, file = "output/croparea.rds")


replace.crops.any <- replace.crops %>% group_by(Country, Altitude, Slope, Soil, SPECIES) %>% summarise(across(where(is.numeric), ~sum(., na.rm=T))) %>% mutate(ColRow="AnyColRow")

replace.crops.LU <- replace.crops %>%
  left_join(LUID_map %>%
              left_join(full.mapping %>% mutate(SimUID=as.character(SimUID))) %>%
              dplyr::select(-SimUID) %>%
              rename("Country"="ALLCOUNTRY",
                     "ColRow"="ColRow30",
                     "Altitude"="AltiClass",
                     "Slope"="SlpClass",
                     "Soil"="SoilClass")) %>% group_by(Country, ALLCOLROW, Altitude, Slope, Soil, SPECIES) %>% summarise(across(where(is.numeric), ~sum(., na.rm=T))) %>% rename("ColRow"="ALLCOLROW")

replace.crops <- replace.crops.any %>% arrange(Country,ColRow,Altitude, Slope, Soil) %>%
  bind_rows(replace.crops.LU %>% arrange(Country,ColRow,Altitude, Slope, Soil)) %>%
  bind_rows(replace.crops %>% arrange(Country,ColRow,Altitude, Slope, Soil)) %>%
  rename("ANYREGION"="Country",
         "AllColRow"="ColRow",
         "AltiClass"="Altitude",
         "SlpClass"="Slope",
         "SoilClass"="Soil")

replace.crops <- replace.crops %>% mutate(across(where(is.numeric), ~ifelse(.==0,NA,.)))

replace.crops <- replace.crops[, c("ANYREGION", "AllColRow", "AltiClass", "SlpClass",
                                   "SoilClass", "SPECIES", "HI", "LI", "IR", "SS" )]

#### write with function, replace in globiom and run data/folder
write.gms(replace.crops,ID.col.number = 6,
          gms.type = "TABLE",
          gms.name = "CROPSAREA_INIT",
          var.name = "MngSystem",
          gms.text = "Initial crops area in 1000 ha",
          decimals = 3,
          gms_file = paste0("output/data_CropAreaInit_",gms.label,".gms"))





