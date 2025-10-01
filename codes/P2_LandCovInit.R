final.crop <- readRDS("./output/croparea.rds")

#### load simuAEZmap

simuAEZmap <- read_gms_data(paste0(input.path,"/mappings/set_simuAEZmap.gms"),header.line = 1, data.line = c(3,211672))
SimUID_map <- read_gms_data(paste0(input.path,"/mappings/SimUID_map.gms"),header.line = 30, data.line = c(32,212738))

LUID_map <- read_gms_data(paste0(input.path,"/mappings/SimUIDlu_map.gms"),header.line = 31, data.line = c(33,425446)) %>% dplyr::select(SimUID, ALLCOLROW)
LUID_map <- unique(LUID_map)


#### replace with old value based on true input
input.cols <- read_lines(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), n_max = 5)[2]
# Split by any number of spaces using regex
input.cols <- unlist(strsplit(input.cols, "\\s+"))
# Remove any empty elements (if needed)
input.cols <- input.cols[input.cols != ""]
old.LU.dat <- read_fwf(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), skip = 2, fwf_empty(paste0(input.path,"/GLOBIOM/data_LandCovInit.gms"), skip = 1, n=267771, col_names = input.cols))
# Split FirstColumn into multiple new columns
old.LU.dat <- old.LU.dat %>%
  separate(ALLCOUNTRY.AllColRow.AltiClass.SlpClass.SoilClass, into = c("Country", "ColRow", "Altitude", "Slope", "Soil"), sep = "\\.")


old.LU.dat <- full.mapping %>% left_join(old.LU.dat %>% rename("ALLCOUNTRY"="Country", "ColRow30"="ColRow", "AltiClass"="Altitude", "SlpClass"="Slope", "SoilClass"="Soil")) %>% dplyr::select(-c(ALLCOUNTRY, ColRow30, AltiClass, SlpClass, SoilClass))



final.simu.wide <- old.LU.dat %>% left_join(final.crop) %>% mutate(
  CrpLnd_H=ifelse(is.na(HI),CrpLnd_H,HI),
  CrpLnd_L=ifelse(is.na(LI),CrpLnd_L,LI),
  CrpLnd_I=ifelse(is.na(IR),CrpLnd_I,IR),
  CrpLnd_S=ifelse(is.na(SS),CrpLnd_S,SS),
  OthAgri=ifelse(is.na(OthAgr),OthAgri,OthAgr)
) %>% dplyr::select(-HI,-LI,-SS,-IR,-OthAgr, -AezClass, -ANYREGION, -AllColRow)




final.simu.wide <- full.mapping %>% left_join(final.simu.wide) %>% dplyr::select(-AllColRow) %>% rename("AllColRow"="ColRow30")
final.simu.wide.any <- final.simu.wide %>% group_by(ALLCOUNTRY, AltiClass, SlpClass, SoilClass) %>% summarise(across(where(is.numeric), ~sum(., na.rm=T))) %>% mutate(AllColRow="AnyColRow") %>% arrange(ALLCOUNTRY,AllColRow,AltiClass, SlpClass, SoilClass) %>% mutate(across(where(is.numeric), ~ifelse(.==0,NA,.)))

final.simu.wide.LU <- final.simu.wide %>% mutate(SimUID=as.character(SimUID)) %>% left_join(LUID_map) %>% group_by(ALLCOUNTRY, ALLCOLROW, AltiClass, SlpClass, SoilClass) %>% summarise(across(where(is.numeric), ~sum(., na.rm=T))) %>% rename("AllColRow"="ALLCOLROW") %>% arrange(ALLCOUNTRY,AllColRow,AltiClass, SlpClass, SoilClass) %>% mutate(across(where(is.numeric), ~ifelse(.==0,NA,.)))

final.simu.wide <- final.simu.wide.any %>% bind_rows(final.simu.wide.LU) %>% bind_rows(final.simu.wide  %>% dplyr::select(-SimUID))



final.simu.wide <- final.simu.wide[, c(
  "ALLCOUNTRY", "AllColRow", "AltiClass", "SlpClass",
  "SoilClass", "CrpLnd", "CrpLnd_H", "CrpLnd_L", "CrpLnd_I", "CrpLnd_S",
  "OthAgri", "Grass", "Forest", "WetLnd", "OthNatLnd", "NotRel", "SimUarea"
)]


#### write with function, replace in globiom and run data/folder
write.gms(final.simu.wide,ID.col.number = 5,
          gms.type = "TABLE",
          gms.name = "LANDCOVERALL_INIT",
          var.name = "LC_TYPES_EPIC",
          gms.text = "Initial land cover (1000 ha)",
          decimals = 2,
          gms_file = paste0("output/data_LandCovInit_",gms.label,".gms"))




