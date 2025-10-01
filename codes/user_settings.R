###### user settings for veggy dist

input.path <- "P:/globiom/Data/Aggregation/Collected_input_MW"


european_countries <- c(
"Austria", "Belgium",  "BosniaHerzg",  "Bulgaria", "Croatia", "CzechRep",
"Denmark", "Estonia", "Finland", "France",   "Germany",  "Greece", "Hungary",
"Ireland", "Italy", "Latvia", "Lithuania",    "Luxembourg",   "Macedonia", "Malta",
 "Netherlands",  "Poland", "Romania", "Serbia-Monte", "Slovakia", "Slovenia", "Spain",
"Sweden", "UK")

band_names <- c("weight", "BARL", "CITR", "DWHE", "FARA", "FRUI", "GRAS",
                "INDU", "MAIZ", "OATS", "OCER", "OLIV", "PARI", "POTA", "PULS",
                "RAPE", "ROOF", "RYEM", "SOYA", "SUGB", "SUNF", "SWHE", "TEXT",
                "TOBA", "VEGE", "VINY")

crop_mapping <- read.csv(paste0(input.path,"/mappings/GLOB_crop_itemmapping.csv"))
colnames(crop_mapping)[1] <- "GLOBIOM"


EPIC_shp <- readRDS(paste0(input.path,"/EPIC/MAP/EUEPIC_SimUID_1k_updt.rds"))

