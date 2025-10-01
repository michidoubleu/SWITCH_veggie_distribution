#### CODE 3: prepare targets from CAPRI!

#### Author: Michael Wögerer
#### Date: 03/02/2025
country_mapping <- data.frame(
  geo = c(
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia",
    "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus",
    "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands",
    "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia",
    "Finland", "Sweden", "Iceland", "Norway", "Switzerland",
    "United Kingdom", "Bosnia and Herzegovina", "Montenegro",
    "North Macedonia", "Albania", "Serbia", "TÃ¼rkiye", "Kosovo*"
  ),
  GLOBIOM = c(
    "Belgium", "Bulgaria", "CzechRep", "Denmark", "Germany", "Estonia",
    "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus",
    "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands",
    "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia",
    "Finland", "Sweden", "Iceland", "Norway", "Switzerland",
    "UK", "BosniaHerzg", "Serbia-Monte", "Macedonia", "Albania", "Serbia-Monte",
    "Turkey", NA  # Kosovo not in your GLOBIOM list
  )
)

crop_mapping_target <- data.frame(
  crop = c(
    "fresh_veg",
    "brassicas",
    "cauli_broc",
    "brusselssprouts",
    "cabbage",
    "leafy_stalkedveg",
    "lettuces",
    "spinach",
    "tomatoes",
    "cucumbers",
    "watermelons",
    "peppers",
    "carrots",
    "onions",
    "lettuce_ug",
    "tomatoes_freshcons",
    "tomatoes_processing",
    "cucumbers_ug",
    "peppers_ug"
  ),
  CROP = c(
    NA,        # fresh_veg = too general
    "CaBr",    # brassicas -> cauliflower/broccoli group
    "CaBr",    # cauli_broc -> cauliflower/broccoli
    "CaBr",    # brusselssprouts also grouped with brassicas
    "Cabb",    # cabbage
    "LetL",        # leafy_stalkedveg = too general (could include multiple)
    "LetH",    # lettuces
    "Spin",    # spinach
    "Toma",    # tomatoes
    "Cucu",    # cucumbers
    "WatM",    # watermelons
    "Pepp",    # peppers
    "Carr",    # carrots
    "Onio",    # onions
    NA    ,    # lettuce_ug (loose leaf/under glass)
    NA    ,    # tomatoes_freshcons
    NA    ,    # tomatoes_processing (GLOBIOM doesn’t split)
    NA    ,    # cucumbers_ug
    NA        # peppers_ug
  )
)

## load CAPRI targets
targets.raw <- read.csv("input/veget_targets.csv") %>% left_join(country_mapping) %>% dplyr::select(-geo) %>% drop_na() %>% filter(year==2000) %>% left_join(crop_mapping_target) %>% dplyr::select(-crop) %>% group_by(GLOBIOM, CROP) %>% summarise(value=sum(area)/1000) %>% rename("country"="GLOBIOM")

