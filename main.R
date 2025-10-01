###### main file for distributing veggies
rm(list=ls())

library(dplyr)
library(tidyr)
library(downscalr)
library(readr)
library(reshape2)
library(sf)
library(data.table)
library(terra)


source("./codes/user_settings.R")
source("./codes/1_prepare_startmap_GLOBIOM.R")
source("./codes/2_prepare_targets.R")

ccc <- "Estonia"
final.res <- NULL
for(ccc in european_countries){
  cat("I am at",ccc,"- still missing:", length(european_countries)-which(ccc==european_countries), "\n")

  curr.SimUs <- old.LU.dat %>% filter(ANYREGION==ccc)
  curr.SimU.IDs <- curr.SimUs$SimUID
  temp.SimU.shp <- SimU.map %>% filter(simuID %in% as.numeric(curr.SimU.IDs))

  source("./codes/3_load_EPIC.R")
  if(nrow(final_yields.simu)==0){next}
  source("./codes/4_prepare_priors_Baumert.R")

  source("./codes/5_area_matching.R")

  final.res <- final.res %>% bind_rows(as.data.frame(res))
}
