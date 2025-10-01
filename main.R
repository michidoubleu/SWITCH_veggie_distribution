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
final.res.corr <- final.res <- NULL
for(ccc in european_countries){
  cat("I am at",ccc,"- still missing:", length(european_countries)-which(ccc==european_countries), "\n")

  curr.SimUs <- old.LU.dat %>% filter(ANYREGION==ccc)
  curr.SimU.IDs <- curr.SimUs$SimUID
  temp.SimU.shp <- SimU.map %>% filter(simuID %in% as.numeric(curr.SimU.IDs))

  source("./codes/3_load_EPIC.R")
  if(nrow(final_yields.simu)==0){next}
  source("./codes/4_prepare_priors_Baumert.R")

  source("./codes/5_area_matching.R")
  source("./codes/6_post_processing.R")

  final.res <- final.res %>% bind_rows(as.data.frame(res))
  final.res.corr <- final.res.corr %>% bind_rows(res.corr)
}

### original result
final.res[is.na(final.res)] <- 0
### result that has each area bigger than cutoff
final.res.corr[is.na(final.res.corr)] <- 0
### result that has each area bigger than cutoff and then rounds to 2 digits
source("./codes/functions/smartround.R")
final.res.corr.rounded <- final.res.corr
final.res.corr.rounded[,-1] <- apply(final.res.corr.rounded[,-1],2,smart.round,round.digits)
### result that rounds to 2 digits
final.res.rounded <- final.res
final.res.rounded[,-1] <- apply(final.res.rounded[,-1],2,smart.round,round.digits)


# ### preformance metrics
# colSums(final.res[,-1], na.rm=T)
# colSums(final.res.rounded[,-1], na.rm=T)
# colSums(final.res.corr[,-1], na.rm=T)
# colSums(final.res.corr.rounded[,-1], na.rm=T)
#
# sum(colSums(final.res[,-1], na.rm=T))
# sum(colSums(final.res.rounded[,-1], na.rm=T))
# sum(colSums(final.res.corr[,-1], na.rm=T))
# sum(colSums(final.res.corr.rounded[,-1], na.rm=T))


### settings are 3 digits.. could be an issue with P2 as the original rounds to 2 digits
#save.image("output/test_oct1.RData")
#load("output/test_oct1.RData")




source("./codes/P1_CropAreaInit.R")
source("./codes/P2_LandCovInit.R")


