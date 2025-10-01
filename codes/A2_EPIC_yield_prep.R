##### CODE 5: EPIC yields prepare

#### Author: Michael Wögerer
#### Date: 07/02/2025


YLDG_files <- list.files(paste0(input.path,"/EPIC/VEGET/"), full.names = T)
YLDG_files <- grep("ReadMe",YLDG_files,inv=TRUE,value=TRUE)
YLDG_files <- YLDG_files[grep(".txt",YLDG_files)]
YLDG <- YLDG_files[1]
final_yields <- NULL
for (YLDG in YLDG_files) {
  temp <- read.csv(paste0(YLDG))

  if(length(grep("BAU", YLDG))==1){
    SYS <- "LI"
  } else {
    SYS <- "HI"
  }

  temp <- temp %>% filter(YR%in%c(1995:2005)) %>% group_by(SimUID, CROP) %>% summarise(YLD=mean(YLD), IRGA=mean(IRGA)) %>% ungroup() %>% mutate(SYS=SYS)

  final_yields <- bind_rows(final_yields, temp)
}

saveRDS(final_yields, file="output/EPIC_veget_yields.rds")

