##### prepares priors based on priors from Storm and yields from Juraj
extraction <- exactextractr::exact_extract(prior.rast, temp.SimU.shp, force_df=T)
names(extraction) <- temp.SimU.shp$simuID
extraction <- bind_rows(extraction, .id = "SimuID")


colnames(extraction)[c(-1,-length(colnames(extraction)))] <- band_names

extraction <- extraction %>%
  filter(weight > 0) %>%
  mutate(across(where(is.numeric), ~ .x * weight * coverage_fraction / 1000)) %>%
  group_by(SimuID) %>%
  summarise(across(where(is.numeric), ~ mean(.x))) %>% dplyr::select(-weight,-coverage_fraction)



extraction <- melt(
  extraction,
  id.vars = c("SimuID"),
  variable.name = "band_name",
  value.name = "value"
)

setDT(extraction)

# Create a named vector for the mapping: band_name -> prior
band_to_prior <- data.table(GLOBIOM=crop_mapping$GLOBIOM, band_name=crop_mapping$prior)

# Update the band_name column in extraction
extraction <- merge(extraction, band_to_prior, by = "band_name", all.x = TRUE, allow.cartesian=TRUE)

extraction[,band_name:=NULL]
extraction <- extraction[, .(value = sum(value)), by = c("SimuID","GLOBIOM")]
extraction <- na.omit(extraction)


extraction <- extraction %>% left_join(final_yields.simu %>% group_by(simuID, GLOBIOM) %>% summarise(YLD=mean(YLD)) %>% rename("SimuID"="simuID") %>% mutate(SimuID=as.character(SimuID))) %>% drop_na() %>% group_by(GLOBIOM) %>% mutate(YLD=YLD/mean(YLD, na.rm=T)) %>% mutate(value=value*YLD) %>%dplyr::select(-YLD)

# Reshape the filtered_extraction to wide format
extraction <- dcast(
  extraction,
  SimuID ~ GLOBIOM,
  value.var = "value",
  fun.aggregate = sum # Handle duplicate entries by summing values
)


#### handeling if priors are missing
check <- colSums(extraction[,-1])
to.corr <-  names(which(check==0))
jjj <- to.corr[1]
for(jjj in to.corr){
  update.prior <- final_yields.simu %>% filter(GLOBIOM==jjj) %>% group_by(simuID, GLOBIOM) %>% summarise(YLD=mean(YLD)) %>% rename("SimuID"="simuID") %>% mutate(SimuID=as.character(SimuID)) %>% group_by(GLOBIOM) %>% mutate(YLD=YLD/sum(YLD))
  extraction[,jjj] <- update.prior$YLD[match(extraction$SimuID,update.prior$SimuID)]
}

extraction[is.na(extraction)] <- 0

max.param <- max(rowSums(extraction[,-1]))


setDT(extraction)
extraction[, names(extraction) := lapply(.SD, function(x) replace(x, is.nan(x), 0))]

# Add a new column for 1 minus the rowsums of all band columns
if(max.param!=0){
  extraction[, OthAgr := max.param - rowSums(.SD, na.rm = TRUE), .SDcols = !("SimuID")]
} else {
  extraction[, OthAgr := 1]
}


extraction[OthAgr < 0, OthAgr := 0]
numeric_cols <- names(extraction)[sapply(extraction, is.numeric)]
# Calculate row sums for numeric columns
extraction[, row_sum := rowSums(.SD), .SDcols = numeric_cols]
# Normalize each numeric column by dividing by the row sum
extraction[, (numeric_cols) := lapply(.SD, function(x) x / row_sum), .SDcols = numeric_cols]
# Remove the temporary row_sum column
extraction[, row_sum := NULL]


save.priors <- extraction
