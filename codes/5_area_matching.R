##### CODE 4: Area target matching with downscalR

#### Author: Michael Wögerer
#### Date: 08/01/2025

init.areas <- curr.SimUs %>% filter(LC.class=="OthAgri")

tot.area <- sum(init.areas$value)

ds.priors <- save.priors
ds.priors <- melt(
  ds.priors,
  id.vars = "SimuID",       # Keep CELLCODE as an identifier
  variable.name = "lu.to",  # Name for the variable column
  value.name = "value"        # Name for the value column
)

ds.priors <- as.data.frame(ds.priors)
ds.priors$lu.to <- as.character(ds.priors$lu.to)
colnames(ds.priors)[1] <- "ns"

available.priors <- unique(ds.priors$lu.to)


curr.targets <- targets.raw %>% filter(country==ccc) %>% ungroup() %>%
  dplyr::select(-country) %>% na.omit() %>% filter(CROP %in% available.priors)
tot.target <- sum(curr.targets$value)

if(tot.area-tot.target<0){ warning("Problem with total area OtherAgri smaller than 0!")
  curr.targets$value <- curr.targets$value*(1-abs((tot.area-tot.target)/sum(curr.targets$value)))
  tot.target <- sum(curr.targets$value)
}

ds.targets <- data.frame(times="2000", curr.targets)

ds.targets <- ds.targets %>% bind_rows(data.frame(times="2000",CROP="OthAgr", value=tot.area-tot.target)) %>% rename("lu.to"="CROP")


ds.start.areas <- init.areas %>% dplyr::select(SimUID, value) %>% as.data.frame() %>% rename("ns"="SimUID")



### fix to correct very small negative diff to one other target
if(ds.targets$value[ds.targets$lu.to=="OthAgr"]<0){
  ds.targets$value[which(ds.targets$value>0)[1]] <- ds.targets$value[which(ds.targets$value>0)[1]] + ds.targets$value[ds.targets$lu.to=="OthAgr"]
  ds.targets$value[ds.targets$lu.to=="OthAgr"] <- 0
} else {
  diff <- sum(ds.targets$value)-sum(ds.start.areas$value)
  ds.targets$value[ds.targets$lu.to=="OthAgr"] <- ds.targets$value[ds.targets$lu.to=="OthAgr"] - diff
}

### fix to check if priors are available for all ns
if(length(intersect(unique(ds.start.areas$ns),unique(ds.priors$ns)))!=max(length(unique(ds.priors$ns)),length(unique(ds.start.areas$ns)))){
missing <- setdiff(ds.start.areas$ns, ds.priors$ns)
to.add <- expand.grid(ns=missing,lu.to=unique(ds.priors$lu.to))
to.add$value <- ifelse(to.add$lu.to=="OthAgr",1,0)
ds.priors <- ds.priors %>% bind_rows(to.add)
}





ds.targets <- ds.targets %>% filter(value!=0)


test <- downscale(ds.targets, ds.start.areas, xmat = NULL,betas = NULL ,priors = ds.priors %>% filter(lu.to%in%ds.targets$lu.to), options=downscale_control(cutoff=1.0e-5, area_cutoff=T))
res <- test$out.res <- as.data.table(test$out.res)


if(round(sum(ds.targets$value),2)!=round(sum(res$value),2)){
  warning("Country ", ccc, " has an issue")
  test <- downscale(ds.targets, ds.start.areas, xmat = NULL,betas = NULL ,priors = ds.priors %>% filter(lu.to%in%ds.targets$lu.to) %>% mutate(value=ifelse(value==0,0.0000001,value)), options=downscale_control(cutoff=1.0e-5, area_cutoff=T))
}

# res %>% group_by(lu.to) %>% summarise(sum(value))
# sum(res$value)
#
# # Group by `ns` and `lu.to`, then sum the `value`
# result <- test$out.res[, .(sum_value = sum(value)), by = .(ns, lu.to)]
# result.table <- dcast(
#   result,
#   ns ~ lu.to,
#   value.var = "sum_value",
#   fun.aggregate = sum # Handle duplicate entries by summing values
# )
#
# other.agri.res <- result.table %>% dplyr::select(ns, OthAgr)
# final.alloc <- other.agri.res %>%
#   # Rename to match target structure
#   rename(CELLCODE = ns, value = OthAgr) %>%
#
#   # Add fixed or placeholder columns
#   mutate(
#     LUM.class = "OthAgr",
#     lu.to     = "OthAgr"
#   ) %>%
#
#   # Reorder columns
#   select(CELLCODE, LUM.class, lu.to, value)
#
#
# # Calculate column sums for all numeric columns
# col_sums_DS <- result.table[, lapply(.SD, sum), .SDcols = is.numeric]
#
