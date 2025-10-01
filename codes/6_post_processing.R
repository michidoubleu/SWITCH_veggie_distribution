### use the results and transform it to be better suit the GLOBIOM narrative
## Idea: all areas with <=0.1ha of veggie are reverted to only otheragri and the other areas are scaled up to keep te targets at the same level
### input:
## res... to be transformed to data.frame before use
## cutoff, make all areas bigger than the cutoff
source("codes/functions/rebalancing.R")

res <- as.data.frame(res) %>% pivot_wider(id_cols = c(times, ns), names_from = "lu.to") %>% dplyr::select(-times)

res.corr <- rebalance_areas(res, cutoff = cutoff)


