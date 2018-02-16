#### Estimation of peak passage dates.
## David Hope
## December 8,2016


# Import cleaned data
# Data cleaned and stored in rds files in data.clean.r, CRD_HART_data.r
# HART_species_composition.R,
# and CRD_species_composition.R
# Data availability for sites are 
# described in the README.md document

Hart_all <- readRDS("./.data/CRD_pred.rds") %>% bind_rows(readRDS("./.data/Hart_early.rds"))
spring.counts <- readRDS("./.data/spring.counts.rds") %>% bind_rows(Hart_all)
spring.counts.w.supp <- readRDS("./.data/spring.counts.w.supp.rds") %>% bind_rows(Hart_all)

# summaryofcounts <- spring.counts.w.supp %>% group_by(Year, SiteID) %>% 
#   summarize(max.WESA = max(WESA, na.rm=T),
#             mean.WESA = max(WESA, na.rm=T),
#             sum.WESA = sum(WESA, na.rm=T),
#             max.DUNL = max(DUNL, na.rm=T),
#             mean.DUNL = max(DUNL, na.rm=T),
#             sum.DUNL = sum(DUNL, na.rm=T))
# write_rds(summaryofcounts, "./datafiles/summarycounts.rds")

source("muEstimation.R")

# Set range to apply function accross
yr.range      <- sort(unique(spring.counts.w.supp$Year)) # year range
sites         <- unique(spring.counts.w.supp$SiteID) # sites
wesa.dun      <- c('WESA', 'DUNL') # Species of interest
yr.site.grid  <- expand.grid(sites,yr.range, wesa.dun) # expanded grid of all crosses between two


######### Run the function for all site, years and species
full_output <- apply( yr.site.grid ,1, function(x,y,z) calc.peak.prog(x[1], x[2], data = spring.counts.w.supp, species = x[3], errorMethod = "jackknife"))


############ Clean up and export the results
output.clean <-
  full_output[full_output != "Insufficient Data"] %>%
  .[. != 'Insufficient Number of Birds'] %>%
  .[. != 'Error in year'] %>% .[!is.null(.)]

 for (i in seq(1, length(output.clean))){
 	if(length(output.clean[[i]])!=2 ){next}
 	if (!exists("jack.df") ) {
 		jack.df <- output.clean[[i]]$jackknife
 		results.df <- output.clean[[i]]$results
 	} else{
 		jack.df <- rbind(output.clean[[i]]$jackknife, jack.df)
 		results.df <- rbind(output.clean[[i]]$results, results.df)
 	}}


saveRDS(results.df, './.data/MuEstimates_w_Hart_w_supp.rds')

saveRDS(jack.df, "./datafiles/jackknifeResults_w_Hart_w_supp.rds")


