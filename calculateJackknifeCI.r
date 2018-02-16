require(tidyverse)
# Data calculated using muEstimation.R 
jack.all <- readRDS("./cleanVersions/datafiles/jackknifeResults_w_Hart_w_supp.rds") 
mu.all <- readRDS("./cleanVersions/datafiles/MuEstimates_w_Hart_w_supp.rds") 


estimateJackCI <- function(s,y,sp, jack, mu) {
  jack.df <- filter(jack, site == s & year == y & species == sp)
  
  mu.df <- filter(mu, SITE == s & YEAR == y & SPECIES == sp)
  if(nrow(mu.df) <1){return(NA)}
  N <- jack.df %>% filter(!is.na(mu)) %>% nrow
  # print(N)
  if(N <= 1) {return(tibble(SITE = s, YEAR = y, SPECIES = sp,MU = mu.df$MU, lci = NA, uci=NA, N, 
                            var.i=NA, var2=NA, ci=NA, unbiasedCI = NA,
                            unbiased_lci = NA, unbiased_uci = NA))  }
  theta <- mu.df$MU * N - (N -1) * jack.df$mu
  unbiased_theta <- mu.df$MU + (N-1)*(mu.df$MU - jack.df$mu)
  var.i <- 1/(N-1) * sum((theta-mu.df$MU)^2, na.rm = T)
  var2 <- var(theta, na.rm=T)
  ci <- 1.96*sqrt(var.i)
  unbiasedCI <- qt(0.975,N-1)*sqrt(var(unbiased_theta, na.rm = T)/N)
  lci <- mu.df$MU - qt(0.975,N-1)*sqrt(var(theta, na.rm = T)/N)
  uci <- mu.df$MU + qt(0.975,N-1)*sqrt(var(theta, na.rm = T)/N)
  unbiased_lci <- mu.df$MU - unbiasedCI
  unbiased_uci <- mu.df$MU + unbiasedCI
  return(tibble(SITE = s, YEAR = y, SPECIES = sp,MU = mu.df$MU, lci, uci, N, var.i, var2, ci, unbiasedCI,
                unbiased_lci, unbiased_uci))
}

require(purrr)
yrs <- unique(mu.all$YEAR)
species <- unique(mu.all$SPECIES)
sites <- unique(mu.all$SITE)
runs <- expand.grid(sites, yrs, species)
results <- apply(runs,1,function(x,y,z) estimateJackCI(x[1], x[2], x[3], jack = jack.all, mu = mu.all))
res.df <- results[!is.na(results)] %>% do.call('rbind', .)

