# Interpolation of missing data from Hartney Bay, AK
# Script modified by Mark Drever and David Hope from one written by
# Mark Drever for Drever et al. 2014 Journal of Field Ornithology

require(lme4)
require(lattice)
require(tidyverse)
## read in data
source('CRD_Hart_data.r')
crd <- CopperDat %>% ungroup %>% 
  mutate(Date = arm::rescale(Day.of.Year),
         Totals = WESA + DUNL,
         p.WESA = WESA/ Totals,
         p.DUNL = DUNL / Totals,
         prop.WESA = ifelse(is.na(p.WESA),  NA, p.WESA),
         prop.DUNL = ifelse(is.na(p.DUNL), NA, p.DUNL),
         Year.factor = as.factor(Year))

## WESA
fulldf <- complete(crd,Day.of.Year = seq(121, 137), Year = 2013:2016) %>% select(-Date) %>% 
  left_join(distinct(select(crd, Date, Day.of.Year)), by = "Day.of.Year") %>% 
  filter(!is.na(Day)) %>% mutate(Date= arm::rescale(Day.of.Year))

with(crd, plot(Date, prop.WESA))

prop.model1 <- lmer(prop.WESA ~ Date + I(Date^2) + (Date + I(Date^2)|Year.factor), data = crd)
prop.model2 <- lmer(prop.DUNL ~ Date + I(Date^2) + (Date + I(Date^2)|Year.factor), data = crd)
# augM1 <- broom::augment(prop.model1, fulldf)
fulldf$pred <- predict(prop.model1, newdata = fulldf, allow.new.levels = TRUE)  
fulldf$pred.D <- predict(prop.model2, newdata = fulldf, allow.new.levels = TRUE)  

CRD <- fulldf %>% filter(!is.na(WESA)) %>%  mutate(
  bounded_prediction = ifelse(pred <0, 0, ifelse(pred>1, 1, pred)),
  bounded_prediction.d = ifelse(pred.D <0, 0, ifelse(pred.D>1, 1, pred.D)),
  predWesa = round(WESA + bounded_prediction * WLD + WL),
  predDunl = round(DUNL + bounded_prediction.d * WLD ) ) %>% 
  dplyr::select(Year, Day.of.Year, SiteID, Site, predWesa, predDunl)

saveRDS(CRD %>% rename(WESA = predWesa, DUNL = predDunl), file = ".data/CRD_pred.rds")


