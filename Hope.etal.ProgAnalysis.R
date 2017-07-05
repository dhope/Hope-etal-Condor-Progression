## Analysis of trends in peak passage estimates
## David Hope
## December 8, 2016
###################################
options(warn = -1)
require(tidyverse)
require(cowplot)

source("calculateJackknifeCI.r")
# Enter location information and distance data.
siteLat <- data.frame(SITE=c('HART', 'CRD', 'KABA', 'RBBP', 'TOFN', 'KENN'),#sites, 
                      Lat=c(60.50, 60.27,59.62,49.05,49.12,47.10), 
                      Lon=c(-145.86, -145.17,-151.46,-123.14,-125.88,-123.08),
                      Bethel = c(868, 910,580, 2740, 2590,  2900),
                      Emmonak = c(1018,1060, 780, 2930, 2790, 3100),
                      Nome = c(1095, 1120, 880,2994, 2860, 3170 )) %>% 
				rowwise %>% 
  				mutate(d2Breeding = mean(c(Bethel, Emmonak, Nome)))


mu <- readRDS("./cleanVersions/datafiles/MuEstimates_w_Hart_w_supp.rds") %>% 
	  mutate(Year=as.numeric(as.character(YEAR))) %>%
	  left_join(siteLat, by = "SITE") %>% 
	  mutate(SD = exp(SIGMA), # Convert estimate to SD
	         row_n = row_number()) %>% 
	  rename(lci_old =lci, uci_old = uci) %>%  # Remove incorrect uci estimates
	  left_join(jackknife_estimates,		   # Add jackknife estimates of error
	  	by = c("SITE", "YEAR", "SPECIES", "MU")) %>% 
	  mutate(Year = as.numeric(YEAR)) %>% arrange(SPECIES, Year, Lat) %>%
	  filter(!is.na(uci) & !is.na(lci) & N >=7) %>% filter(iter < 500)  
	  # Remove points where estimate did not coverge or too few counts were conducted


wesa <- filter(mu, SPECIES == "WESA") #& err < 25)
dunl <- filter(mu, SPECIES == "DUNL" ) #& err < 25)

## ---------  Western Sandpiper Analysis ---------------
## The script here is used to describe the WESA analysis to run the DUNL
## analysis replace 'wesa' for 'dunl' below.

# create a linear mixed model of lat by year with siteid and random effect
# fit model
library(lme4)

## Remove outlier described in text
wesa <- filter(wesa, SITE != "KENN" | !Year %in% c(2007, 2016 )) %>% 
			arrange(desc(d2Breeding), SITE, Year)


## --- Check assumptions ----

## 1. Outliers Y & X
ggplot(wesa, aes(row_n, MU)) + geom_pointrange(aes(y=MU,ymin=lci, ymax= uci )) 


ggplot(wesa, aes(row_n, MU)) + facet_wrap(~SPECIES, scales = "free") + geom_point() + coord_flip()




library(lattice)
Z <- cbind(wesa$MU, wesa$Year,  wesa$Lat)

colnames(Z) <- c("Mu", "Year", "Latitude")

dotplot(Z, groups = NULL,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

## 2. Homogeneity Y

ggplot(wesa, aes(SITE, MU)) + geom_boxplot()
ggplot(wesa, aes(y=MU, group = Year, x = Year)) + geom_boxplot()

## 3. Normality Y

p_norm <- ggplot(wesa, aes(MU)) + geom_histogram(binwidth = 1)
p_norm
p_norm + facet_grid(SITE~.)



## 4. Zero toruble Y
# Not an issue
## 5. Colinearity X
library(corrgram)
corrgram(dplyr::select(wesa, Lat, Year, MU), order=TRUE, lcier.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
## 6. Relationships Y & X

ggplot(wesa, aes(Year, MU)) + geom_pointrange(aes(y=MU,ymin=lci, ymax= uci )) 
ggplot(wesa, aes(Lat, MU)) + geom_pointrange(position = position_jitter(width = 0.5), aes(y=MU,ymin=lci, ymax= uci )) 

## 7. Interactions
ggplot(wesa, aes(Year, MU, colour = Lat)) + #geom_point()
  geom_pointrange(position = position_jitter(width = 0.25),aes(y=MU,ymin=lci, ymax= uci )) 

ggplot(wesa, aes(Year, MU, colour = d2Breeding)) + #geom_point()
  geom_pointrange(position = position_jitter(width = 0.25),aes(y=MU,ymin=lci, ymax= uci )) 

ggplot(wesa, aes(Lat, MU, colour = Year)) + #geom_point()
  geom_pointrange(position = position_jitter(width = 0.5),aes(y=MU,ymin=lci, ymax= uci )) 


## 8 Independence Y
## Not indepenent, so used MM with random intercept of SiteID


## --- Data used for analysis  -----

data.for.analysis <- wesa %>% 
			mutate(
			       yr_standardize = arm::rescale(Year),
			       Lat_std = arm::rescale(Lat),
			       d2b.st = arm::rescale(d2Breeding) )


## Model comparisons
mixedModel_ML <- lmer(MU ~ Lat_std*yr_standardize + (1|SITE), data = data.for.analysis,  REML =F)
mixedModelNoYrEff <- lmer(MU~Lat_std + (1|SITE), data  = data.for.analysis, REML=F)
mix_null <- lmer(MU~ 1 + (1|SITE),  data = data.for.analysis, REML=F)


mixedModel_ML.d2Breeding <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis, REML =T)
mixedModelNoYrEff.d2b <- lmer(MU~d2b.st + (1|SITE), data  = data.for.analysis, REML=F)
aicTable <- bbmle::AICctab(mixedModel_ML, mixedModelNoYrEff, mix_null,  mixedModel_ML.d2Breeding,mixedModelNoYrEff.d2b ,
                           mnames = c("Full Model - Latitude", "No Year Effect- Latitude", "No Fixed Effects", 
                                      "Full Model - Distance", "No Year Effect- Distance"),
                           logLik = F, weights = TRUE, base =T)
aicRes <- stats::AIC(mixedModel_ML, mixedModelNoYrEff, mix_null,  mixedModel_ML.d2Breeding,mixedModelNoYrEff.d2b)
aicRes$mnames <- c("Full Model - Latitude", "No Year Effect- Latitude", "No Fixed Effects", 
                      "Full Model - Distance", "No Year Effect- Distance")

rsquares<- rbind(MuMIn::r.squaredGLMM(mixedModel_ML),
               MuMIn::r.squaredGLMM(mixedModelNoYrEff ),
               MuMIn::r.squaredGLMM(mix_null ),
               MuMIn::r.squaredGLMM(mixedModel_ML.d2Breeding),
               MuMIn::r.squaredGLMM(mixedModelNoYrEff.d2b)
                 ) %>% as.tibble

aicRes$N <- data.for.analysis %>% summarize(n=n()) %>% .[['n']] %>% rep(., 5)

aicResTab <- aicRes %>% bind_cols(rsquares) %>% arrange(AIC) %>% 
  mutate(AICc = AIC + (  (2*(df)*(df+1))/(N - df -1) ),
         dAICc = AICc - min(AICc),
         logSup = exp(-0.5*dAICc),
         w  = logSup / sum(logSup)) %>% 
  dplyr::select(mnames, df, N, AICc, dAICc, w, R2m, R2c) #%>% 
  
  
knitr::kable(aicResTab, digits = 2)


# Final model refit with REML
mixedModel <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis,  REML =T)

# Criticizing the model -----------------------------------------------------


library(broom)
# Add predictions and residuals to data.frame
aug_MM <- augment(mixedModel, data.for.analysis)
aug_MM$lev <- hatvalues(mixedModel)
aug_MM$pearson <- residuals(mixedModel,type="pearson")
# ggplot(aug_MM, aes(Year, lev, shape = SITE)) + geom_point()
# ggplot(aug_MM, aes(Year, pearson, shape = SITE)) + geom_point()
# ggplot(aug_MM, aes(lev, pearson, shape = SITE)) + geom_point()


# Calculate confidence Intervals
require(boot)
CIs <- confint(mixedModel, method = "boot",nsim= 1500, seed = 5735779)
# wesa.ranef <- sjPlot::sjp.lmer(mixedModel)
# sjTab <- sjPlot::sjt.lmer(mixedModel, p.kr=F)
# sjPlot::sjp.lmer(mixedModel, type = "fe")
# sjPlot::sjp.lmer(mixedModel, type = "re.qq")
# lattice::qqmath(mixedModel)
# p <- sjPlot::sjp.int(mixedModel, type = "eff", show.ci = T, int.term = "d2b.st*yr_standardize")

# re.mod <- ranef(mixedModel)

mixedModel.results <- cbind(as_tibble(CIs), "term" = rownames(CIs)) %>% 
  full_join(tidy(mixedModel), by = "term") %>% mutate(supp=TRUE)


yr.key <- data.for.analysis %>% select( yr_standardize, Year) %>% distinct
boot.strap <- data.for.analysis %>% modelr::bootstrap(1000)
boot.models <-  map(boot.strap$strap, ~lmer(MU ~ d2b.st*yr_standardize + (1|SITE), REML=T, data = .)) 

boot.augment <- map_df(boot.models, broom::augment, .id = "id") %>% 
  left_join(yr.key, by = "yr_standardize")
  
boot.augment$SITE_F <- factor(boot.augment$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
aug_MM$SITE_F <- factor(aug_MM$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))

alpha_a = 0.05
summarizeBootstrap <- boot.augment %>% group_by(Year, SITE_F, SITE) %>% 
  summarize(median = median(.fitted),
            conf.low = quantile(.fitted, alpha_a/2),
            conf.high = quantile(.fitted, 1-alpha_a/2)) %>% ungroup



## Compare with individual site models
site_key <- siteLat %>% 
	left_join(
		dplyr::select(data.for.analysis, SITE, d2b.st) %>% distinct,
		 by = "SITE") %>% 
  filter(!is.na(d2b.st))%>% arrange(SITE)

estimates <- data.for.analysis %>% group_by(SITE) %>%
			do(mod = lm(MU~yr_standardize, data = .)) %>%
			mutate(int = coef(mod)[1], slope = coef(mod)[2], method = "Separate") %>%
			dplyr::select(-mod)
estimates$re_int <- coef(mixedModel)$SITE[,1]
estimates$mm_slope <- coef(mixedModel)$SITE[,3]
estimates$mm_interaction <- coef(mixedModel)$SITE[,4]
estimates$mm_site_slope <- site_key$d2b.st*estimates$mm_interaction + estimates$mm_slope

  

Year<- (sd(data.for.analysis$Year) *2) + mean(data.for.analysis$Year)

estimates$unstandardized <- estimates$mm_site_slope /(Year - mean(data.for.analysis$Year)) 

sample_sizes <-data.for.analysis %>% group_by(SITE) %>%
  summarise(n = n())
estimates$n <- sample_sizes$n


# ggplot(estimates, aes(x = "lm()", y = int, xend = "lmer()", yend = re_int)) +
#   geom_segment(alpha = 0.5) +
#   geom_point(aes(colour = SITE, size = n)) +
#   xlab("") + ylab("Intercept")

# ggplot(estimates, aes(x = "lm()", y = slope, xend = "lmer()", yend = mm_site_slope)) +
#   geom_segment(alpha=0.5) +
#   geom_point(aes(colour = SITE, size = n)) +
#   xlab("") + ylab("Annual slope by site")

######-------------------------------------------------------------------
###------ Predict out with all years - Described in Discussion
######-------------------------------------------------------------------
new_dat <- expand.grid(d2b.st = unique(data.for.analysis$d2b.st),
                       yr_standardize = unique(data.for.analysis$yr_standardize)) %>% #), max(data.for.analysis$yr_standardize),
                           # length.out = range.Yr )) %>% 
  left_join(yr.key, by = "yr_standardize") %>%
  left_join(site_key, by = "d2b.st")

new_dat$.fitted <- predict(mixedModel, newdata = new_dat)


new_dat[which(new_dat$Year%in% c(1992, 1994, 1995) & new_dat$SITE %in% c("CRD", "RBBP")),] %>% 
  group_by(Year) %>% summarize(diff = diff(.fitted))

graysDist <- mean(2860,3060 , 3140 )


graytoCRD.wesa <- data.frame(Year = c(1995,1995), SITE = c("CRD", "Gray"), d2Breeding = c(910,graysDist)) %>% 
  mutate(d2b.st =  (d2Breeding - mean(data.for.analysis$d2Breeding)) / (sd(data.for.analysis$d2Breeding) * 2) ,
         yr_standardize = (Year - mean(data.for.analysis$Year)) / (sd(data.for.analysis$Year) *2) )

predict_crd_1995 <- predict(mixedModel, newdata = graytoCRD.wesa[1,])
predict_grey_1995 <- fixef(mixedModel)[["(Intercept)"]] + 
  fixef(mixedModel)[["d2b.st"]] * graytoCRD.wesa$d2b.st[2] + 
  fixef(mixedModel)[["yr_standardize"]]  * graytoCRD.wesa$yr_standardize[2]+ 
  fixef(mixedModel)[["d2b.st:yr_standardize"]] * graytoCRD.wesa$d2b.st[2] * graytoCRD.wesa$yr_standardize[2] 

predict_crd_1995-predict_grey_1995



