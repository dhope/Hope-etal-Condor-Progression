## Analysis of trends in peak passage estimates
## David Hope
## December 8, 2016
###################################
options(warn = -1)
require(tidyverse)
require(cowplot)

setwd("/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/CWS.Mansucript/Rscripts")
## Create data.frame with Site Lat and Lon as well as distance from KENN and 
 # Estimate of Distance to breeding grounds on YK Delta
source("jack_reanalysis.r")
siteLat <- data.frame(SITE=c('HART', 'CRD', 'KABA', 'RBBP', 'TOFN', 'KENN'),#sites, 
                      Lat=c(60.50, 60.27,59.62,49.05,49.12,47.10), 
                      Lon=c(-145.86, -145.17,-151.46,-123.14,-125.88,-123.08),
                      Bethel = c(868, 910,580, 2740, 2590,  2900),
                      Emmonak = c(1018,1060, 780, 2930, 2790, 3100),
                      Nome = c(1095, 1120, 880,2994, 2860, 3170 )) %>% 
  rowwise %>% 
  mutate(d2Breeding = mean(c(Bethel, Emmonak, Nome)))
                      # d2Breeding = c(mean(c(910, 1060,1120) ), mean(580, 780, 880),
                      #                mean(2740, 2930,2994), mean(2590,2790, 2860),
                      #                mean(2900, 3100, 3170)))
                      # distance = c(2070,2320,220,310,0),
                      #d2Breeding = c(950,600,2800,2600,2900))

# othersites <- tibble(SITE = c("HART","147067", "147068")) %>% mutate(
#   Lat=60.27, 
#   Lon=-145.17,
#   Bethel = 910,
#   Emmonak = 1060,
#   Nome = 1120) %>% 
#   rowwise %>% 
#   mutate(d2Breeding = mean(c(Bethel, Emmonak, Nome)))

# siteLat <- bind_rows(siteLat, othersites)

mu <- readRDS("./cleanVersions/datafiles/MuEstimates_w_Hart_w_supp.rds") %>% 
  # filter(SITE != "CRD") %>% bind_rows(readRDS("./cleanVersions/datafiles/MuEstimates_CRD_noHART.rds")) %>% 
  #readRDS('./cleanVersions/datafiles/MuEstimates_newCRD.rds') %>% 
  # bind_rows(readRDS("./cleanVersions/datafiles/MuEstimates_Hartney.rds")) %>% 
  mutate(Year=as.numeric(as.character(YEAR))) %>% #filter(SITE != "CRD"| YEAR < 2000) %>% 
	  left_join(siteLat, by = "SITE") %>% mutate(SD = exp(SIGMA), # Convert estimate to SD
	                                             # yr_standardize = arm::rescale(as.numeric(YEAR)), # Scale Year variable
	                                             row_n = row_number()) 

mu <- mu %>% rename(lci_old =lci, uci_old = uci) %>% left_join(jackknife_estimates, by = c("SITE", "YEAR", "SPECIES", "MU")) %>% 
  mutate(Year = as.numeric(YEAR)) %>% arrange(SPECIES, Year, Lat) %>%
  filter(!is.na(uci) & !is.na(lci) & N >=7) %>% filter(iter < 500)

ggplot(mu, aes(Year, N)) + facet_wrap(~SITE) + geom_col() + geom_hline(yintercept = 7)

ggplot(mu, aes(iter, MU)) + geom_pointrange(aes(ymin=lci, ymax = uci, colour = SITE)) + 
  facet_wrap(~SPECIES) + geom_hline(yintercept = 90)


# Import supplimentary KABA data

kaba.no_supp <- readRDS('./cleanVersions/datafiles/MuEstimates_KABA_without_supp_CRD.rds') %>%
  mutate(supp=FALSE)#%>% mutate(lci = low, uci = high)


kaba.no_supp <- kaba.no_supp %>% rename(lci_old =lci, uci_old = uci) %>% 
  left_join(jackknife_estimates_KABA_No_supp, by = c("SITE", "YEAR", "SPECIES", "MU")) %>% 
  mutate(Year = as.numeric(YEAR))

# kaba.compare <- 
# 	mutate(mu, supp = FALSE) %>% filter(SITE == "KABA" & Year > 2000) %>% 
# 	bind_rows(kaba.supp)

mu_w_out_supp <- filter(mu, SITE != "KABA" | Year < 2000) %>% mutate(supp = FALSE) %>%
					dplyr::select(one_of(names(kaba.no_supp))) %>%
					bind_rows(kaba.no_supp) %>%
					left_join(siteLat, by = "SITE") %>%
			  	    mutate(SD = exp(SIGMA),
				         row_n = row_number()) %>% mutate(err = uci-lci) 

kaba.contrast <- 
  mutate(mu, supp = TRUE) %>% filter(SITE == "KABA" & Year > 2000) %>% 
  left_join(kaba.no_supp, by = c("Year", "SPECIES" ) ) %>% rename(Mu.w.supp = MU.x,
                                                               Mu.no.supp = MU.y) %>% 
  mutate(SPECIES = factor(SPECIES, 
                          levels = c("WESA", "DUNL")))
## KABA mu estimate comparisons by species
ggplot(kaba.contrast, aes(Mu.no.supp, Mu.w.supp)) + geom_point(size=3) + facet_wrap(~SPECIES, scales ="free") +
  geom_errorbar(aes( ymin=lci.y, ymax=uci.y)) +
  geom_errorbarh(aes(xmin = lci.x, xmax=uci.x)) +
  geom_abline(intercept = 0, slope =1, linetype=2) + 
  labs(y = "Peak Passage estimate \nwith supplimental counts", x = "Peak Passage estimate without supplimental counts") +
  ggthemes::theme_few()

ggplot(kaba.contrast, aes(Mu.no.supp, Mu.w.supp)) + geom_point(size=3) + facet_wrap(~SPECIES, scales ="free") +
  geom_abline(intercept = 0, slope =1, linetype=2) + lims(x = c(126,135), y =c(126,135)) +
  labs(y = "Peak Passage estimate \nwith supplimental counts", x = "Peak Passage estimate without supplimental counts") +
  ggthemes::theme_few()

# ggsave("../4th Submission/kaba.compare.png", width = 6, height = 4, dpi = 600)
# mu_w_supp$SITE[which(mu_w_supp$SITE == "CRD" & mu_w_supp$YEAR > 2000)] <- "ORCA"

wesa <- filter(mu, SPECIES == "WESA") #& err < 25)
dunl <- filter(mu, SPECIES == "DUNL" ) #& err < 25)

## ---------  Western Sandpiper Analysis ---------------


# create a linear mixed model of lat by year with siteid and random effect
# fit model
library(lme4)


## Remove outlier described in text
# wesa <- filter(wesa, MU> 137)# & MU > 112.36 )
wesa <- filter(wesa, SITE != "KENN" | !Year %in% c(2007, 2016 )) %>% arrange(desc(d2Breeding), SITE, Year)


## 1. Outliers Y & X
# wesa1 <- wesa
# wesa <- filter(wesa, MU<140 & N>5)
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



data.for.analysis <- wesa %>% #filter(wesa, MU < 137 & MU >113) %>% 
			mutate(#SITE = factor(SITE, levels = c('KENN', 'RBBP', 'TOFN', "CRD","ORCA", 'KABA')),
			       yr_standardize = arm::rescale(Year),
			       YR.Group = ifelse(yr_standardize < 0, "Early", "Late"),
			       Lat_std = arm::rescale(Lat),
			       # dist.st = arm::rescale(distance),
			       d2b.st = arm::rescale(d2Breeding),
			       err = uci - lci)

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


# data.for.analysis %>% summarize(n=n())






sjstats::r2(mixedModelNoYrEff.d2b)$r2

# Final model refit with REML
mixedModel <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis,  REML =T)


arm::display(mixedModel)

## # Criticizing the model 

# Criticizing the model -----------------------------------------------------


library(broom)
# Add predictions and residuals to data.frame
aug_MM <- augment(mixedModel, data.for.analysis)
aug_MM$lev <- hatvalues(mixedModel)
aug_MM$pearson <- residuals(mixedModel,type="pearson")
ggplot(aug_MM, aes(Year, lev, shape = SITE)) + geom_point()
ggplot(aug_MM, aes(Year, pearson, shape = SITE)) + geom_point()
ggplot(aug_MM, aes(lev, pearson, shape = SITE)) + geom_point()

## Calculate confidence Intervals
require(boot)
CIs <- confint(mixedModel, method = "boot",nsim= 1500, seed = 5735779)
wesa.ranef <- sjPlot::sjp.lmer(mixedModel)
sjTab <- sjPlot::sjt.lmer(mixedModel, p.kr=F)
sjPlot::sjp.lmer(mixedModel, type = "fe")
sjPlot::sjp.lmer(mixedModel, type = "re.qq")
lattice::qqmath(mixedModel)
p <- sjPlot::sjp.int(mixedModel, type = "eff", show.ci = T, int.term = "d2b.st*yr_standardize")

re.mod <- ranef(mixedModel)

mixedModel.results <- cbind(as_tibble(CIs), "term" = rownames(CIs)) %>% 
  full_join(tidy(mixedModel), by = "term") %>% mutate(supp=TRUE)


ggplot(aug_MM, aes(Year, .resid)) + geom_point() + facet_wrap(~SITE, scales = "free") + ggthemes::theme_few()+
		geom_hline(yintercept = 0,  colour = 'red')

ggplot(arrange(aug_MM, Year, Lat), aes(.fitted, .resid)) + geom_point() + facet_wrap(~SITE, scales = 'free') + ggthemes::theme_few()+
		geom_hline(yintercept = 0,  colour = 'red')

## Plot the results

ggplot(aug_MM, aes(Year, .fitted, colour = SITE)) + geom_line() + #geom_point(aes(y=MU)) + 
  geom_pointrange(data = data.for.analysis, aes( y = MU, ymin = lci, ymax = uci )) #+ ylim(100, 150)

yr.key <- data.for.analysis %>% select( yr_standardize, Year) %>% distinct
boot.strap <- data.for.analysis %>% modelr::bootstrap(1000)
boot.models <-  map(boot.strap$strap, ~lmer(MU ~ d2b.st*yr_standardize + (1|SITE), REML=T, data = .)) 
# options(warn=-1)
boot.augment <- map_df(boot.models, broom::augment, .id = "id") %>% 
  left_join(yr.key, by = "yr_standardize")
# options(warn=0)

# bootnls_aug <- data.for.analysis %>% modelr::bootstrap(1000) %>%
#   do(augment(lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = .,  REML =T),.)) %>% 
#   mutate(siteRep = paste0(SITE,"_", replicate)) #%>% 
  
boot.augment$SITE_F <- factor(boot.augment$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
aug_MM$SITE_F <- factor(aug_MM$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))

confint(mixedModel, method = "boot")
bootnls_aug <-  boot.augment
alpha_a = 0.05
summarizeBootstrap <- bootnls_aug %>% group_by(Year, SITE_F, SITE) %>% 
  summarize(median = median(.fitted),
            conf.low = quantile(.fitted, alpha_a/2),
            conf.high = quantile(.fitted, 1-alpha_a/2)) %>% ungroup


## Figure 2 WESA Bootstrap data 

plot_bootstrap <- ggplot(bootnls_aug, aes(Year, MU))  +
  geom_line(aes(group=id, y=.fitted), alpha = 0.05, colour = 'grey')+
  # geom_ribbon(data = summarizeBootstrap,  
  #             aes(y = median, ymin = conf.low, ymax = conf.high),  fill = 'grey', alpha=0.25) +
  geom_linerange(alpha = 0.6,
                  # position = position_dodge(width = 0.4),
                  data = aug_MM,
                  aes(ymin=MU-SD, ymax=MU+SD)
                  ) +
  geom_point(data=aug_MM)+
  geom_line(data=aug_MM, aes(y=.fitted)) +
  labs(y = "Peak passage estimate", group = "Site", fill = "Site") + facet_grid(.~SITE_F)
  # geom_line(data = bootnls_aug[which(bootnls_aug$replicate %in% sample(unique(bootnls_aug$replicate), 5, F)), ], 
  #           aes(y=.fitted, group=siteRep, colour =NULL),colour ="grey", alpha=.6) +
  
  # geom_linerange(data=aug_MM, aes(), alpha = 0.4) +
  # scale_shape_manual(values = c(19,17,15, 0,1,4)) +
   # ggthemes::theme_few() +
  
  # coord_cartesian(ylim= c(113, 135) ) + 
  

plot_bootstrap 

# 
# ggplot(aug_MM, aes(d2Breeding, .fitted)) + 
#   geom_line()

## Compare with individual site models
site_key <- siteLat %>% left_join(dplyr::select(data.for.analysis, SITE, d2b.st) %>% distinct, by = "SITE") %>% 
  filter(!is.na(d2b.st))%>% arrange(SITE)
#data.frame(
  # SITE = unique(data.for.analysis$SITE), d2b.st = unique(data.for.analysis$d2b.st)) %>% arrange(SITE)
estimates <- data.for.analysis %>% group_by(SITE) %>%
			do(mod = lm(MU~yr_standardize, data = .)) %>%
			mutate(int = coef(mod)[1], slope = coef(mod)[2], method = "Separate") %>%
			dplyr::select(-mod)
estimates$re_int <- coef(mixedModel)$SITE[,1]
estimates$mm_slope <- coef(mixedModel)$SITE[,3]
estimates$mm_interaction <- coef(mixedModel)$SITE[,4]
estimates$mm_site_slope <- site_key$d2b.st*estimates$mm_interaction + estimates$mm_slope

  
  #(estimates$mm_site_slope * 2019.538- estimates$mm_site_slope*mean(data.for.analysis$Year) ) / ( sd(data.for.analysis$Year) *2)
Year<- (sd(data.for.analysis$Year) *2) + mean(data.for.analysis$Year)

estimates$unstandardized <- estimates$mm_site_slope /(Year - mean(data.for.analysis$Year)) 

sample_sizes <-data.for.analysis %>% group_by(SITE) %>%
  summarise(n = n())
estimates$n <- sample_sizes$n


ggplot(estimates, aes(x = "lm()", y = int, xend = "lmer()", yend = re_int)) +
  geom_segment(alpha = 0.5) +
  geom_point(aes(colour = SITE, size = n)) +
  xlab("") + ylab("Intercept")

ggplot(estimates, aes(x = "lm()", y = slope, xend = "lmer()", yend = mm_site_slope)) +
  geom_segment(alpha=0.5) +
  geom_point(aes(colour = SITE, size = n)) +
  xlab("") + ylab("Annual slope by site")

range.Yr <- length(seq(max(unique(data.for.analysis$Year)) , min(unique(data.for.analysis$Year))))

# Yr_key <- data.frame(Year = seq(1985, 2016), yr_standardize = seq(min(data.for.analysis$yr_standardize), max(data.for.analysis$yr_standardize),
#                                                                   length.out = range.Yr ))


# Predict out with all years
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


distStikine <- mean(1750, 1910, 1970)

peakPassageStikine2010 <- data.frame(Year = c(2010), SITE = c("Stikine"), d2Breeding = c(distStikine)) %>% 
  mutate(d2b.st =  (d2Breeding - mean(data.for.analysis$d2Breeding)) / (sd(data.for.analysis$d2Breeding) * 2) ,
         yr_standardize = (Year - mean(data.for.analysis$Year)) / (sd(data.for.analysis$Year) *2) )

predict_stikine_2010 <- fixef(mixedModel)[["(Intercept)"]] + 
  fixef(mixedModel)[["d2b.st"]] * peakPassageStikine2010$d2b.st[1] + 
  fixef(mixedModel)[["yr_standardize"]]  * peakPassageStikine2010$yr_standardize[1]+ 
  fixef(mixedModel)[["d2b.st:yr_standardize"]] * peakPassageStikine2010$d2b.st[1] * peakPassageStikine2010$yr_standardize[1] 

merTools::predictInterval(mixedModel, peakPassageStikine2010)

# Compare Results with and without supp -----------------------------------

# mu_w_supp
# kaba.compare
# ggplot(kaba.compare, aes(as.numeric(YEAR), MU, colour = supp)) + geom_point()

data.for.analysis.nosupp <- filter(mu_w_out_supp, SPECIES == "WESA", MU<140 & MU > 90 &N > 5) %>% 
  mutate(SITE = factor(SITE, levels = c('KENN', 'RBBP', 'TOFN', "CRD", "ORCA", 'KABA')),
         yr_standardize = arm::rescale(Year),
         YR.Group = ifelse(yr_standardize < 0, "Early", "Late"),
         d2b.st = arm::rescale(d2Breeding))

mixedModel.no.supp.wesa <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis.nosupp,  REML =T)
mixed_aug <- augment(mixedModel.no.supp.wesa, data.for.analysis.nosupp)
tidy(mixedModel)
tidy(mixedModel.no.supp.wesa)

CI.nosupp <- confint(mixedModel.no.supp.wesa, method = "boot", nsim= 1500, seed = 5735779)
CIs
CI.nosupp
nosupp.results <- cbind(as_tibble(CI.nosupp), "term" = rownames(CI.nosupp)) %>% 
  full_join(tidy(mixedModel.no.supp.wesa), by = "term")  %>% mutate(supp=FALSE)

compareResults <- bind_rows(nosupp.results, mixedModel.results)

plot_estimate_effects <- compareResults %>% filter(!is.na(std.error) & term != "(Intercept)") %>% 
  ggplot(aes(term,estimate , colour = supp)) + 
    geom_pointrange(position = position_dodge(width=.5),
                    aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
    coord_flip() +  labs(colour='With KABA \nsupplimentary\n data?', y = "Estimate", x="") + ggthemes::theme_few() +
    geom_hline(yintercept = 0, linetype=2) #+ theme(legend.position = "none")

plot_estimate_effects


# boot_nosupp_aug <- data.for.analysis.nosupp %>% bootstrap(1000) %>%
#   do(augment(lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = .,  REML =T),.)) %>% 
#   mutate(siteRep = paste0(SITE,"_", replicate))
# 
# ggplot(boot_nosupp_aug, aes(Year, MU, colour = SITE)) +
#   geom_line(aes(y=.fitted, group=siteRep, colour =NULL),colour ="grey", alpha=.1) +
#   geom_line(data=mixed_aug, aes(y=.fitted)) +
#   geom_point(data = mixed_aug) + ggthemes::theme_few() +
#   geom_linerange(data=mixed_aug, aes(ymin=lci, ymax=uci)) +
#   coord_cartesian(ylim= c(113, 135) ) + ggtitle("Results without supplimentary data")


## If need be here are the pvalues from drop1
drop1(mixedModel,~d2b.st*yr_standardize, test = "Chisq")


## Model with only KABA and RBBP

KB_RB_Model <-lmer(MU ~ Lat_std*yr_standardize +(1|SITE), 
                   data = filter(data.for.analysis, SITE %in% c("RBBP", "KABA"))) 
kb_rb_aug <- augment(KB_RB_Model,  filter(data.for.analysis, SITE %in% c("RBBP", "KABA")))
# plot_bootstrap + geom_line(data = kb_rb_aug, aes(y = .fitted), linetype = 2)

unpooled.wesa <- 
ggplot(bootnls_aug, aes(Year, MU, group = SITE))  +
  geom_ribbon(data = summarizeBootstrap,  aes(y = median, ymin = conf.low, ymax = conf.high), colour = 'grey', fill = 'grey', alpha=0.25) +
  geom_line(alpha = 0.4, data=aug_MM, aes(y=.fitted, colour = SITE), size =1) +
  labs(y = "Peak passage estimate", colour = "Site", fill = "Site") + 
  geom_smooth(data = mutate(data.for.analysis, wt = 1/SD), aes(Year, MU, colour =SITE), 
              weight = 5, 
              method ="lm", linetype = 2, se=F) +
  coord_cartesian(ylim=c(113,135)) +
  ggthemes::theme_few()







## --------- Dunlin Analysis ---------------


# create a linear mixed model of lat by year with siteid and random effect
# fit model
library(lme4)

## Remove Outliers
# dunl <- filter(dunl, MU >90)
dunl <- mutate(dunl, row_n = row_number())

## 1. Outliers Y & X

ggplot(dunl, aes(row_n, MU)) + geom_pointrange(aes(y=MU,ymin=lci, ymax= uci )) 



ggplot(dunl, aes(row_n, MU)) + facet_wrap(~SPECIES, scales = "free") + geom_point() + coord_flip()




library(lattice)
Z <- cbind(dunl$MU, dunl$Year,  dunl$Lat)

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

ggplot(dunl, aes(SITE, MU)) + geom_boxplot()
ggplot(dunl, aes(YEAR, MU)) + geom_boxplot()

## 3. Normality Y

p_norm.dunl <- ggplot(dunl, aes(MU)) + geom_histogram(binwidth = 2)
p_norm.dunl
p_norm.dunl + facet_grid(SITE~.)



## 4. Zero toruble Y
# Not an issue
## 5. Colinearity X
library(corrgram)
corrgram(dplyr::select(dunl, Lat, Year, MU), order=TRUE, lcier.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
## 6. Relationships Y & X

ggplot(dunl, aes(Year, MU)) + geom_pointrange(aes(y=MU,ymin=lci, ymax= uci )) 
ggplot(dunl, aes(Lat, MU)) + geom_pointrange(aes(y=MU,ymin=lci, ymax= uci )) 

## 7. Interactions
ggplot(dunl, aes(Year, MU, colour = Lat)) + #geom_point()
  geom_pointrange(position = position_jitter(width = 0.25),aes(y=MU,ymin=lci, ymax= uci )) 


## 8 Independence Y
## Not indepenent, so used MM with random intercept of SiteID



data.for.analysis.dunl <- filter(dunl, MU > 90) %>% 
  mutate(SITE = factor(SITE, levels = c('KENN', 'RBBP', 'TOFN', "CRD","HART", 'KABA')),
         yr_standardize = arm::rescale(Year),
         YR.Group = ifelse(yr_standardize < 0, "Early", "Late"),
         Lat_std = arm::rescale(Lat),
         d2b.st = arm::rescale(d2Breeding))

mixedModel_ML.dunl <- lmer(MU ~ Lat_std*yr_standardize + (1|SITE), data = data.for.analysis.dunl,  REML =F)
mixedModelNoYrEff.dunl <- lmer(MU~Lat_std + (1|SITE), data  = data.for.analysis.dunl, REML=F)
mix_null.dunl <- lmer(MU~ 1 + (1|SITE),  data = data.for.analysis.dunl, REML=F)
mixedModelNoYrEff.dunl.d2b <- lmer(MU~d2b.st + (1|SITE), data  = data.for.analysis.dunl, REML=F)
mixedModel_ML.dunl.d2b <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis.dunl,  REML =F)

dunlAICtab <- bbmle::AICctab(mixedModel_ML.dunl, mixedModelNoYrEff.dunl, mix_null.dunl, mixedModel_ML.dunl.d2b, mixedModelNoYrEff.dunl.d2b,
                             mnames = c("Full Model - Latitude", "No Year Effect- Latitude", "No Fixed Effects", 
                                        "Full Model - Distance", "No Year Effect- Distance"),
                             logLik = F, weights = TRUE, base =T)
dunlAICtab      


aicRes.dunl <- stats::AIC(mixedModel_ML.dunl, mixedModelNoYrEff.dunl, mix_null.dunl,  mixedModel_ML.dunl.d2b,mixedModelNoYrEff.dunl.d2b)
aicRes.dunl$mnames <- c("Full Model - Latitude", "No Year Effect- Latitude", "No Fixed Effects", 
                   "Full Model - Distance", "No Year Effect- Distance")
aicRes.dunl$r2 <- c(sjstats::r2(mixedModel_ML.dunl)$r2,
               sjstats::r2(mixedModelNoYrEff.dunl )$r2,
               sjstats::r2(mix_null.dunl )$r2,
               sjstats::r2(mixedModel_ML.dunl.d2b)$r2,
               sjstats::r2(mixedModelNoYrEff.dunl.d2b)$r2
)

rsquares.dunl<- rbind(MuMIn::r.squaredGLMM(mixedModel_ML.dunl),
                 MuMIn::r.squaredGLMM(mixedModelNoYrEff.dunl ),
                 MuMIn::r.squaredGLMM(mix_null.dunl ),
                 MuMIn::r.squaredGLMM(mixedModel_ML.dunl.d2b),
                 MuMIn::r.squaredGLMM(mixedModelNoYrEff.dunl.d2b) ) %>% as.tibble


aicRes.dunl$N <- data.for.analysis.dunl %>% summarize(n=n()) %>% .[['n']] %>% rep(., 5)

aicRes.dunl <- aicRes.dunl %>% bind_cols(rsquares.dunl) %>% arrange(AIC) %>% 
  mutate(AICc = AIC + (  (2*(df)*(df+1))/(N - df -1) ),
         dAICc = AICc - min(AICc),
         logSup = exp(-0.5*dAICc),
         w  = logSup / sum(logSup)) %>% 
  dplyr::select(mnames, df, N, AICc, dAICc, w, R2m, R2c)

knitr::kable(aicRes.dunl, digits = 2)






mixedModel.dunl <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis.dunl,  REML =T)

arm::display(mixedModel.dunl)

## # Criticizing the model 

library(broom)

aug_MM.dunl <- augment(mixedModel.dunl, data.for.analysis.dunl)
aug_MM.dunl$lev <- hatvalues(mixedModel.dunl)
aug_MM.dunl$pearson <- residuals(mixedModel.dunl,type="pearson")

CIs.dunl <- confint(mixedModel.dunl, method = "boot",nsim= 1000, seed = 5735779)
dunl.ranef <- sjPlot::sjp.lmer(mixedModel.dunl)
sjPlot::sjp.lmer(mixedModel.dunl, type = "fe")
sjPlot::sjp.lmer(mixedModel.dunl, type = "re.qq")
lattice::qqmath(mixedModel.dunl)
p <- sjPlot::sjp.int(mixedModel.dunl, type = "eff", show.ci = T, int.term = "d2b.st*yr_standardize")

ggplot(aug_MM.dunl, aes(Year, lev, shape = SITE)) + geom_point()



mixedModel.results.dunl <- cbind(as_tibble(CIs.dunl), "term" = rownames(CIs.dunl)) %>% 
  full_join(tidy(mixedModel.dunl), by = "term") %>% mutate(supp=TRUE)


ggplot(aug_MM.dunl, aes(Year, .resid)) + geom_point() + facet_wrap(~SITE) + ggthemes::theme_few()+
  geom_hline(yintercept = 0,  colour = 'red')

ggplot(arrange(aug_MM.dunl, Year, Lat), aes(.fitted, .resid)) + geom_point() + facet_wrap(~SITE) + ggthemes::theme_few()+
  geom_hline(yintercept = 0,  colour = 'red')

## Plot the results

ggplot(aug_MM.dunl, aes(Year, .fitted, colour = SITE)) + geom_line() + #geom_point(aes(y=MU)) + 
  geom_pointrange(data = data.for.analysis.dunl, aes( y = MU, ymin = lci, ymax = uci )) #+ ylim(100, 150)

yr.key.dunl <- data.for.analysis.dunl %>% select( yr_standardize, Year) %>% distinct
boot.strap.d <- data.for.analysis.dunl %>% modelr::bootstrap(1000)
boot.models.d <-  map(boot.strap.d$strap, ~lmer(MU ~ d2b.st*yr_standardize + (1|SITE), REML=T, data = .)) 
boot.augment.d <- map_df(boot.models.d, broom::augment, .id = "id") %>% left_join(yr.key.dunl, by = "yr_standardize")
bootnls_aug.dunl <- boot.augment.d


# bootnls_aug.dunl <- data.for.analysis.dunl %>% bootstrap(1000) %>%
#   do(augment(lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = .,  REML =T),.)) %>% 
#   mutate(siteRep = paste0(SITE,"_", replicate))

bootnls_aug.dunl$SITE_F <- factor(bootnls_aug.dunl$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
aug_MM.dunl$SITE_F <- factor(aug_MM.dunl$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))


summarizeBootstrap.dunl <- bootnls_aug.dunl %>% group_by(Year, SITE_F, SITE) %>% 
  summarize(median = median(.fitted),
            conf.low = quantile(.fitted, 0.025),
            conf.high = quantile(.fitted, 0.975)) %>% ungroup


plot_bootstrap.dunlold <- ggplot(bootnls_aug.dunl, aes(Year, MU, group = SITE))  + scale_color_grey()+
  
  scale_shape_manual(values = c(19,17,15, 0,5,1)) +
  geom_ribbon(data = summarizeBootstrap.dunl,  aes(y = median, ymin = conf.low, ymax = conf.high, group = SITE), fill = 'grey', alpha=0.25) +
  # geom_line(data = bootnls_aug.dunl[which(bootnls_aug.dunl$replicate %in% sample(unique(bootnls_aug.dunl$replicate), 10, F)), ], 
  #           aes(y=.fitted, group=siteRep, colour =NULL, linetype = SITE),colour ="grey", alpha=.6) +
  # geom_point(data = aug_MM.dunl, aes(shape = SITE),  alpha = 0.4) + 
  # ggthemes::theme_few() +
  geom_pointrange(position = position_dodge(width = 0.4), data = aug_MM.dunl, aes(ymin=MU-SD, ymax=MU+SD, shape = SITE), alpha = 0.7 ) +
  # geom_linerange(data=aug_MM.dunl, aes(ymin=MU-SD, ymax=MU+SD), alpha = 0.7) +
  geom_line(data=aug_MM.dunl, aes(y=.fitted, linetype = SITE)) +
  
   # coord_cartesian(ylim= c(90, 135) ) + 
  labs(y = "Peak passage estimate", colour = "Site", fill = "Site")

plot_bootstrap.dunl <- ggplot(bootnls_aug.dunl, aes(Year, MU))  + 
  geom_line(aes(group=id, y=.fitted), alpha = 0.05, colour = 'grey')+
    # # geom_line(data=summarizeBootstrap.dunl, aes(y = conf.low), linetype=2)+
    # # geom_line(data=summarizeBootstrap.dunl, aes(y = conf.high), linetype=2)+
    # geom_ribbon(data = summarizeBootstrap.dunl,
    #           aes(y = median, ymin = conf.low, ymax = conf.high), fill = 'grey', alpha=0.25) +
  geom_linerange(alpha = 0.6,
                 # position = position_dodge(width = 0.4),
                 data = aug_MM.dunl,
                 aes(ymin=MU-SD, ymax=MU+SD)) +
  geom_point(data=aug_MM.dunl)+
  geom_line(data=aug_MM.dunl, aes(y=.fitted)) +
  labs(y = "Peak passage estimate", colour = "Site", fill = "Site") +
  facet_grid(.~SITE_F)




plot_bootstrap.dunl #+ geom_point(data = exD, colour = 'red', aes(shape = SITE))



## Compare with individual site models

estimates.dunl <- data.for.analysis.dunl %>% group_by(SITE) %>%
  do(mod = lm(MU~yr_standardize, data = .)) %>%
  mutate(int = coef(mod)[1], slope = coef(mod)[2], method = "Separate") %>%
  dplyr::select(-mod) %>% arrange(as.character(SITE))
estimates.dunl$re_int <- coef(mixedModel.dunl)$SITE[,1]
estimates.dunl$mm_slope <- coef(mixedModel.dunl)$SITE[,3]
estimates.dunl$mm_interaction <- coef(mixedModel.dunl)$SITE[,4]
estimates.dunl$mm_site_slope <- site_key$d2b.st*estimates.dunl$mm_interaction + estimates.dunl$mm_slope


sample_sizes <-data.for.analysis %>% group_by(SITE) %>%
  summarise(n = n())
estimates.dunl$n <- sample_sizes$n


ggplot(estimates.dunl, aes(x = "lm()", y = int, xend = "lmer()", yend = re_int)) +
  geom_segment(alpha = 0.5) +
  geom_point(aes(colour = SITE, size = n)) +
  xlab("")

ggplot(estimates.dunl, aes(x = "lm()", y = slope, xend = "lmer()", yend = mm_site_slope)) +
  geom_segment(alpha=0.5) +
  geom_point(aes(colour = SITE, size = n)) +
  xlab("")



# DUNL Compare Results with and without supp -----------------------------------


# ggplot(kaba.compare, aes(as.numeric(YEAR), MU, colour = supp)) + geom_point()

data.for.analysis.nosupp.dunl <- filter(mu_w_out_supp, SPECIES == "DUNL", N >=7 & MU > 90) %>% 
  mutate(SITE = factor(SITE, levels = c('KENN', 'RBBP', 'TOFN', "CRD","HART", 'KABA')),
         yr_standardize = arm::rescale(Year),
         YR.Group = ifelse(yr_standardize < 0, "Early", "Late"),
         d2b.st = arm::rescale(d2Breeding))

mixedModel.nosupp.dunl <- lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = data.for.analysis.nosupp.dunl,  REML =T)
mixed_aug.dunl <- augment(mixedModel.nosupp.dunl, data.for.analysis.nosupp.dunl)
tidy(mixedModel.dunl)
tidy(mixedModel.nosupp.dunl)
CI.nosupp.dunl <- confint(mixedModel.nosupp.dunl, method = "boot")
CIs.dunl
CI.nosupp.dunl
nosupp.results.dunl <- cbind(as_tibble(CI.nosupp.dunl), "term" = rownames(CI.nosupp.dunl)) %>% 
  full_join(tidy(mixedModel.nosupp.dunl), by = "term")  %>% mutate(supp=FALSE)

compareResults.dunl <- bind_rows(nosupp.results.dunl, mixedModel.results.dunl)

plot_estimate_effects.dunl <- compareResults.dunl %>% filter(!is.na(std.error) & term != "(Intercept)") %>% 
  ggplot(aes(term,estimate , colour = supp)) + 
  geom_pointrange(position = position_dodge(width=.5),
                  aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  coord_flip() +  labs(colour='Supplimentary\nData?', y = "Estimate", x="") + ggthemes::theme_few() +
  geom_hline(yintercept = 0, linetype=2)

plot_estimate_effects.dunl


# boot_nosupp_aug.dunl <- data.for.analysis.nosupp.dunl %>% bootstrap(1000) %>%
#   do(augment(lmer(MU ~ d2b.st*yr_standardize + (1|SITE), data = .,  REML =T),.)) %>% 
#   mutate(siteRep = paste0(SITE,"_", replicate))

# ggplot(boot_nosupp_aug.dunl, aes(Year, MU, colour = SITE)) +
#   geom_line(aes(y=.fitted, group=siteRep, colour =NULL),colour ="grey", alpha=.1) +
#   geom_line(data=mixed_aug.dunl, aes(y=.fitted)) +
#   geom_point(data = mixed_aug.dunl) + ggthemes::theme_few() +
#   geom_linerange(data=mixed_aug.dunl, aes(ymin=lci, ymax=uci)) +
#   coord_cartesian(ylim= c(90, 135) ) + ggtitle("Results without supplimentary data")


## If need be here are the pvalues from drop1
drop1(mixedModel.dunl,~d2b.st*yr_standardize, test = "Chisq")


## Model with only KABA and RBBP

KB_RB_Model.dunl <-lmer(MU ~ d2b.st*yr_standardize +(1|SITE), 
                   data = filter(data.for.analysis.dunl, SITE %in% c("RBBP", "KABA")) 
                   ) 
kb_rb_aug.dunl <- augment(KB_RB_Model.dunl,  filter(data.for.analysis.dunl, SITE %in% c("RBBP", "KABA")))
# plot_bootstrap.dunl + geom_line(data = kb_rb_aug.dunl, aes(y = .fitted), linetype = 2)

unpooled.dunl <- 
  ggplot(bootnls_aug.dunl, aes(Year, MU, group = SITE))  +
  geom_ribbon(data = summarizeBootstrap.dunl,  aes(y = median, ymin = conf.low, ymax = conf.high), colour = 'grey', fill = 'grey', alpha=0.25) +
  geom_line(alpha = 0.4, data=aug_MM.dunl, aes(y=.fitted, colour = SITE), size =1) +
  labs(y = "Peak passage estimate", colour = "Site", fill = "Site") + 
  geom_smooth(data = mutate(data.for.analysis.dunl, wt = 1/SD), aes(Year, MU, colour =SITE), weight = 5, 
                                                                                  method ="lm", linetype = 2, se=F) + #coord_cartesian(ylim=c(113,135)) +
  ggthemes::theme_few()

unpooled.dunl


## Predictions for comparison to DUNL tracking




new_dat$.fitted.dunl <- predict(mixedModel.dunl, newdata = new_dat)


new_dat[which(new_dat$Year%in% c(2001) & new_dat$SITE %in% c("CRD", "RBBP")),] %>% 
  group_by(Year) %>% summarize(diff = diff(.fitted.dunl))





# graytoCRD <- data.frame(Year = c(2001,2001), SITE = c("CRD", "Gray"), Lat = c(60.27, 46.98)) %>% 
#   mutate(Lat_std =  (Lat - mean(data.for.analysis$Lat)) / (sd(data.for.analysis$Lat) * 2) ,
#          yr_standardize = (Year - mean(data.for.analysis$Year)) / (sd(data.for.analysis$Year) *2) )
graytoCRD <- graytoCRD.wesa
predict_crd_2001 <- predict(mixedModel.dunl, newdata = graytoCRD[1,])
predict_grey <- fixef(mixedModel.dunl)[["(Intercept)"]] + 
  fixef(mixedModel.dunl)[["d2b.st"]] * graytoCRD$d2b.st[2] + 
  fixef(mixedModel.dunl)[["yr_standardize"]]  * graytoCRD$yr_standardize[2]+ 
  fixef(mixedModel.dunl)[["d2b.st:yr_standardize"]] * graytoCRD$d2b.st[2] * graytoCRD$yr_standardize[2] 

predict_crd_2001 - predict_grey




# Combine species estimates -----------------------------------------------

fulleffects <- bind_rows(mutate(compareResults.dunl, species = "DUNL"), mutate(compareResults, species = "WESA")) %>% 
  mutate(species_supp = paste0(species, supp))


## Convert estimates
fixef(mixedModel)[["yr_standardize"]]


plot_estimate_effects.full <- fulleffects %>% filter(!is.na(std.error) & term != "(Intercept)") %>% 
  ggplot(aes(term,estimate , shape = species_supp)) + 
  scale_shape_manual(values = c(1, 2, 19, 17),
                     labels = c("Dunlin without\nSupplemental Data\n", "Dunlin\n", "WESA without\nSupplemental Data\n", "WESA")) +
  geom_pointrange(position = position_dodge(width=.5),
                  aes(ymin = `2.5 %`, ymax = `97.5 %`)) + scale_x_discrete(labels = c("Distance to \nBreeding Area", "Dist to Breeding \nby Year", "Year")) +
  coord_flip() +  labs(shape='', y = "Standardized Estimate", x="") + ggthemes::theme_few(base_size = 14, base_family = "Helvetica") +
  geom_hline(yintercept = 0, linetype=2)  + 
  theme(legend.justification=c(1,1), legend.position=c(.43,.99))#legend.position = "none")
plot_estimate_effects.full

# write_excel_csv(x = fulleffects %>% filter(supp==TRUE), "../4th Submission/figures_and_appendix/model_effects.csv")
plot_estimate_effects.forMS <- fulleffects %>% filter(!is.na(std.error) & 
                                                        term != "(Intercept)" & 
                                                        supp ==TRUE) %>% 
  ggplot(aes(term,estimate, shape = species )) + 
  scale_shape_manual(values = c(17,19),labels = c("Dunlins", "Western\nSandpipers")) +
  geom_pointrange(size = .3,position = position_dodge(width=.25),
                  aes(ymin = `2.5 %`, ymax = `97.5 %`)) + 
  scale_x_discrete(labels = c("Distance to \nBreeding Area", "Dist to Breeding \nby Year", "Year")) +
  coord_flip() +  labs(shape='', y = "Standardized Estimate", x="") + 
  # ggthemes::theme_few(base_size = 12, base_family = "Helvetica") +
  geom_hline(yintercept = 0, linetype=2)  + theme_cowplot(font_size=8,font_family = "helvetica")+
  theme(legend.justification=c(1,.9), legend.position=c(.4,1), #legend.text = element_text(size = 14),
        legend.background = NULL)#legend.position = "none")


plot_estimate_effects.forMS

save_plot(plot = plot_estimate_effects.forMS , filename = 'Figure4.tiff', base_width = 3.5, base_height =2.33,units = 'in',
       dpi=1200)#jpg', width = 6, height = 4, dpi = 300)


ggsave(plot = plot_estimate_effects.forMS ,filename =  'Figure4.jpg', width = 6, height = 4, dpi = 600)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# legend1 <- g_legend(plot_bootstrap+theme(legend.position="bottom",legend.text = element_text(size = 8)))



# pdf("PredictivePlots.pdf", width = 6, height = 12)
# png("PredictivePlots.png", width = 6, height = 12,units = "in", res = 600)

require(gridExtra)
# tiff("Figure5.tif", width = 3.5, height = 4.66667,units='in', res = 600 )
# modelEst.plots <- gridExtra::grid.arrange(
#   plot_bootstrap + ggtitle("Western Sandpipers") + 
#                                             theme(legend.position = 'none',
#                                                   text = element_text(size=8)) + 
#                           coord_cartesian(ylim= c(113, 135) ) ,
#   legend1,
#   plot_bootstrap.dunl + 
#     ggtitle("Dunlin") + 
#     theme(legend.position = 'none', text = element_text(size=8)) +
#     coord_cartesian(ylim= c(90, 135) ) , 
#   
#   ncol = 1, nrow =3, heights = c(10,1,10))#,

                        # widths = c(0.45, 0.55))
# dev.off()
# ggsave(plot = modelEst.plots , 'Figure5.jpg', width = 6, height = 4, dpi = 600)
# ggsave(plot = modelEst.plots , 'Figure5.eps', width = 3.5, height =4.66667,
       # dpi=600)#jpg', width = 6, height = 4, dpi = 300)

plotModelESTs <- plot_grid(plot_bootstrap + labs(x="", title = "Western Sandpipers") +
            theme(axis.text.x = element_blank()), #, axis.title.y = element_blank()
          
          plot_bootstrap.dunl +
            labs(title="Dunlins")+
            theme(#axis.title.y = element_text(hjust = 2),
              axis.text.x = element_text(angle=45, hjust = 1),
              strip.background = element_blank(),
                  strip.text.x = element_blank()), labels = c("A", "B"),nrow = 2, align = "v")

plotModelESTs

save_plot("../4th Submission/figures_and_appendix/Figure5.jpeg", plotModelESTs, base_height = 7,  base_aspect_ratio = 1.0,dpi = 600)
save_plot("../4th Submission/figures_and_appendix/Figure5.tiff", plotModelESTs,base_width = 7,base_height = 7, dpi = 600)
# png("suppFigUnpooled.png", width = 6, height = 8, units='in', res=300)
gridExtra::grid.arrange(unpooled.wesa +ggtitle("Western Sandpipers") + theme(legend.position = 'none'),
                        unpooled.dunl+ggtitle("Dunlin") + theme(legend.position = 'none'),
                        ncol=1, nrow=2)
# dev.off()

    

plot_ranef <- 
  wesa.ranef$data %>% mutate(SITE = rownames(.), species = 'WESA') %>% 
  bind_rows(dunl.ranef$data  %>% mutate(SITE = rownames(.), species = 'DUNL') ) %>% 
  mutate(SITE = factor(SITE, levels = c("KENN", "RBBP", "TOFN", "CRD","HART", "KABA"))) %>% 
  ggplot(aes(SITE, estimate, shape = species)) + 
  geom_pointrange(size = .3,position = position_dodge(width=.5), aes(ymin = conf.low, ymax=conf.high)) +
  coord_flip() +  labs(shape='', y = "Random Effect Intercept", x="") + 
  # ggthemes::theme_few(base_family = "Helvetica") +
  geom_hline(yintercept = 0, linetype=2) + theme_cowplot(font_size=12,font_family = "helvetica")+
  theme(#legend.justification=c(.8,1), 
        legend.position=c(.15,.9)) + 
  scale_shape_manual(values = c(17,19),labels = c("Dunlins", "Western\nSandpiper"))
plot_ranef
#ggsave(plot = plot_ranef, 'ranefplot.png', width = 6, height = 4, dpi = 300)
ggsave(plot = plot_ranef +
         theme(text = element_text(size=8), legend.background = NULL,
               legend.text = element_text(size = 6)), 'Figure6.tiff', width = 3.5, height =2.33,
       dpi=600)#jpg', width = 6, height = 4, dpi = 300)  
ggsave(plot = plot_ranef +
         theme(text = element_text(size=8), 
               legend.text = element_text(size = 6)), 'Figure6.jpg', width = 3.5, height = 2.33, dpi = 600)  

# ggplot(aes(term,estimate , shape = species_supp)) + 
#   scale_shape_manual(values = c(1, 2, 19, 17),
#                      labels = c("Dunlin", "Dunlin with Supp Data", "WESA", "WESA with Supp Data")) +
#   geom_pointrange(position = position_dodge(width=.5),
#                   aes(ymin = `2.5 %`, ymax = `97.5 %`)) + scale_x_discrete(labels = c("Distance to \nBreeding Area", "Dist to Breeding \nby Year", "Year")) +
#   coord_flip() +  labs(shape='', y = "Standardized Estimate", x="") + ggthemes::theme_few(base_size = 14, base_family = "Helvetica") +
#   geom_hline(yintercept = 0, linetype=2)  + 
#   theme(legend.justification=c(1,1), legend.position=c(.6,.95))#legend.posit


# Final Tables ------------------------------------------------------------
siteNames <- data_frame(SITE = sort(unique(mu$SITE)), Site = c("Copper River Delta","Hartney Bay", "Kachemak Bay","Kennedy Creek","Roberts Bank", "Tofino Mudflats"  ))
species_key <-  data_frame(SPECIES = c("WESA", "DUNL"), Species = c("Western Sandpiper", "Dunlin"))

## Mu Tables 
require(lubridate)
summary.tab <- data.for.analysis %>%
  bind_rows(data.for.analysis.dunl) %>%  
  left_join(siteNames, by = "SITE") %>% left_join(species_key, by = "SPECIES") %>% 
  dplyr::select(Species, Site, YEAR, N, Min, Max, MU, SD) %>% 
  group_by(Species, Site) %>% 
  summarise("N Years" = n(), 
            "Survey Days\nMean" =mean(N, na.rm=T),
            "Survey Days\nSE"= sd(N, na.rm=T)/sqrt(n()),
            "SID\nMean" = paste0(day(as_date(mean(Min, na.rm=T)))," ", month(as_date(mean(Min, na.rm=T)), label = T)),
            "SID\nSE" = sd(Min, na.rm=T)/sqrt(n()),
            "STD\nMean" = paste0(day(as_date(mean(Max, na.rm=T)))," ", month(as_date(mean(Max, na.rm=T)), label = T)),
            "STD\nSE" = sd(Max, na.rm=T)/sqrt(n()),
            "MU\nMean" =paste0(day(as_date(mean(MU, na.rm=T)))," ", month(as_date(mean(MU, na.rm=T)), label = T)),
            "MU\nSE" = sd(MU, na.rm=T)/sqrt(n()),
            "SD\nMean" = mean(SD, na.rm=T),
            "SD\nSE" = sd(SD, na.rm=T)/sqrt(n())            ) 

#' --------------------------------------
knitr::kable(summary.tab, digits = 2, format = 'latex')


# saveRDS(summary.tab, "cleanVersions/sumtable.rds")
#'------------------

aicTable

#'-------------
dunlAICtab

#'-------------
knitr::kable(aicResTab, digits = 2)
knitr::kable(aicRes.dunl, digits = 2)

#'-------------


## Export full MU table for Supplementary file



# mu.export <-
#   readRDS("./cleanVersions/datafiles/MuEstimates_w_Hart_w_supp.rds") %>%
#   # filter(SITE !="CRD") %>% bind_rows(readRDS("./cleanVersions/datafiles/MuEstimates_CRD_noHART.rds")) %>%
#   rename(lci_old =lci, uci_old = uci) %>%
#   left_join(jackknife_estimates, by = c("SITE", "YEAR", "SPECIES", "MU")) %>%
#   # readRDS('./cleanVersions/datafiles/MuEstimates_newCRD.rds') %>% filter(as.numeric(as.character(YEAR))<2009 | SITE != "KABA") %>%
#   # bind_rows(readRDS('./cleanVersions/datafiles/MuEstimates_KABA_w_supp_CRD.rds')) %>%
#   # bind_rows(readRDS("./cleanVersions/datafiles/MuEstimates_Hartney.rds")) %>%
#   mutate(Year=as.numeric(as.character(YEAR))) %>%
#   left_join(siteNames, by = "SITE") %>% left_join(species_key, by = "SPECIES") %>%  mutate(SD = exp(SIGMA), # Convert estimate to SD
# 
#                                              # yr_standardize = arm::rescale(as.numeric(YEAR)), # Scale Year variable
#                                              row_n = row_number()) %>% arrange(Species,Site, Year) %>%
#   dplyr::select(Site, Species, Year, lci, MU, uci, SD, N, Min, Max, g.dev, iter, conv)
# 
# #
# write_excel_csv(mu.export, "../4th Submission/Supplimentary_Mu_full_wHART.csv")

# kbsupp <- readRDS('./cleanVersions/datafiles/MuEstimates_KABA_w_supp_CRD.rds') %>%
#   mutate(supp=TRUE) %>% rename(lci_old =lci, uci_old = uci) %>% 
#   left_join(jackknife_estimates_supp, by = c("SITE", "YEAR", "SPECIES", "MU")) %>% 
#   mutate(Year = as.numeric(YEAR))
# 
# mu.export <- 
#   readRDS('./cleanVersions/datafiles/MuEstimates_newCRD.rds') %>% mutate(Year=as.numeric(as.character(YEAR))) %>% 
#   left_join(siteLat, by = "SITE") %>% mutate(SD = exp(SIGMA), # Convert estimate to SD
#                                              # yr_standardize = arm::rescale(as.numeric(YEAR)), # Scale Year variable
#                                              row_n = row_number()) %>% rename(lci_old =lci, uci_old = uci) %>%
#   left_join(jackknife_estimates, by = c("SITE", "YEAR", "SPECIES", "MU")) %>% 
#   mutate(Year = as.numeric(YEAR)) %>% arrange(SPECIES, Year, Lat) %>% filter(Year < 2009 | SITE != "KABA") %>% bind_rows(kbsupp) %>% 
#   left_join(siteNames, by = "SITE") %>% left_join(species_key, by = "SPECIES") %>%  mutate(SD = exp(SIGMA), # Convert estimate to SD
# 
#                                              # yr_standardize = arm::rescale(as.numeric(YEAR)), # Scale Year variable
#                                              row_n = row_number()) %>% arrange(Species,Site, Year) %>%
#   select(Site, Species, Year, lci, MU, uci, SD, N, Min, Max, g.dev, iter, conv)
# write_excel_csv(mu.export, "../4th Submission/Supplimentary_Mu_full.csv")

# kaba.compare <- 
# 	mutate(mu, supp = FALSE) %>% filter(SITE == "KABA" & Year > 2000) %>% 
# 	bind_rows(kaba.supp)

# mu_w_supp <- filter(mu, SITE != "KABA" | Year < 2000) %>% mutate(supp = FALSE) %>%
#   dplyr::select(one_of(names(kaba.supp))) %>%
#   bind_rows(kaba.supp) %>%
#   left_join(siteLat, by = "SITE") %>%
#   mutate(SD = exp(SIGMA),
#          row_n = row_number()) %>% mutate(err = uci-lci)







## Convert estimates
fixef(mixedModel)[["yr_standardize"]] /( 2 * sd(data.for.analysis$Year))
# aug_MM[which(aug_MM$SITE =="RBBP"),] %>% arrange(Year) %>% mutate(yr_1 = lag(.fitted), delta = .fitted / yr_1) %>% 
#  .[["delta"]] %>% hist
delta_rbbp <- (aug_MM[which(aug_MM$SITE =="RBBP" & aug_MM$Year==2016),][['.fitted']] - aug_MM[which(aug_MM$SITE =="RBBP" & aug_MM$Year==1991),][['.fitted']] )
delta_rbbp_yr <- delta_rbbp / (2016-1991)

delta_kaba <- (aug_MM[which(aug_MM$SITE =="KABA" & aug_MM$Year==2016),][['.fitted']] - aug_MM[which(aug_MM$SITE =="KABA" & aug_MM$Year==1991),][['.fitted']] )
delta_kaba_yr <- delta_kaba / (2016-1991)

delta_KENN <- (aug_MM[which(aug_MM$SITE =="KENN" & aug_MM$Year==2014),][['.fitted']] - aug_MM[which(aug_MM$SITE =="KENN" & aug_MM$Year==1985),][['.fitted']] )
delta_KENN_yr <- delta_KENN / (2014-1985)

delta_rbbp
delta_rbbp_yr 
delta_rbbp_yr * 31
delta_kaba
delta_kaba_yr 
delta_kaba_yr * 31
delta_KENN
delta_KENN_yr
delta_KENN_yr * 31
(-delta_KENN_yr + delta_kaba_yr)*31
(-delta_rbbp_yr + delta_kaba_yr)*31


options(warn = 0)