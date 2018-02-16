# Standardize new version --------------------------------------------------
# Post-hoc analysis with temperature

require(tidyverse)
require(lme4)
# Load data. Created from Hope.etal.ProgAnalysis.R and climate.r
temp_dat <- read_rds("./.data/tempdat.rds")
wesa_dat <-  read_rds("./.data/mudat.rds")
dunlin_dat <-  read_rds("./.data/dunlinMu.rds")

# Manually entered site locations and distance to each site
# Values calculated from Google Earth
siteLat <- data.frame(SITE=c('HART', 'CRD', 'KABA', 'RBBP', 'TOFN', 'KENN'),#sites, 
                      Lat=c(60.50, 60.27,59.62,49.05,49.12,47.10), 
                      Lon=c(-145.86, -145.17,-151.46,-123.14,-125.88,-123.08),
                      Bethel = c(868, 910,580, 2740, 2590,  2900),
                      Emmonak = c(1018,1060, 780, 2930, 2790, 3100),
                      Nome = c(1095, 1120, 880,2994, 2860, 3170 )) %>% 
  rowwise %>% 
  mutate(d2Breeding = mean(c(Bethel, Emmonak, Nome)))


# Temperature Plot --------------------------------------------------------
require(cowplot)
temp_dat$SITE_F <- factor(temp_dat$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
temp_data <- left_join(temp_dat, wesa_dat %>% select(SITE, Year ) %>% mutate(inAnalysis=T))
temp_description_plot <- 
ggplot(temp_data %>%  filter(inAnalysis==T), aes(Year,TMIN)) + 
  facet_grid(~SITE_F) + 
  geom_point(data=temp_data %>%  filter(is.na(inAnalysis)), alpha=0.5, colour = 'grey') +
  geom_smooth(data=temp_data, method='lm', se=F, alpha=0.5, colour = 'grey') +
  geom_point() + geom_smooth(colour = 'black',method='lm', se=F) +
  ylab("Mean minimum\nair temperature (C)")


## ---------  Western Sandpiper Analysis ---------------
## The script here is used to describe the WESA analysis to run the DUNL
## analysis replace 'wesa_dat' for 'dunl_dat' below.

data.for.analysis <- left_join(wesa_dat, temp_dat) %>% 
  mutate(T_st = arm::rescale(TMIN)) %>% group_by(SITE) %>% 
  mutate(meanT=mean(TMIN)) %>% group_by(SITE, Year) %>% 
  mutate(devT = TMIN-meanT) %>% ungroup %>% 
  mutate(NS = ifelse(d2Breeding < 2000, "N", "S"),
         scaleT = arm::rescale(TMIN),
         devT.st = arm::rescale(devT))

## Select Random Effect Structure
RE_models  <- vector(mode = 'list')
RE_models[['full']] <- lmer(MU~devT.st*yr_standardize*d2b.st + (1+yr_standardize|SITE) + (1+devT.st|SITE)+ (1|SITE), data = data.for.analysis, REML=F)
RE_models[['ran_slope_yr']] <- lmer(MU~devT.st*yr_standardize*d2b.st + (1+yr_standardize|SITE), data = data.for.analysis, REML=F)
RE_models[['ran_slope_T']] <- lmer(MU~devT.st*yr_standardize*d2b.st +  (1+devT.st|SITE), data = data.for.analysis, REML=F)
RE_models[['ran_intercept']] <- lmer(MU~devT.st*yr_standardize*d2b.st +  (1|SITE), data = data.for.analysis, REML=F)

RE_models[['null']] <- lm(MU~devT.st*yr_standardize*d2b.st, data = data.for.analysis)

bbmle::AICctab(RE_models, weights =T)


## Compare fixed effect models
FE_models <- vector(mode='list')
FE_models[['full model']] <- RE_models[["ran_intercept"]]


FE_models[['null model']]  <- lmer(MU~1 +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['year by d2b']]  <- lmer(MU~yr_standardize*d2b.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['year by d2b plus T']]  <- lmer(MU~yr_standardize*d2b.st+devT.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['d2b plus T']]  <- lmer(MU~d2b.st+devT.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['d2b']]  <- lmer(MU~d2b.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['d2b plus year']]  <- lmer(MU~yr_standardize+d2b.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['yr plus d2b plus T']]  <- lmer(MU~yr_standardize+d2b.st+devT.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['year']]  <- lmer(MU~yr_standardize +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['T']]  <- lmer(MU~devT.st +  (1|SITE), data = data.for.analysis, REML=F)
FE_models[['year by d2b plus T by NorthSouth']]  <-  lmer(MU~yr_standardize*d2b.st+devT.st*NS+(1|SITE), data = data.for.analysis, REML=F)

# Final AIC comparison of FE models
bbmle::AICctab(FE_models, weights=T)


# Uncomment second model for DUNL analysis
FinalModel <-   lmer(MU~yr_standardize*d2b.st+devT.st*NS  +  (1|SITE), data = data.for.analysis, REML=T) # Final WESA model
# FinalModel <-   lmer(MU~yr_standardize*d2b.st +  (1|SITE), data = dunl, REML=T) # Final Dunlin model.

# criticize the model -----------------------------------------------------


library(broom)

# Add predictions and residuals to data.frame
aug_MM <- augment(FinalModel, data.for.analysis)
aug_MM$lev <- hatvalues(FinalModel)
aug_MM$pearson <- residuals(FinalModel,type="pearson")
ggplot(aug_MM, aes(Year, lev, shape = SITE)) + geom_point()
ggplot(aug_MM, aes(Year, pearson, shape = SITE)) + geom_point()
ggplot(aug_MM, aes(lev, pearson, shape = SITE)) + geom_point()

## Calculate confidence Intervals
require(boot)
CIs <- confint(FinalModel, method = "boot",nsim= 1000, seed = 5735779)
wesa.ranef <- sjPlot::sjp.lmer(FinalModel)
sjTab <- sjPlot::sjt.lmer(FinalModel, p.kr=F)
sjPlot::sjp.lmer(FinalModel, type = "fe")
sjPlot::sjp.lmer(FinalModel, type = "re.qq")
lattice::qqmath(FinalModel)

re.mod <- ranef(FinalModel)

FinalModel.results <- cbind(as_tibble(CIs), "term" = rownames(CIs)) %>% 
  full_join(tidy(FinalModel), by = "term") %>% mutate(supp=TRUE)

ggplot(aug_MM, aes(Year, .resid)) + geom_point() + facet_wrap(~SITE, scales = "free") + ggthemes::theme_few()+
  geom_hline(yintercept = 0,  colour = 'red')

ggplot(arrange(aug_MM, Year, Lat), aes(.fitted, .resid)) + geom_point() + facet_wrap(~SITE, scales = 'free') + ggthemes::theme_few()+
  geom_hline(yintercept = 0,  colour = 'red')


## Calculate bootstrap intervals

#1. Set up bootstrap on original data (1000 runs in my case)
boot.strap <- data.for.analysis %>% modelr::bootstrap(1000)
#2. Implement model on bootstraps of data
boot.models <-  map(boot.strap$strap, ~lmer(MU~yr_standardize*d2b.st+devT.st*NS + (1|SITE), data = ., REML=T))
#3. For each bootstrapped model, produce dataframe with variables and predictions
boot.augment_yr <- map_df(boot.models, returnprediction, dat = yrdat, .id = "id")#map_df(boot.models, broom::augment, .id = "id") 
boot.augment_T <- map_df(boot.models, returnprediction, dat = tmpdat, .id = "id")#map_df(boot.models, broom::augment, .id = "id") 
boot.augment <- map_df(boot.models, broom::augment, .id = "id") 




boot.augment_yr$SITE_F <- factor(boot.augment_yr$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
aug_MM$SITE_F <- factor(aug_MM$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))

confint(FinalModel, method = "boot")
bootnls_aug <-  boot.augment %>% left_join(yr.key, by = c("yr_standardize", "devT.st"))
bootnls_aug$SITE_F <- factor(bootnls_aug$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
alpha_a = 0.05
summarizeBootstrap <- bootnls_aug %>% group_by(Year, SITE_F, SITE) %>% 
  summarize(median = median(.fitted),
            conf.low = quantile(.fitted, alpha_a/2),
            conf.high = quantile(.fitted, 1-alpha_a/2)) %>% ungroup

aug_final <-  returnprediction(FinalModel, yrdat)
aug_final$SITE_F <- factor(aug_final$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))
data.for.analysis$SITE_F <- factor(data.for.analysis$SITE, levels = c("KENN", "RBBP", "TOFN", "CRD", "HART", "KABA"))


## Figure 2 WESA Bootstrap data 

plot_bootstrap <- ggplot(bootnls_aug, aes(Year, MU))  +
  geom_line(aes(group=id, y=.fitted), alpha = 0.05, colour = 'grey')+
  # geom_ribbon(data = summarizeBootstrap,  
  #             aes(y = median, ymin = conf.low, ymax = conf.high),  fill = 'grey', alpha=0.25) +
  geom_linerange(alpha = 0.2,
                 position = position_dodge(width = 0.4),
                 data = data.for.analysis,
                 aes(ymin=MU-SD, ymax=MU+SD)
                 ) +
  geom_point(data=data.for.analysis)+
  geom_line(data=aug_MM, aes(y=.fitted)) +
  labs(y = "Peak Passage Date\n(Day of Year)", group = "Site", fill = "Site") + facet_grid(.~SITE_F) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


plot_bootstrapT <- ggplot(bootnls_aug, aes(devT, MU))  +
  geom_line(aes(group=id, y=.fitted), alpha = 0.05, colour = 'grey')+
  # geom_ribbon(data = summarizeBootstrap,  
  #             aes(y = median, ymin = conf.low, ymax = conf.high),  fill = 'grey', alpha=0.25) +
  geom_linerange(alpha = 0.2,
                 # position = position_dodge(width = 0.4),
                 data = aug_MM,
                 aes(ymin=MU-SD, ymax=MU+SD)  ) +
  geom_point(data=aug_MM)+
  geom_line(data=aug_MM, aes(y=.fitted)) +
  labs(y = "Peak Passage Date\n(Day of Year)", 
       x = "Deviation from site mean minimum temperature", group = "Site", fill = "Site") + facet_grid(.~SITE_F)







# AIC Table ---------------------------------------------------------------

aicRes <- cbind(sapply(FE_models, bbmle::AIC ), 
                sapply(FE_models, function(x) length(names(coef(x)$SITE)) + 2),
                sapply(FE_models, nobs),
                sapply(FE_models, MuMIn::r.squaredGLMM) %>% t)%>% 
  # sapply(mods, stats::AIC) %>% 
  as.data.frame() %>% 
  rownames_to_column("mnames") %>% 
  rename(AIC=V1, df = V2, N=V3)

aicResTab <- aicRes %>%  arrange(AIC) %>%
  mutate(AICc = AIC + (  (2*(df)*(df+1))/(N - df -1) ),
         dAICc = AICc - min(AICc),
         logSup = exp(-0.5*dAICc),
         w  = logSup / sum(logSup)) 


