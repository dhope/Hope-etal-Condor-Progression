### Single site sensitivity analysis
options(warn = -1)

require(tidyverse)
output <- readRDS(".data/SingleSim.rds")
output.succes <- do.call("bind_rows", output) %>% 
	filter(conv != FALSE) %>% mutate(sd = exp(as.numeric(SIGMA)))


nowintering <- output.succes %>% filter(propWint == "0.0" & propA == "1.0") %>% mutate(MU = MU - mean(baseruns$MU))

baseruns <- nowintering %>% filter(LoS == 5 & ArrivalDate ==4)
nowintering <- nowintering%>% mutate(MU = MU - mean(baseruns$MU))
lm.dat <- mutate(nowintering, LoS = as.numeric(LoS), Pop = as.numeric(Pop), ArrivalDate = as.numeric(ArrivalDate))
lm.res <- lm(MU~LoS*ArrivalDate, lm.dat)

aug_lm <- broom::augment(lm.res, lm.dat)


#### Final plot # 1
simplt1 <- 
ggplot(aug_lm, aes(LoS,.fitted, shape = as.factor(ArrivalDate))) + #geom_line() + 
  stat_summary(aes(y=MU), fun.data = "mean_cl_boot" , #position = position_dodge(width = 0.15),
               fun.args = c(conf.int = 0.95, B = 2000), geom = "pointrange") +
  # geom_hline(yintercept = 0)+ 
  # geom_pointrange(position = position_jitter(width = 0.2),aes(y=MU, ymin=MU - sd, ymax = MU + sd )) +
   scale_shape_manual(values=c(2,1,19,15)) + geom_hline(yintercept = 0, linetype=2)+
  scale_y_continuous( breaks = seq(-6, 6, by = 2), minor_breaks = seq(-5, 5, by = 2)) +
  cowplot::background_grid(size.major = 1, major = "y", minor = "y", size.minor = .8)+
  labs(x = "\nMean length of stay", y= "Deviation from \nbaseline peak passage date", shape = "Mean\ndate of\narrival")




# Wintering birds ---------------------------------------------------------

outputWinter <- output.succes %>% filter(propA == "1.0") %>% mutate(MU = MU - mean(baseruns$MU))
baseWint <- outputWinter %>% filter(LoS == 5 & ArrivalDate ==4)

# Winter final plot 1
wintplt1 <- 
  ggplot(baseWint, aes(as.numeric(propWint), MU)) + 
  # geom_smooth(method="lm", formula = "y~poly(x,2)", se = F, colour = 'grey') +
  # geom_point()+
  stat_summary(fun.data= 'mean_cl_boot', geom="pointrange") +
  labs(x = "Proportion of Winter\nResidents in Population", y="Deviation from \nbaseline peak passage date") +#+ ggthemes::theme_few()
  geom_hline(yintercept = 0, linetype=2)+
  scale_y_continuous( breaks = seq(-6, 6, by = 1), minor_breaks = seq(-5, 5, by = 2)) +
  cowplot::background_grid(size.major = 1, major = "y", minor = "y", size.minor = .8)


wintplt2 <- 
ggplot(outputWinter, aes(as.numeric(propWint), MU, shape = ArrivalDate)) + scale_x_continuous() +
  # geom_smooth(colour = 'grey', aes(linetype = ArrivalDate), method="lm", formula = "y~poly(x,2)", se = F) +
   stat_summary(position=position_dodge(width = 0.02), fun.data= 'mean_cl_boot') +
  scale_shape_manual(values=c(2,1,19,15)) +
  #scale_color_grey(start = 0.9, end = 0.2)+
  geom_hline(yintercept = 0, linetype=2)+
  scale_y_continuous( breaks = seq(-60, 20, by = 20), minor_breaks = seq(-60, 20, by = 10)) +
  cowplot::background_grid(size.major = 1, major = "y", minor = "y", size.minor = .8)+
  labs(x = "Proportion of Winter Residents in Population", 
       y="Deviation from \nbaseline peak passage date",
       colour = "Mean\nArrival\nDate")



# Two groups --------------------------------------------------------------

two.shoes <- output.succes %>% filter(propWint == "0.0" & ArrivalDate >1) %>% mutate(MU = MU - mean(baseruns$MU, na.rm=T))

base2gr <-two.shoes %>% filter(LoS == 5 & ArrivalDate ==4)


twopops.plt <- 
ggplot(base2gr, aes(as.numeric(propA), MU)) +  
  # geom_smooth(method="lm", formula = "y~poly(x,2)", se = F, colour = "grey") +
  stat_summary(fun.data= 'mean_cl_boot', geom="pointrange") + 
  labs(x = "Proportion of Baseline\nStrategy in Population", y="Deviation from baseline passage date")+
  geom_hline(yintercept = 0, linetype=2)+
  scale_y_continuous( breaks = seq(-6, 6, by = 0.5), minor_breaks = seq(-5, 5, by = 2)) +
  cowplot::background_grid(size.major = 1, major = "y", minor = "y", size.minor = .8)






# Smallpop ----------------------------------------------------------------

smallpop <- readRDS(".data/SingleSim_smallpop.rds") %>% 
  do.call("rbind", .) %>% filter(conv == TRUE) %>% bind_rows(baseruns) %>% 
mutate(Pop = as.numeric(as.character(Pop)), MU = MU - mean(baseruns$MU)) 
  
pop.plt <- 
ggplot(smallpop, aes(Pop, MU))  + 
  # geom_hline(yintercept = 0, colour = 'grey')+
  stat_summary( fun.data= 'mean_cl_boot') + 
  scale_x_log10(breaks = c(1,10,50,100,500,1000, 10000))+ 
   # ggthemes::theme_few() +
  labs(y = "Deviation from baseline passage date", x = "Total number of birds stopping at site") +
  geom_hline(yintercept = 0, linetype=2)+ theme(axis.text.x = element_text(size = 10,angle=30, hjust = 0.9,vjust =0.9))+
  scale_y_continuous( breaks = seq(-6, 6, by = 1), minor_breaks = seq(-5, 5, by = 2)) +
  cowplot::background_grid(size.major = 1, major = "y", minor = "y", size.minor = .8)






require(cowplot)
legend1 <- cowplot::get_legend(wintplt2 + labs(shape = "Arrival\nDate"))
final.p <-plot_grid(simplt1  + theme(legend.position="none", text=element_text(size=10))+ ylab("") + 
                      background_grid(major = 'y', minor = "none") + ylim(-6.5,5),
                    twopops.plt + ylab("")+ theme(legend.position="none", text=element_text(size=10))+
                      background_grid(major = 'y', minor = "none") + 
                                    ylim(-6.5,5),
                    wintplt1 + ylab("")+ theme(legend.position="none", text=element_text(size=10))+
                      background_grid(major = 'y', minor = "none") + 
                                    ylim(-6.5,5),
                    pop.plt+theme(legend.position="none", text=element_text(size=10))+ ylab("")+ 
                      background_grid(major = 'y', minor = "none") + 
                                     ylim(-6.5,5),
                                  nrow=2,ncol=2, rel_heights = c(.45,.45,.1), labels = c("(A)", "(B)", "(C)", "(D)"),
                    hjust=-0.1)#,
                                  # left = "Deviation from baseline\npassage date (days)")

final.pLab <- gridExtra::grid.arrange(final.p, legend1, ncol =2, widths = c(0.9, 0.1), padding=unit(0.8, "line"),
                                      left= "Deviation from baseline passage date (days)")

# save_plot("../6th Submission/Figure5.tiff", final.pLab, base_width = 7, base_height = 7)


## Example of how to read and calculate peak passage date from output of model
# ---------------------------------------------------------------------------------
# Test def of peak passage date -------------------------------------------

# require(tidyverse)

# exampleRun <- read_tsv(".data/singleSimulation_[573]_[5]_[4]_[10000]_[0.0][1.0].csv") %>% 
#   mutate()
# exPPest <- exampleRun %>% 
#   filter(site == 0) %>% 
#   group_by(Bird) %>% 
#   summarize(meanD = floor(mean(time))) %>% ungroup %>% 
#   group_by(meanD) %>% 
#   summarize(nPass = n(),
#             peakPassage = mean(meanD),
#             medPassage = median(meanD))

# source("muEstimation.R")

# dataforCalc <- exampleRun %>% group_by(time) %>% 
#   filter(site == 0) %>% 
#   summarize(WESA = n()) %>% 
#   ungroup %>% mutate(Year = 2016,
#                      SiteID = "Simulation",
#                      Day.of.Year = time) %>% filter(Day.of.Year >= quantile(exampleRun$time, 0.025)[1] & Day.of.Year <= quantile(exampleRun$time, 0.975)[1])
# results_1 <- calc.peak.prog(site = "Simulation", year = 2016, data = dataforCalc, species = "WESA", estimateCI = T, errorMethod = 'jackknife')
# results_1$results
# exPPest
# weighted.mean(dataforCalc$Day.of.Year, dataforCalc$WESA)
# options(warn = 0)
