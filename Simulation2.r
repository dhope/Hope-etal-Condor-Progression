### Single site sensitivity analysis
options(warn = -1)

require(tidyverse)
output <- readRDS("SingleSim.rds")
output.succes <- do.call("bind_rows", output) %>% 
	filter(conv != FALSE) %>% mutate(sd = exp(as.numeric(SIGMA)))


nowintering <- output.succes %>% filter(propWint == "0.0" & propA == "1.0")
hist(nowintering$MU)

baseruns <- nowintering %>% filter(LoS == 5 & ArrivalDate ==4)



#### Final plot # 1
simplt1 <- 
ggplot(aug_lm, aes(LoS,.fitted, shape = as.factor(ArrivalDate))) + #geom_line() + 
  stat_summary(aes(y=MU), position = position_dodge(width = 0.15),fun.data = "mean_cl_boot" , 
               fun.args = c(conf.int = 0.95, B = 2000), geom = "pointrange") +
  # geom_hline(yintercept = 0)+ 
  # geom_pointrange(position = position_jitter(width = 0.2),aes(y=MU, ymin=MU - sd, ymax = MU + sd )) +
    ggthemes::theme_few() + scale_shape_manual(values=c(2,1,19,15)) +
  labs(x = "Mean length of stay", y= "Deviation from \nbaseline peak passage date", colour = "Mean\ndate of\narrival")





# Wintering birds ---------------------------------------------------------

# Wwintering <- readRDS("/home/dhope/Documents/SFU/PhD/Publications/Drever.Progression/Drever.MS/Model/powanal/winter.rds")
# outputWinter <- do.call("bind_rows", Wwintering) %>% filter(conv != FALSE) %>% mutate(sd = exp(as.numeric(SIGMA)))
# nwintering <- output.succes %>% filter(propWint == "0.0")
outputWinter <- output.succes %>% filter(propA == "1.0") %>% mutate(MU = MU - mean(baseruns$MU))
baseWint <- outputWinter %>% filter(LoS == 5 & ArrivalDate ==4)

# Winter final plot 1
wintplt1 <- 
  ggplot(baseWint, aes(as.numeric(propWint), MU)) + 
  # geom_smooth(method="lm", formula = "y~poly(x,2)", se = F, colour = 'grey') +
  stat_summary(fun.data= 'mean_cl_boot', geom="pointrange") + 
  labs(x = "Proportion of Winter Residents in Population", y="Deviation from \nbaseline peak passage date") + ggthemes::theme_few()
wintplt1




# Two groups --------------------------------------------------------------

two.shoes <- output.succes %>% filter(propWint == "0.0" & ArrivalDate >1) %>% mutate(MU = MU - mean(baseruns$MU, na.rm=T))

base2gr <-two.shoes %>% filter(LoS == 5 & ArrivalDate ==4)
ggplot(two.shoes, aes(as.numeric(propA), MU, colour = ArrivalDate, alpha = LoS)) + #geom_point()
  stat_summary(position=position_dodge(width = 0.05),fun.y= 'mean', geom="point", size = 3) + xlim(0,1)

example2 <- read.csv("/home/dhope/Documents/SFU/PhD/Publications/Drever.Progression/Drever.MS/Model/powanal/singleSimulation_[573]_[5]_[4]_[10000]_[0.0][0.75].csv", sep ="\t") %>% 
  filter(site == 0)

ggplot(example2, aes(time, fill = group)) + geom_histogram() + xlim(c(100,130))

lmBase2gr <- lm(MU~as.numeric(propA), data = base2gr)
broom::tidy(lmBase2gr)

twopops.plt <- 
ggplot(base2gr, aes(as.numeric(propA), MU)) +  
  # geom_smooth(method="lm", formula = "y~poly(x,2)", se = F, colour = "grey") +
  stat_summary(fun.data= 'mean_cl_boot', geom="pointrange") + 
  ggthemes::theme_few() + scale_color_grey(start = 0.8, end = 0.1)+
  labs(x = "Proportion of Baseline Strategy in Population", y="Deviation from baseline passage date")






# Smallpop ----------------------------------------------------------------

smallpop <- readRDS("/home/dhope/Documents/SFU/PhD/Publications/Drever.Progression/Drever.MS/Model/powanal/smallpop/SingleSimFuckThis_smallpop.rds") %>% 
  do.call("rbind", .) %>% filter(conv == TRUE) %>% bind_rows(baseruns) %>% 
mutate(Pop = as.numeric(as.character(Pop)), MU = MU - mean(baseruns$MU)) 
  
pop.plt <- 
ggplot(smallpop, aes(Pop, MU))  + 
  # geom_hline(yintercept = 0, colour = 'grey')+
  stat_summary( fun.data= 'mean_cl_boot') + 
  scale_x_log10(breaks = c(1,10,50,100,500,1000, 10000))+ 
   ggthemes::theme_few() +
  labs(y = "Deviation from baseline passage date", x = "Total number of birds stopping at site")







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
                                  nrow=2,ncol=2, rel_heights = c(.45,.45,.1), labels = 'AUTO')#,
                                  # left = "Deviation from baseline\npassage date (days)")

# final.pLab <- gridExtra::grid.arrange(final.p, legend1, ncol =2, widths = c(0.9, 0.1),
#                                       left= "Deviation from baseline passage date (days)")
# save_plot("cleanVersions/Figure3.png", final.pLab, base_aspect_ratio = 1.5, base_width = 8 )
# save_plot("cleanVersions/Figure3.eps", final.pLab, base_aspect_ratio = 1.5, base_width = 7 )
# ggsave("cleanVersions/Figure3.eps",final.p,width = 7, height =2.916667, dpi = 600)
