#poweranalysis.r
# David Hope
# Dec 8, 2016

require(tidyverse)



simData <- function(mu, sd, seed, CI = FALSE) {
    set.seed(seed)

    tst <- as.vector(rnorm(1000000, mu, sd), "integer")
    qnt <- quantile(tst,probs =c(0.005,  0.995))
    test <- tst[tst >=qnt[1] & tst<= qnt[2]]
    allRuns <- data.frame(time = test, Rnum = seed) 

    SimWESA <- allRuns %>% group_by(time, Rnum) %>% 
      summarize(WESA = max(n() +  rnorm(1, 0,2000),0)) %>% 
      ungroup %>% mutate(Year = 2016,
                         SiteID = "Simulation",
                         Day.of.Year = time) 


    source("muEstimation.R")

    simResults <- function(everyNdays, startingDay,  data = SimWESA, calcCI = FALSE){
      ndaySurveys <- data[seq(startingDay, nrow(data), everyNdays), ]
      if(nrow(ndaySurveys)== 1) {return(NULL)}
      output <-calc.peak.prog(site = "Simulation", year = 2016,
                                                data = ndaySurveys, species = 'WESA', estimateCI = calcCI, errorMethod = "jackknife" )
      if(is_null(output)) {return(NULL)}
      output$results$daySpacing <- everyNdays
      output$results$start <- startingDay
      output$results$seed <- seed


      return(output$results)
    }

    runs <- expand.grid(seq(1,7), seq(1,5))

    outputdf <- map2_df(runs[,1], runs[,2], simResults, calcCI = CI )
    baserun <- simResults(1, 1, data = SimWESA, calcCI = FALSE)

    return(list(outputdf, baserun))

}


# f = file()
# sink(file=f)
# simulationResults <- map(seq(1,1000), function(x) simData(125, 4.2, x))
# sink() ## undo silencing
# close(f)
# 
# saveRDS(simulationResults, "simResultsFull2.rds")
simulationResults <- read_rds("cleanVersions/simResultsFull2.rds") %>% transpose()
simfullCounts <- do.call("rbind", simulationResults[[2]])

simboot <- do.call("rbind", simulationResults[[1]]) %>% mutate(err = uci-lci, dev = abs(MU -mean(simfullCounts$MU)))

sum_boot <-
filter(simboot, conv == TRUE & MU > 50) %>% #& !is.na(lci) & !is.na(uci)) %>%
  mutate(dev = MU- mean(simfullCounts$MU)) %>%
  group_by(daySpacing, start) %>%
  summarize(median= median(dev),
            mn=mean(dev),
            conf.low = quantile(dev, 0.025),
            conf.high = quantile(dev, 0.975))  %>% ungroup %>%
  mutate(start = as.factor(start))


### Final figure for paper


N_boot <-
  filter(simboot, conv == TRUE & MU > 50) %>% #& !is.na(lci) & !is.na(uci)) %>%
  mutate(dev = MU- mean(simfullCounts$MU)) %>%
  group_by(N) %>%
  sample_n(1000, replace=T) %>% 
  summarize(n= n(),median= median(dev),
            mn = mean(dev),
            conf.low = quantile(dev, 0.025),
            conf.high = quantile(dev, 0.975))  %>% ungroup

require(cowplot)
Spacing_plot <-
  ggplot(sum_boot,
         aes(daySpacing, mn, shape = as.factor(start))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.1),aes(ymin = conf.low,
                      ymax= conf.high))   + scale_shape_manual( values = c(19, 17, 15, 2, 1)) +
  # ggthemes::theme_few() + #scale_colour_grey() +  #scale_y_continuous(breaks = c(seq(-7,7,2))) +
  theme(legend.position = 'bottom')+#, legend.text = element_text(size=6), text = element_text(size=8)) +
  labs(shape = "Survey Initiation Day", y = "Deviation from true peak date (days)", x = "Days between Surveys")


N_plot <- 
ggplot(N_boot, aes(N, mn)) + 
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = conf.low,
                      ymax= conf.high)) +
  # ggthemes::theme_few() +
  labs(x = "Number of Days Surveyed",
       y= "Deviation from true peak date (days)") + 
  # scale_y_continuous(breaks = c(seq(-6,6))) +
  geom_vline(xintercept = 7, linetype = 2, alpha = 0.4)



Figure2 <- plot_grid(Spacing_plot, N_plot, labels = c("A", "B"),nrow = 2, align = "v")
