################################################
# Estimation of truncated normal distribution
#         with jackknife estimate of error
# David Hope
# December 8, 2016
################################################




################# Truncated normal distribution function ################
# For each site, we go through each year and estimate the mean peak progression date from a 
# truncated normal distribution
calc.peak.prog <- function(site, year, data, species = 'WESA',  estimateCI = TRUE, errorMethod, supp =FALSE){
  require(tidyverse)
  require(gamlss)
  require(gamlss.tr)
  require(lazyeval)

  # Select appropriate year and site, remove extra columns
  filter_criteria <- interp(~!is.na(which_column ), which_column = as.name(species))
  site.yr <- 
    data %>%
    dplyr::filter(SiteID == site & Year == year) %>%
    dplyr::filter_(filter_criteria)
  
  # Stop if not enough days or birds counted to run analysis
  if (nrow(site.yr) < 2) {return(NULL)}#'Insufficient Data')}
  else if (sum(site.yr[[species]], na.rm=T) < 5) {
    return(NULL)#"Insufficient Number of Birds")
    }  else {

      ##### Variables for model run
      min.day <- min(site.yr$Day.of.Year) # First day of surveys
      max.day <- max(site.yr$Day.of.Year) # Last day of surveys
      # mu.start <- mean(site.yr$Day.of.Year) # starting point for iteration
      days <- site.yr$Day.of.Year # Survey Dates
      counts <- site.yr[[species]] # Individual Counts
      data.used <- as.data.frame(cbind(days, counts))
      start.day <- days[[which.max(counts)]]
      startDays <- start.day#c(start.day, seq(start.day-3, start.day + 3, 1))
      winteringsites <- c('KENN', 'RBBP')
      ###########
      
      # Set the limits for the truncated distribution
      par.i <-c(min.day, max.day)

      
     # Generate a truncated normal distribution
      gen.trun(par = par.i, family = NO, type = "both", name = "FullDataNO")
      
      
      # Set up the interation steps to fit the truncated distribution  
      con.gl <- gamlss.control(c.crit = 0.01, n.cyc = 2000, mu.step =0.01, trace =FALSE)
      icon <- glim.control(cyc = 200)

      # Fit the distribution using the counts as weights and Day of Year as the variable
        gamlss.output <- tryCatch({ 
          gamlss(days~1, family=NOFullDataNO, weights =counts, data = data.used, trace = F,
            control = con.gl, i.control=icon, mu.start = startDays)},#mean(days))}, 
                                  error=function(err) {   
                                    print(' Error here in original fit.')
                                    a <- 'Error'
                                    return(a)} )
        ## # If the first run fails and cannot complete, model will not analyze that year
      
      if(gamlss.output == "Error") {if(site %in% winteringsites){
        gen.trun(par = par.i[1], family = NO, type = "left", name = "FullDataNOL")
        gamlss.output <- tryCatch({ 
        gamlss(days~1, family=NOFullDataNOL, weights =counts, data = data.used, trace = F,
               control = con.gl, i.control=icon, mu.start = startDays)},#mean(days))}, 
        error=function(err) {   
          print(' Error here in original fit.')
          a <- 'Error'
          return(a)} )} else{return(NULL)}}#gamlss.output)}
      
      if(gamlss.output == "Error"){return(NULL)} 
      # However if model finshes, but doesn't converge, try once to refit the model    
        # First we try refiting the model
        if (gamlss.output$converged != TRUE) {
          tryCatch({refit(gamlss.output)},
          error=function(err) {
                                    print(' Error here in refit.')
                                    a <- 'Error'
                                    return(a)} ) # Again if model fails, do not analyze that year
      }
      
      
      ## Collect the results 

      if  ('gamlss' %in% class(gamlss.output)){
          # summary.gamlss <- summary(gamlss.output, save = T)
          Mu <- coef(gamlss.output, "mu")[[1]]
          Sigma <- coef(gamlss.output, "sigma")[[1]]

          if(isTRUE(estimateCI)) {
            if(errorMethod == "boot"){ 
            # If running a bootstrap, pull out coeficients and resample using
            # Broom
              con.gl <- gamlss.control(n.cyc = 20, mu.step =1, trace =FALSE)
              icon <- glim.control(cyc = 50)
            require(broom)
            bootstrapresults <- 
            data.used %>% bootstrap(1000) %>% 
              do({gen.trun(par = c(min(.[["days"]], na.rm=T), max(.[["days"]], na.rm=T)), family = NO, type = "both") 
                tryCatch({tidy(gamlss(days~1, family=NOtr, weights =counts, data = ., trace = F,
                                      mu.start = startDays,control = con.gl, i.control=icon))},
                       error=function(err){return(data.frame("parameter"=NA, "term"=NA, "estimate"=NA,  "std.error"=NA,
                        "statistic"=NA, "p.value"=NA ))}) })
          alpha = .05
          bootCI <-   bootstrapresults %>% filter(!is.na(estimate)) %>%
                      group_by(parameter) %>% summarize(low=quantile(estimate, alpha / 2),
                                         high=quantile(estimate, 1 - alpha / 2))
          range.mu <- bootCI[1,2:3]
          range.sd  <- bootCI[2,2:3]
          jack.est <- bootstrapresults
          jack.var_mu <- NA
          jack.var_Sigma <- NA
          jackSE_mu <- NA


          } else if(errorMethod == "jackknife") {
            # Create an index of estimation confidence by runing a jacknife analysis 
                  n.counts <- nrow(data.used) # Note the number of observations
                  con.gl <- gamlss.control(n.cyc = 20, mu.step =1, trace =FALSE)
                  icon <- glim.control(cyc = 50)
                  jack.est <- data.frame(droppedRow = seq(1, n.counts), 
                    mu=rep(NA, n.counts), 
                    sd=rep(NA, n.counts), 
                    devMu = rep(NA, n.counts),
                    devSig = rep(NA, n.counts)) # Create data frame for output

                  for (row.i in seq(1,n.counts)) { # Cycle thorugh each observation, dropping it and recalculating mu and sigma
                        site.yr.i <- data.used[-row.i, ]
                        gen.trun(par = c(min(site.yr.i[["days"]], na.rm=T), max(site.yr.i[["days"]], na.rm=T)), family = NO, type = "both") 
                        updatedobject <- tryCatch({gamlss(days~1, family=NOtr, weights =counts, data = site.yr.i, trace = F,
                                                          mu.start = startDays,control = con.gl, i.control=icon)}, 
                                                            error=function(err){return(NA)})
                        mu <- ifelse(is.na(updatedobject), NA, coef(updatedobject, 'mu'))
                        sigma <- ifelse(is.na(updatedobject), NA, coef(updatedobject, 'sigma'))
                        var_i <- (mu - Mu)^2
                        vari_i_sigma <-(sigma - Sigma) ^2
                        jack.est[row.i,] <- c(row.i, mu, sigma, var_i, vari_i_sigma)



                  }
                    jack.est$site <- site
                    jack.est$year <- year
                    jack.est$species <- species
                  range.mu <- quantile(jack.est$mu, probs = c(0.025, 0.975), na.rm=T)
                  range.sd <- quantile(jack.est$sd ,  probs = c(0.025, 0.975), na.rm=T)
                  # Create an estimate of variance in jackknife estimates
                  jack.var_mu <- ( (n.counts - 1) / n.counts) * 
                            sum( jack.est$var_i , na.rm=T)
                  jack.var_Sigma <- ( (n.counts - 1) / n.counts) * 
                            sum( jack.est$vari_i_sigma , na.rm=T)
                  jackSE_mu <- jack.var_mu / sqrt(n.counts)

          }} else{  # If not estimating predictive intervals fill in NAs for estimates
            range.mu <- c(NA, NA)
            range.sd <-  c(NA, NA)
            jack.est <- NA
            jack.var_mu <- NA
            jack.var_Sigma <- NA
            jackSE_mu <- NA
          }




            # if (is.na(profileGMLSS$CI[1])) {profileGMLSS$CI <- c(999,999)}
            # if(summary.gamlss$type == "vcov") {
              results <- list(SITE = site, # Site
                              SPECIES = species, # Species
                              YEAR = year, # Year
                              MU= Mu, #mu estimate
                              SIGMA = Sigma,  #sigma estimate
                              N = nrow(site.yr), # Sample Number
                              Min = min.day, Max = max.day, # First, last days
                              # SE.Mu = summary.gamlss$coef.table[1,2], #SE for mu estimate
                              # Se.Sigma = summary.gamlss$coef.table[2,2], #SE for sigma estimate
                              # sum.type = summary.gamlss$type,
                              g.dev = gamlss.output$G.deviance, 
                              lci = range.mu[1], # Min Jackknife Mu Est
                              uci = range.mu[2], # Max Jackknife Mu Est
                              lci.sd = range.sd[1], # Min Sigma
                              uci.sd = range.sd[2], # Max Sigma
                              varMu = jack.var_mu,
                              varSigma = jack.var_Sigma,
                              jackSE_mu = jackSE_mu
              )

            # Generate plot showing predicted versus observed counts and estimated mu
             predictedDist <- as.integer(rNOFullDataNO(1000000, Mu, exp(Sigma))) %>% data.frame(Day.of.Year = .) %>% 
                        filter(Day.of.Year %in% unique(site.yr$Day.of.Year))

             require(ggplot2)
             site.yr$birds <- site.yr[[species]]
             gplotResults <-ggplot(site.yr, aes(Day.of.Year-.25, weights = birds, y = ..density..)) + geom_histogram(binwidth = .5) + 
              geom_histogram(data = predictedDist, aes(Day.of.Year+.25,weights = NULL),
                             binwidth = .5, fill = 'red', position='dodge') + ggthemes::theme_few() +
              geom_vline(xintercept = Mu,size = 2) + 
              labs(x = "Day of Year", y = "Density",title = paste0(site, "---", year, "---", species, 
                                                                   ifelse(test = isTRUE(supp), "--- with Supplemental Counts","") )) 
             if(isTRUE(supp)){
               ggsave(paste0('.images/', site, year,species,"supp.png", sep =""),gplotResults, width = 12, height = 8)
               } else{
             ggsave(paste0('.images/', site, year,species,".png", sep =""),gplotResults, width = 12, height = 8)}

            
            # add convergence, iteration number and distrubtion description
            results$conv <- TRUE
            results$iter <- gamlss.output$iter #number of iterations

        
          # If the final model finshed, but didn't converge, note this in results
          if (gamlss.output$converged != "TRUE") {
            results$conv <- FALSE}
        cat(site, year,  species, "Completed\n", sep = "-")
        
        if(is.na(jack.est)) {return(list(results = as.data.frame(results), jackknife = NULL))}
        return(list(results = as.data.frame(results), jackknife = jack.est ))#dplyr::select(jack.est, site, year, species))) # Export the results
      }
      else{return(NULL)}  # If not one of the above methods return NA
    }
}
