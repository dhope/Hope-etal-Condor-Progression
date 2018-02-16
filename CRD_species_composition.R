# Interpolation of missing data from Copper River Delta, AK
# Script modified by Mark Drever from one written by
# Mark Drever for Drever et al. 2014 Journal of Field Ornithology

require(lme4)
require(lattice)
require(tidyverse)
## read in data
crd <- read.csv('CRD_Cleaned_noHART.csv') %>% filter(!is.na(Totals))#CRD_Cleaned(2).csv')

# some variable prep
crd$prop.WESA <- crd$WESA  / crd$Totals
crd$prop.DUNL <- crd$DUNL  / crd$Totals
crd$Year.factor <- as.factor(crd$Year)


## WESA

with(crd, plot(Date, prop.WESA))

prop.model1 <- lmer(prop.WESA ~ Date + I(Date^2) + (Date + I(Date^2)|Year.factor), data = crd)
#prop.model2 <- lmer(prop.WESA ~ Date + I(Date^2) + (1|Year.factor), data = crd)
coefs1 <- data.frame(Year = c(1992, 1993, 1994, 1995), coefs = coef(prop.model1)$Year.factor) 

coefs1 <- merge( coefs1, crd[,c( 'Date' , 'Totals', 'Year')])

coefs1$pred.prop.WESA <- coefs1$coefs..Intercept. + coefs1$coefs.Date*coefs1$Date + coefs1$coefs.I.Date.2.*coefs1$Date^2
coefs1$pred.WESA <- round(coefs1$Totals*coefs1$pred.prop.WESA  )

crd <- merge(crd, coefs1[,c( 'Year', 'Date','pred.prop.WESA', 'pred.WESA')])


pdf('pred_WESAprops.pdf', width = 7, height = 10 )

opar <- par(mfrow = c(2,1))

with(crd, plot(prop.WESA,pred.prop.WESA, las = 1, ylim = c(0,1), xlim = c(0,1)))
text(0.9,0, paste('corr =', round(with(crd,cor.test(prop.WESA,pred.prop.WESA )$estimate),2) ) )
abline(0,1, col = 'red')

with(crd, plot(Date, prop.WESA, las = 1))
for (yr in unique(crd$Year)) {
with(crd[crd$Year == yr,], lines(Date, pred.prop.WESA, col = 'blue'))
}

par(opar)

dev.off()


## DUNL 

with (crd, plot(Date, prop.DUNL, las = 1))

prop.model2 <- lmer(prop.DUNL ~ Date + I(Date^2) + (Date + I(Date^2)|Year.factor), data = crd)
summary(prop.model2)

coefs2 <- data.frame(Year = c(1992, 1993, 1994, 1995), coefs = coef(prop.model2)$Year.factor) 
coefs2 <- merge( coefs2, crd[,c('Date' , 'Totals', 'Year')])
coefs2$pred.prop.DUNL <- coefs2$coefs..Intercept. + coefs2$coefs.Date*coefs2$Date + coefs2$coefs.I.Date.2.*coefs2$Date^2
coefs2$pred.DUNL <- round(coefs2$Totals*coefs2$pred.prop.DUNL  )

crd <- merge(crd, coefs2[,c('Year', 'Date','pred.prop.DUNL', 'pred.DUNL')])
crd <- crd[order(crd$Year, crd$Date),]

write.csv(crd,'predicted DUNL and WESA.csv', row.names = FALSE)

pdf('pred_DUNLprops.pdf', width = 7, height = 10)

opar <- par(mfrow = c(2,1))

with(crd, plot(prop.DUNL,pred.prop.DUNL, las = 1, ylim = c(0,1), xlim = c(0,1)))
text(0.9,0, paste('corr =', round(with(crd,cor.test(prop.DUNL,pred.prop.DUNL)$estimate),2) ) )
abline(0,1, col = 'red')

with(crd, plot(Date, prop.DUNL, las =1))
for (yr in unique(crd$Year)) {
with(crd[crd$Year == yr,], lines(Date, pred.prop.DUNL, col = 'blue'))
}

par(opar)
dev.off()



pdf('Daily counts - WESA.pdf', height = 9, width = 6.5)
xyplot(WESA ~ Date|as.factor(Year), data = crd, main = 'Western Sandpiper',
       as.table = T,  
       xlab = 'Day of Year (DOY)', ylab = 'Number of birds',    # scales = 'free',
       panel=function(x, y, subscripts){
         panel.points(x, y,    col = 'black'  , type = 'b'   )
         panel.points( crd$Date[subscripts],  crd$pred.WESA[subscripts], col = 'blue' )
       },  subscripts=T)
dev.off()



pdf('Daily counts - DUNL.pdf', height = 9, width = 6.5)
xyplot(DUNL ~ Date|as.factor(Year), data = crd, main = 'Dunlin',
       as.table = T,  
       xlab = 'Day of Year (DOY)', ylab = 'Number of birds',    # scales = 'free',
       panel=function(x, y, subscripts){
         panel.points(x, y,    col = 'black'  , type = 'b'   )
         panel.points( crd$Date[subscripts],  crd$pred.DUNL[subscripts], col = 'blue' )
       },  subscripts=T)
dev.off()



