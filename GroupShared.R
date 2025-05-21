


#######
#
#  Sea Ice       Dan Enoka      s3686502
#
#######

library(TSA)
library(tseries)
library(readr)

#######################
#
#         ---    Part 1
#
#######

sea_ice <- read_csv("sea_ice.csv")
#View(sea_ice)

# Reducing to Univariate (one column) and assigning a start date 
Artic <- ts( sea_ice[1:255, -c(1,3)], start=1990, frequency = 12)
# View(Artic)
# class(Artic)

Antartica <- ts( sea_ice[1:255, -c(1,2)], start=1990, frequency = 12)
# View(Antartica)
# class(Antartica)

par(mfrow=c(1,1))
plot(Artic [65:110],type='o', xlab=" ", ylab=" ")  
par(new = TRUE) # Allow a 2nd plot overlay
plot(Antartica[65:110],type='o', col='red',
     xlab="Years:  	1995 Month 5 - 1999 Month 2 ",
     axes = FALSE,ylab="Subset of Combined Seasonal Overlay Rows 65:110",
     main = "      Artic = Black Line , Antartic = Red line ,
     Shows Seasonally Switching")  


# plot(Artic)

###########  <<===   Use this ---   Start of Visual to use  ---   

#  https://anomaly.io/seasonal-trend-decomposition-in-r/  ##<<-- from this website

library(forecast)

Artic <- ts( sea_ice[1:255, -c(1,3)], start=1990, frequency = 12)

decompose_Artic = decompose(Artic, "additive")

plot(as.ts(decompose_Artic$seasonal))
plot(as.ts(decompose_Artic$trend))
plot(as.ts(decompose_Artic$random))
plot(decompose_Artic)  ## <<-- This one puts all 4 plots as same visual

###########  <<===   Use this ---   End of Visual to use  --- 

#######  Dont Use , Explains Decompose used above,  detrending the time series

#  https://anomaly.io/seasonal-trend-decomposition-in-r/  ##<<-- from this website


library(forecast)
trend_Artic = ma(Artic, order = 30, centre = T) ##use a moving average window of 30
plot(Artic)
abline(h=10, col="red")
lines(trend_Artic, col='blue')

## can be seen better with this one
par(new = TRUE)
plot(trend_Artic)

#######    detrending the time series

detrend_Artic = Artic - trend_Artic
plot(as.ts(detrend_Artic))

##   Average the seasonality 
m_Artic = t(matrix(data = detrend_Artic, nrow = 51))
seasonal_Artic = colMeans(m_Artic, na.rm = T)
plot(rep(seasonal_Artic,4), col='red')  #  4 seasons

###########  Examining Random noise

random_Noise = Artic - trend_Artic - seasonal_Artic
plot(as.ts(random_Noise))

###########  End of Decompose    ################


#  Overlaying same area wtih different time periods for the Artic dataset

##  1. 1990 m 1   ->     73. 1996 m 1 
##  109. 1999 m 1   ->   181. 2005 m 1

Artic90_96 <- ts( sea_ice[1:73, -c(1,3)], start=1990, frequency = 12)
View(Artic)
class(Artic)

Artic99_05 <- ts( sea_ice[109:181, -c(1,3)], start=1999, frequency = 12)
View(Artic)
class(Artic)

par(mfrow=c(1,1))
plot(Artic90_96,type='o', xlab=" ", ylab=" ")  
par(new = TRUE) # Allow a 2nd plot overlay
plot(Artic99_05,type='o', col='red',
     xlab="Years: Artic 1990Month5 - 1996Month2, and Artic Red Line Overlay 1999Month5 - 2005Month2",
     axes = FALSE,ylab="Subset of Combined Seasonal Overlay Rows 65:110",
     main = "Artic Yrs 90 - 96 with, Artic Red line Overlay (Yrs 99 - 05),
     Shows as One land Mass Volume decreases and other Land Mass Volume Increases
     and how this occurs Seasonally")

############################

par(mfrow=c(1,2))
acf(Artic)
pacf(Artic)  # Possible models AR(1) and AR(2)

par(mfrow=c(1,1))
Artic.transform = BoxCox.ar(Artic) #   -------   Trying BoxCox Transform
Artic.transform$ci
# Mid-point of interval is 1.9 to 2.0 So we will take lambda as 0.85
lambda = 1.94
BC.Artic = (Artic^lambda-1)/lambda
qqnorm(BC.Artic)
qqline(BC.Artic, col = 2)
shapiro.test(BC.Artic)    

par(mfrow=c(1,2))
acf(BC.Artic)
pacf(BC.Artic)  # Possible models AR(1) and AR(2)

adf.test(BC.Artic) # Yes p-value is < 0.05 (p-value = 0.01)
par(mfrow=c(1,1))
plot(BC.Artic,type='o',main='Box-Cox transformed for Artic data set')
##There seems to be a possible downward trend here  <<-----

par(mfrow=c(1,1))
BC.Artic.dif <- diff(BC.Artic, differences = 1)
plot(BC.Artic.dif,type='o')

par(mfrow=c(1,2))
acf(BC.Artic.dif)
pacf(BC.Artic.dif) # Indicates Possible models AR(1) and AR(2)
adf.test(BC.Artic.dif) # Lag order = 6, p-value = 0.01

par(mfrow=c(1,1))
qqnorm(BC.Artic.dif)
qqline(BC.Artic.dif, col = 2)
shapiro.test(BC.Artic.dif)  # Tails still a bit far out

par(mfrow=c(1,1))
BC.Artic.dif2 <- diff(BC.Artic, differences = 2)
plot(BC.Artic.dif2,type='o')

par(mfrow=c(1,2))
acf(BC.Artic.dif2)
pacf(BC.Artic.dif2) # Indicates Possible models AR(1) and AR(2)
adf.test(BC.Artic.dif2) # Lag order = 6, p-value = 0.01

par(mfrow=c(1,1))
qqnorm(BC.Artic.dif2)
qqline(BC.Artic.dif2, col = 2)
shapiro.test(BC.Artic.dif2)  # Nope, p value is good, graph is bad

##  Possible Models  AR(1,0,0) , AR(1,1,0), AR(2,0)
#######################################

library("kdensity")
kde=kdensity(Artic)  
plot(kde,  xaxis=" ", yaxis="", main="")
#abline(v=0, col="blue")
#plot(Artic90_96,type='o', xlab=" ", ylab=" ")

# par(new = TRUE) # Allow a 2nd plot overlay
kde=kdensity(Artic.transform$loglike)  
plot(kde, col='red', xlab=" ", ylab=" ", main="")

final.arima110 <- arima(BC.Artic.dif2, c(1,0,0))  # Trying AR(1,0,0)       
tsdiag(final.arima110) 

##########################

#trying different Transforms
model2 = lm(Artic~ time(Artic))  #   -------   Trying Linear Transform
#View(model2)    ##  

par(mfrow=c(1,1))
plot(model2$residuals)

par(mfrow=c(1,1))
qqnorm(model2$residuals)
qqline(model2$residuals, col = 2)
summary(model2) # Q-Q plot isn't very good but p-value: 0.004642

adf.test(model2$residuals)  # Dickey-Fuller Test p-value = 0.5469, nope
################

eacf(Artic)

eacf(Artic, ar.max = 12, ma.max = 14)












