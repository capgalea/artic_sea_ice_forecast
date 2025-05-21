---
  title: "APPENDIX - Assignment2_MATH1318"
author: "Charles Galea (s3688570)"
date: "04 May 2019"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Load R packages
```{r message=FALSE, warning=FALSE}
rm(list=ls())
library(TSA)
library(readr)
library(dplyr)
library(fUnitRoots)
library(lmtest)
library(forecast)
```


# Load data
```{r}
eggs <- read.csv("eggs.csv", header = TRUE)
```

# Data Pre-processing
## Convert to time series
```{r}
eggs.ts <- ts(eggs[,"eggs"], start = 1981)
```

## Plot time series data
```{r}
plot(eggs.ts, ylab='Total Eggs (millions)', xlab = 'Year', type = "o", main = "Egg Depositions of Lake Huron Bloasters")
```

### In the time series plot there is trend, no seasonality, no obvious intervention, some changing variance and succeeding observations imply the existence of autoregressive behavior.


## Plot of data for neighbouring years 
```{r}
plot(y=eggs.ts,x=zlag(eggs.ts),ylab='Total Eggs (millions)', xlab='Previous Year Total Eggs (millions)' , 
     main = "Scatter plot of neighbouring year's eggs")
```


## Correlation between neighbouring values
```{r}
y = eggs.ts       # Read the data into y
x = zlag(eggs.ts)   # Generate first lag of the eggs data series
index = 2:length(x)
cor(y[index],x[index])
```


# Linear time trend model
```{r}
model.eggs.ts.ln = lm(eggs.ts~time(eggs.ts)) # label the linear trend model as model.eggs.ts.ln
summary(model.eggs.ts.ln)
```

## Plot of linear time tread fit to the data
```{r}
plot(eggs.ts, type='o', ylab='Total Eggs (millions)', xlab = 'Year', main = 'Time series plot of total eggs')
abline(model.eggs.ts.ln, col="red") #Superimpose trend line
```


### The Linear trend model was chosen because the data plot display a trend that appeared to increase with time and this could not be fit using a least squares model.

## Plot of Standardized Residuals for Linear Model
```{r}
plot(y=rstudent(model.eggs.ts.ln),x=as.vector(time(eggs.ts)),
     ylab='Standardized Residuals',xlab='Year',type='p')
```

```{r}
plot(y=rstudent(model.eggs.ts.ln),x=fitted(model.eggs.ts.ln), 
     ylab='Standardized Residuals',xlab='Fitted Trend Line Values', type='p', main = "Plot of standardized Residuals")
```

# Check normality of data
```{r}
qqnorm(eggs.ts)
qqline(eggs.ts, col = 2)
```
## Shapiro-Wilk test for normality
```{r}
shapiro.test(eggs.ts)
```
## QQ plot and Shapiro-Wilk test (p > 0.05) suggest that the time series is normally distributed. 

# Examination of original dataset
## ACF plot
```{r}
acf(eggs.ts, ci.type = 'ma', main = "Sample  ACF for eggs series")
```
### There is an indication of a damped oscillatory behaviour encourages us to look at the PACF plot.

## PACF plot
```{r}
pacf(eggs.ts, main = "Sample PACF for eggs series")
```
### The PACF plot indicates that AR(1) is worthy of first consideration.


## ADF unit-root test
```{r}
ar(diff(eggs.ts, nlag = NULL, output = TRUE))
```

```{r}
adfTest(eggs.ts, lags = 0, title = NULL,description = NULL)
```
### We can conclude non-stationarity of the series at 5% level of signifance. 


# De-trending
## First difference of data
```{r}
eggs.diff = diff(eggs.ts)
```

### Plot first difference of Box-Cox transformed data
```{r}
plot(eggs.diff, type='o', ylab='y', xlab = 'Year', main = 'First difference plot of eggs time series')
```


### ADF test for stationarity
```{r}
ar(diff(eggs.diff))
```


```{r}
adfTest(eggs.diff, lags = 4, title = NULL,description = NULL)
```
### We can conclude non-stationarity of the first difference of the series at 5% level of signifance. So, we apply the second difference:




## Second difference
```{r}
eggs.diff2 = diff(eggs.ts, difference = 2)
```

### Plot second difference
```{r}
plot(eggs.diff2, type='o', ylab='y', xlab = 'Year', main = 'Second difference plot of eggs time series')
```

### ADF test
```{r}
ar(diff(eggs.diff2))
```

```{r}
adfTest(eggs.diff2, lags = 4, title = NULL, description = NULL)
```
### We can conclude non-stationarity of the second difference of the series at 5% level of signifance. So, we apply the third difference:

### ACF plot
```{r}
acf(eggs.diff2, ci.type = 'ma', main = "Sample ACF for the third differenced eggs series")
```
### One significant lag (q = 1).

### PACF plot
```{r}
pacf(eggs.diff2, main = "Sample PACF for the third differenced eggs series")
```




## Third difference
```{r}
eggs.diff3 = diff(eggs.ts, difference = 3)
```

### Plot third difference
```{r}
plot(eggs.diff3, type='o', ylab='y', xlab = 'Year', main = 'Third difference plot of eggs time series')
```

### ADF test of third difference
```{r}
ar(diff(eggs.diff3))
```

```{r}
adfTest(eggs.diff3, lags = 3, title = NULL, description = NULL)
```

### ACF plot
```{r}
acf(eggs.diff3, ci.type = 'ma', main = "Sample ACF for the third differenced eggs series")
```
### One significant lag (q = 1).

### PACF plot
```{r}
pacf(eggs.diff3, main = "Sample PACF for the third differenced eggs series")
```
### Two significant lag (p = 2).

### We have 1 significant autocorrelation in ACF and 2 significant autocorrelations in PACF. So, possible models are {ARIMA(1,3,1) and ARIMA(2,3,1)}.

### Now perform EACF analysis and because of the size of the series we should restrict the max number of AR and MA parameters.

# EACF analysis
```{r}
eacf(eggs.diff3, ar.max = 2, ma.max = 3)
```
### The possible models becomes {ARIMA(1,3,1), ARIMA(2,3,1), ARIMA(0,3,1), ARIMA(0,3,2), ARIMA(1,3,2)}

# BIC plot
```{r}
#options(warn = -1)
res = armasubsets(eggs.diff3, nar=3, nma=3, y.name="test", ar.method="ols")
plot(res)
```
### From the BIC table we read models AR(1) and AR(2).

### The possible models now becomes {ARIMA(1,3,1), ARIMA(2,3,1), ARIMA(0,3,1), ARIMA(0,3,2), ARIMA(1,3,2), ARIMA(2,3,2)}







# Analysis of ARIMA models

1. ARIMA(1,3,1)

## Now find the parameter estimates and related signifance tests.

```{r}
model.131 = arima(eggs.ts, order = c(1,3,1), method = "ML")
coeftest(model.131)
```
### MA(1) of ARIMA(1,3,1) is significant at the 5% level of signifance but not AR(1). Try another estimation method to determine if the conclusion is supported.

```{r}
model.131.css = arima(eggs.ts,order=c(1,3,1),method='CSS')
coeftest(model.131.css)
```
### Now both AR(1) and MA(1) are significant at 5% lelvel of significance.

# Examine standardized residuals
### ARIMA(1,3,1)
```{r}
plot(rstandard(model.131), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

# Normality of standardized residuals
```{r}
e1 = residuals(model.131)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals there is an obvious change in variance in the residuals of ARIMA(1,3,1) model. Normality is not supported by the Shapiro-Wilks test at 5% level of significance.

## ACF plot of residuals for ARIMA(1,3,1) model.
```{r}
acf(residuals(model.131), ci.type = 'ma')
```

## PACF plot of residuals for ARIMA(1,3,1) model.
```{r}
pacf(residuals(model.131))
```


```{r}
Box.test(residuals(model.131), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.131, gof=15, omit.initial=F)
```

### From the ACF and PACF (except one lag) of the residuals, we can conclude that there is no significant information on the correlation structure of the series left in the residuals. This is supported by the Box-Ljung test at 5% level significance.







2. ARIMA(2,3,1)

```{r}
model.231 = arima(eggs.ts, order = c(2,3,1), method = "ML")
coeftest(model.231)
```
### MA(1) of ARIMA(2,3,1) is significant at the 5% level of signifance but not AR(1) or AR(2). Try another estimation method to see if this conclusion is supported.

```{r}
model.231.css = arima(eggs.ts,order=c(2,3,1),method='CSS')
coeftest(model.231.css)
```
### When the conditional sum of squares are used, AR(1) and AR(2) are still insigificant at the 5% level of significance.

### Plot of ARIMA(2,3,1) model
```{r}
plot(rstandard(model.231), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.231)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals there is an obvious change in variance in the residuals of ARIMA(2,3,1) model. Normality is not supported by the Shapiro-Wilks test at 5% level of significance.


## ACF plot of residuals for ARIMA(2,3,1) model.
```{r}
acf(residuals(model.231), ci.type = 'ma')
```

```{r}
pacf(residuals(model.231))
```


```{r}
Box.test(residuals(model.231), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.231, gof=15, omit.initial=F)
```

### From the ACF and PACF (except one lag) of the residuals, we can conclude that there is no significant information on the correlation structure of the series left in the residuals. This is supported by the Box-Ljung test at 5% level significance.






3. ARIMA(0,3,1)


```{r}
model.031 = arima(eggs.ts, order = c(0,3,1), method = "ML")
coeftest(model.031)
```
### MA(1) of ARIMA(0,3,1) is significant at the 5% level of signifance.

```{r}
plot(rstandard(model.031), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.031)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals there is an obvious change in variance in the residuals of ARIMA(1,3,1) model. Normality is supported by the Shapiro-Wilks test at 5% level of significance.

## ACF plot of residuals for ARIMA(0,3,1) model.
```{r}
acf(residuals(model.031), ci.type = 'ma')
```

```{r}
pacf(residuals(model.031))
```


```{r}
Box.test(residuals(model.031), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.031, gof=15, omit.initial=F)
```

### From the ACF and PACF of the residuals, we can conclude that there is some information on the correlation structure of the series left in the residuals. This is not supported by the Box-Ljung test at 5% level significance although it is close to this value.

### Therefore, the ARIMA(0,3,1) model is not a promising model for forecasting.







4. ARIMA(0,3,2)

```{r}
model.032 = arima(eggs.ts, order = c(0,3,2), method = "ML")
coeftest(model.231)
```
### MA(1) of ARIMA(0,3,2) is significant at the 5% level of signifance but not AR(1) and AR(2). Will try anoter estimation method to see if this conclusion is supported.

```{r}
model.032.css = arima(eggs.ts,order=c(0,3,2),method='CSS')
coeftest(model.032.css)
```
### When the conditional sum of squares are used both MA(1) and MA(2) are significant at 5% lelvel of significance.


# ARIMA(0,3,2)

```{r}
model.032 = arima(eggs.ts, order = c(0,3,2), method = "ML")
coeftest(model.032)
```
### MA(1) of ARIMA(0,3,2) is significant at the 5% level of signifance but not MA(2).


```{r}
model.032.css = arima(eggs.ts,order=c(0,3,2),method='CSS')
coeftest(model.032.css)
```
### Now both MA(1) and MA(2) are significant at 5% level of significance.

```{r}
plot(rstandard(model.032), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0)
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.032)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1)
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals while there is an obvious change in variance in the residuals of the ARIMA(0,3,2) model. Normality is supported by the Shapiro-Wilks test at 5% level of significance.

## ACF plot of residuals for ARIMA(0,3,2) model.
```{r}
acf(residuals(model.032), ci.type = 'ma')
```

```{r}
pacf(residuals(model.032))
```

```{r}
Box.test(residuals(model.032), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.032, gof=15, omit.initial=F)
```

### From the ACF and PACF of the residuals, we can conclude that there is no significant information on the correlation structure of the series left in the residuals. This is supported by the Box-Ljung test at 5% level significance.







5. ARIMA(1,3,2)

```{r}
model.132 = arima(eggs.ts, order = c(1,3,2), method = "ML")
coeftest(model.132)
```
### MA(1) and MA(2) of ARIMA(1,3,2) is significant at the 5% level of signifance but not AR(1). Try another estimation method to see if this conclusion is supported.

```{r}
model.132.css = arima(eggs.ts,order=c(1,3,2),method='CSS')
coeftest(model.132.css)
```
### Now both AR(1) and MA(2) are insignificant at 5% lelvel of significance.

```{r}
plot(rstandard(model.132), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.132)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals there is an obvious change in variance in the residuals of ARIMA(1,3,1) model. Normality is not supported by the Shapiro-Wilks test at 5% level of significance.

## ACF plot of residuals for ARIMA(1,3,2) model.
```{r}
acf(residuals(model.132), ci.type = 'ma')
```
## PACF plot of residuals for ARIMA(1,3,2) model.
```{r}
pacf(residuals(model.132))
```

```{r}
Box.test(residuals(model.132), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.132, gof=15, omit.initial=F)
```

### From the ACF and PACF of the residuals, we can conclude that there is no significant information on the correlation structure of the series left in the residuals. This is supported by the Box-Ljung test at 5% level significance.







6. ARIMA(1,3,0)

```{r}
model.130 = arima(eggs.ts, order = c(1,3,0), method = "ML")
coeftest(model.130)
```
### AR(1) is significant at the 5% significance level. Try another estimation method to see if this conclusion is supported.


```{r}
model.130.css = arima(eggs.ts,order=c(1,3,0),method='CSS')
coeftest(model.130.css)
```
### Now both AR(1) is insignificant at 5% level of significance.

```{r}
plot(rstandard(model.130), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.130)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

# There is no apparent trend in the residuals while there is an obvious change in variance in the residuals of ARIMA(1,3,0) model. Normality is not supported by the Shapiro-Wilks test at 5% level of significance.

## ACF plot of residuals for ARIMA(1,3,0) model.
```{r}
acf(residuals(model.130), ci.type = 'ma')
```

## PACF plot of residuals for ARIMA(1,3,0) model.
```{r}
pacf(residuals(model.130))
```

```{r}
Box.test(residuals(model.130), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.130, gof=15, omit.initial=F)
```

### From the PACF of the residuals, there was one significant residual autocorrelation remaining in the structure of the series. However, this was not supported by the Box-Ljung test at 5% level significance.








7. ARIMA(2,3,0)

```{r}
model.230 = arima(eggs.ts, order = c(2,3,0), method = "ML")
coeftest(model.230)
```
### AR(1) is significant at the 5% significance level while AR(2) is not. Try another estimation method to see if this conclusion is supported.


```{r}
model.230.css = arima(eggs.ts,order=c(2,3,0),method='CSS')
coeftest(model.230.css)
```
### Again, AR(1) is significant at 5% level of significance while AR(2) is not.

```{r}
plot(rstandard(model.230), ylab = 'Standardized Residuals', type = 'o', main = "Time series plot of transformed eggs series")
abline(h=0, col = 'blue')
```
### There is reduced variability at the start of the series and increased variation in the second half.This is not ideal though the values lie between -3 and 3, as expected.

## Normality of standardized residuals
```{r}
e1 = residuals(model.230)
qqnorm(e1, main = "QQ plot for standardized residuals")
qqline(e1, col = 'red')
```

```{r}
shapiro.test(e1)
```

### There is no apparent trend in the residuals while there is an obvious change in variance in the residuals of ARIMA(1,3,0) model. Normality is not supported by the Shapiro-Wilks test at 5% level of significance.

### ACF plot of residuals for ARIMA(2,3,0) model.
```{r}
acf(residuals(model.230), ci.type = 'ma')
```

### PACF plot of residuals for ARIMA(2,3,0) model.
```{r}
pacf(residuals(model.230))
```

```{r}
Box.test(residuals(model.230), lag = 6, type = "Ljung-Box", fitdf = 0)
```

```{r}
tsdiag(model.230, gof=15, omit.initial=F)
```

### From the PACF of the residuals, there was one significant iresidual autocorrelation in the residuals structure of the series. This was not supported by the Box-Ljung test at 5% level significance.





## For the model selection, we will use AIC and BIC values.

```{r}
AIC(model.131, model.231, model.032, model.132, model.130, model.230)
```


```{r}
BIC(model.131, model.231, model.032, model.132, model.130, model.230)
```
### Both AIC and BIC agree on the best model which is ARIMA(0,3,2).



# For the forecast of the next 5 years, we use the prediction function:

```{r}
predict(model.032, n.ahead = 5, newxreg = NULL, se.fit = TRUE)
```

## Plot the forecasts over the time series plot with confidence intervals.
```{r}
fit = Arima(eggs.ts, c(0,3,2))
plot(forecast(fit, h=5), xlab = "Year", ylab = "Total Eggs (millions)")
```

