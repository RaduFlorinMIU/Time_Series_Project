######################################
# Stationarity for Toulouse Airport traffic
######################################

#-------------
# Import data
#-------------
TlseData <- read.csv2('TlseAirport2019.csv')
traffic.ts <- ts(TlseData$trafic, 
                 start=c(1982,1), frequency=12)

# Stationarity of traffic.ts
#----------------------------
par(mfrow=c(2,2))
plot(traffic.ts, 
     main="traffic at Toulouse airport")
acf(ts(traffic.ts, 
       frequency=1), main="ACF")
pacf(ts(traffic.ts, 
        frequency=1), main="PACF")

# plot: non constant trend and also increasing variance
# ACF: persistence of significant coefficients
# conclusion: the series is not stationary

# First transformation: log (to remove increasing variance)
#----------------------------------------------------------
ltraf <- log(traffic.ts)
par(mfrow=c(2,2))
plot(ltraf, 
     main="log(traffic) at Toulouse airport")
acf(ts(ltraf, 
       frequency=1), main="ACF")
pacf(ts(ltraf, 
        frequency=1), main="PACF")

# The log series is still nonstationary
# with non constant trend and ACF not decaying fast to 0

# Second transformation: 1st order difference (to remove trend)
#--------------------------------------------------------------
d1_ltraf <- diff(ltraf, 1)
par(mfrow=c(2,2))
plot(d1_ltraf, 
     main="1st diff of log(traffic)")
acf(ts(d1_ltraf, 
       frequency=1), main="ACF")
pacf(ts(d1_ltraf, 
        frequency=1), main="PACF")
# The ACF has significant coefficients repeated every 12 months
# no fast decay to 0
# the series is still not stationary

# Third transformation: difference of order 12 (to remove seasonality)
#---------------------------------------------------------------------
d12_ltraf <- diff(d1_ltraf, 12)
par(mfrow=c(2,2))
plot(d12_ltraf, 
     main="without trend and seasonality")
acf(ts(d12_ltraf, 
       frequency=1), main="ACF")
pacf(ts(d12_ltraf, 
        frequency=1), main="PACF")

# Seasonality has been removed
# Still, we observe some significant coefficients at lag 12 for 
# the ACF and up to lag 24 for the PACF
# We also observe on the plot some very large values corresponding
# to perturbations of the traffic at specific dates.


#######################################
# Identification of the orders p and q
######################################
#d=1, s=12, D=1
# p, q, P, Q  ?
# ACF: q=1 and Q=1
# PACF: p=1 and P=1  or P=2

# multiplicative SARIMA(1,1,1)(1,1,1) s=12
# multiplicative SARIMA(P,D,Q)(p,d,q)
#arima(ltraf, c(p,d,q), seasonal=list(order=c(P,D,Q), period=12) )
mod1 <- arima(ltraf,c(1,1,1), 
              seasonal=list(order=c(1,1,1), 
                            period=12), method='ML')

mod1
# AIC= -1528.81

# Plot of the fitted value
#-------------------------
install.packages('TSA')
library(TSA)
fit1 <- fitted(mod1)
par(mfrow=c(1,1))
plot.ts(cbind(ltraf, fit1), 
        plot.type='single', 
        col=c('black', 'red'))


#-------------------
# Validation of mod1
#-------------------

#''''''''''''''''''''''''''''''''''''''''''''''''
# Significance of coefficients: pvalue analysis
#''''''''''''''''''''''''''''''''''''''''''''''''

#Student Test Ho: coeff=0 against H1: coeff # 0
# If pvalue < 5%, then we reject Ho

mod1$coef #value of coefficients
mod1$var.coef # variance matrix of coefficients

tstat <- mod1$coef/sqrt(diag(mod1$var.coef)) # test statistic of the Student test
pvalue <- 2*(1-pnorm(abs(tstat))) #pvalue of the test

pvalue
## The pvalues of AR1 and SAR1 coeff are larger than 5%
# So we could remove the AR part from the model
#multiplicative SARIMA(P=0, D=1, Q=1)(p=0, d=1, q=1)


#''''''''''''''''''''''''''''''''''''''''''''''''
# Residuals analysis
#''''''''''''''''''''''''''''''''''''''''''''''''

# General overview
install.packages("forecast")
library(forecast)
checkresiduals(mod1)

# From the plot of the residuals, we observe one very low
# value (potentially due to an external event that 
# perturbated the traffic for one specific date)

#From the ACF of the residuals, we observe that 
# all the coefficients are non significant except
# at lag 5... (we almost satisfy the WN assumption)

# From the histogram, we observe that the shape of the
# distribution is close to Gaussian
# there are some extreme values both very low and very large

# Analysis of normality
#''''''''''''''''''''''

res1 <- mod1$residuals
res_stand <- res1/sqrt(mod1$sigma2) # standardized residuals

plot(res_stand)
abline(a=2, b=0, col="red")
abline(a=-2, b=0, col="red")

# We observe on the plot several values outside
# the interval [-2 ; 2]
# These values correspond the dates where the model
# does not fit well to the data

out1 <- which(res_stand< -4) 
# identify the row number of the outlier
out1 # the outlier corresponds to the observation 50

install.packages("zoo")
library(zoo)
index(res_stand)[out1] # date of the outlier value
# February 1986
traffic.ts[out1] #value of the outlier
traffic.ts[(out1-2):(out1+2)] # values around the outlier
traffic.ts[(out1-14):(out1-10)] # 12 months before the outlier

# next model to be run: add a dummy variable 0/1 

## QQplot
# Check whether the points are close to the line
qqnorm(res1)
qqline(res1)
# The points are far from the line
# at the boundaries (presence of outliers)

## Shapiro test
shapiro.test(res1)
# H0: the variable is normally distributed
# H1: the variable is not normally distributed
# pvalue <<< 5% so we reject Ho: 
# the residuals are not normally distributed

#-------------
# Prediction
#-------------

# Check the fit with respect to confidence bounds
#------------------------------------------------

cb80 <- mod1$sigma2^.5*qnorm(0.9) 
# confidence bound of security 80%
plot(cbind(ltraf, fit1-cb80, fit1+cb80), 
     plot.type="single", lty=c(1,2,2), 
     xlim=c(2000, 2006))

# Proportion of points in the confidence interval
indi <- (ltraf-(fit1-cb80))>0&(fit1+cb80-ltraf)>0
prop <-  100*sum(indi)/length(indi) 
prop  
# prop = 85% > 80% the fit is considered as good!


# In-sample (80%) / out-of-sample (20%) analysis
#-----------------------------------
end(ltraf)
data.train <- window(ltraf, start=c(1982,1), end=c(2012,1))
# 361 observations
data.test <- window(ltraf, start=c(2012,2), end=c(2019,7))
# 90 observations

mod1.train <- arima(data.train,c(1,1,1), 
                    seasonal=list(order=c(1,1,1), 
                                  period=12), method='ML')
pred1.test <- predict(mod1.train, n.ahead=90)

library(forecast)
accuracy(pred1.test$pred, data.test)

#               ME       RMSE        MAE        MPE      MAPE
#Test set -0.00224581 0.06115729 0.04870164 -0.0194098 0.3625947
#               ACF1 Theil's U
#Test set 0.6199073 0.7205598

# The best model has the lowest values for the accuracy parameters

# plot of the observed values and prediction
plot(traffic.ts, xlim=c(2016, 2020), 
     ylim=c(400000, 1500000))
lines(2.718^(pred1.test$pred), 
      col="red")
lines(2.718^(pred1.test$pred-1.28*pred1.test$se), 
      col=4, lty=2)
lines(2.718^(pred1.test$pred+1.28*pred1.test$se), 
      col=4, lty=2)


#qnorm(0.90)


#--------------------------------
# 2nd model: without the AR part
#----------------------------------
mod2 <- arima(ltraf,c(0,1,1), 
              seasonal=list(order=c(0,1,1), 
                            period=12), method='ML')

mod2
# AIC=-1532.08 < -1528
# The second model seems to fit a little bit better....
# Check the significance of coeff and
# the residuals and the predictive power....
checkresiduals(mod2)
# The residuals behave very similarly to mod1

#-----------------------------------------------------
# 3rd model: include the dummy variable, february 1986
#-----------------------------------------------------









  
  
  
  
  























































