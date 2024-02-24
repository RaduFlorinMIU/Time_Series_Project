#--------------------Air Traffic Time Series Project Group 4--------------------
library(lubridate)
library(readr)
library(TSA)
library(forecast)
library(dplyr)
library(tseries)
library(ggplot2)
library(ggfortify)
library(ggpubr)

#---------------------Custom Functions---------------------------------------------

# Function to calculate Schwarz Bayesian Criterion (SBC)
sbc_function <- function(mod){
  aic_value <- AIC(mod)
  
  # Retrieve the number of parameters (k) and sample size (n)
  k <- length(coef(mod))
  n <- nobs(mod)
  
  # Compute BIC
  bic_value <- aic_value + k * log(n) / 2
  
  return(bic_value)
}

# Function for model validation
model_validation <- function(mod, ts_to_fit){
  fit <- fitted(mod)
  
  tstat <- mod$coef / sqrt(diag(mod$var.coef)) # test statistic of the Student test
  pvalue <- 2 * (1 - pnorm(abs(tstat))) # p-value of the test
  
  print(mod)
  
  cat("\nSBC value is :\n")
  cat(sbc_function(mod))
  
  cat("\n\nPvalues are : \n")
  print(pvalue)
  
  par(mfrow = c(1, 1))
  plot_fitted <- plot.ts(cbind(ts_to_fit, fit),
                         plot.type = "single",
                         col = c("black", "red"))
}

# Function for residual diagnostics
residual_diagnostic <- function(mod){
  res <- mod$residuals
  res_stand <- res / sqrt(mod$sigma2) # standardized residuals
  
  shapiro_test <- shapiro.test(res)
  
  par(mfrow = c(1, 1))
  result1 <- plot(res_stand)
  abline(a = 2, b = 0, col = "red")
  abline(a = -2, b = 0, col = "red")
  
  check_residual_plot <- checkresiduals(mod)
  
  print(shapiro_test)
  
  McLeod.Li.test(mod)
}

# Function to calculate and plot confidence bounds
calculate_and_plot_cb <- function(mod, ts_to_fit) {
  # Calculate confidence bound for security 80%
  cb80 <- mod$sigma^0.5 * qnorm(0.9)
  
  # Plot original data, fitted values, and confidence interval
  plot(
    cbind(ts_to_fit, fitted(mod), fitted(mod) - cb80, fitted(mod) + cb80),
    plot.type = "single", 
    col = c("black", "red", "lightblue", "lightblue"),
    lty = c(1, 1, 2, 2), 
    xlim = c(2004, 2023),
    main = "Original Data, and Confidence Interval"
  )
  
  # Proportion of points in the confidence interval
  indi <- (ts_to_fit - (fitted(mod) - cb80)) > 0 & ((fitted(mod) + cb80) - ts_to_fit) > 0
  prop <- 100 * sum(indi) / length(indi)
  
  # Return proportion value
  return(prop)
}

# Function to split data for forecasting
forcasting_data <- function(ts_to_fit){
  data.train <- window(ts_to_fit, start = c(2003, 1), end = c(2019, 8))
  
  data.test <- window(ts_to_fit, start = c(2019, 9), end = c(2023, 9))
  return(list(train = data.train, test = data.test))
}

# Function to plot predictions
prediction_plot <- function(prediction, ts_to_fit, log = 2.718, xlim = c(2019, 2023)){
  plot1 <- plot(
    ts_to_fit, 
    xlim = xlim, 
    ylim = c(0, 120000000),
    main = "Forecast Model 6"
  )
  lines(log^(prediction$pred), 
        col = "red")
  lines(log^(prediction$pred - 1.28 * prediction$se), 
        col = 4, lty = 2)
  lines(log^(prediction$pred + 1.28 * prediction$se), 
        col = 4, lty = 2)
}


# Read the data
air_traffic <- read_csv("ProjectFiles/air_traffic_dataset.csv")
summary(air_traffic)
dim(air_traffic)

#---------------------Data Cleaning---------------------------------------------

# Check for NA values
sum(is.na(air_traffic))
# Check for duplicate rows
sum(duplicated(air_traffic)) #No duplicated and N.A values

# Create a new Date column
air_traffic$Date <- make_date(air_traffic$Year, air_traffic$Month) 
# Convert 'Pax' to numeric without comma
air_traffic$Pax <- as.numeric(gsub(",", "", air_traffic$Pax))

str(air_traffic)

#---------------------Description of the Time Series----------------------------

# Plot for total passengers over time

# Grouping by Year and Month to calculate total passengers
monthly_totals <- air_traffic %>%
  group_by(Year, Month) %>%
  summarise(Total_Pax = sum(Pax))

# Identifying the highest and lowest months
highest_month <- monthly_totals[which.max(monthly_totals$Total_Pax), ]
lowest_month <- monthly_totals[which.min(monthly_totals$Total_Pax), ]

# Plot with highlighted points
ggplot(air_traffic, aes(x = Date, y = Pax)) +
  geom_line() +
  geom_point(data = filter(air_traffic, Year == highest_month$Year & Month == highest_month$Month), 
             aes(x = Date, y = Pax), color = "red", size = 3) +
  geom_point(data = filter(air_traffic, Year == lowest_month$Year & Month == lowest_month$Month), 
             aes(x = Date, y = Pax), color = "blue", size = 3) +
  labs(title = "Air Traffic Passengers Over Time with Highlights",
       subtitle = paste("Red: Highest in", month(highest_month$Month, label = TRUE), highest_month$Year,
                        " - Blue: Lowest in", month(lowest_month$Month, label = TRUE), lowest_month$Year),
       x = "Date", y = "Number of Passengers")

# We can find that the trend shows a seasonality trend with similar pattern 
# observed over time, and following a dramatic decrease in 04/2020 
# (total traffic 3013899) which gradually reached to the highest in 07/2023 with 
# 87810772 total traffic.

#---------------------Seasonality Analysis--------------------------------------

# Decomposition of additive time series
air_traffic_ts %>% 
  decompose() %>% 
  autoplot()


# Seasonal plot with different month over time
air_traffic %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  ggplot(aes(x = Month, y = Pax, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Seasonal Plot of Air Traffic Passengers", x = "Month", 
       y = "Number of Passengers")

# We detect the seasonality among months in different year. In March, June to 
# August, Octobor, and December are the peak traffic months. Except 2020 when 
# there was a sharp decline from February.
# We suppose the seasonality is due to the impact of COVID-19 on the aviation industry

#---------------------Stationarity Transformation-------------------------------

# Perform stationarity test and visualize ACF and PACF plots

# Set up a 2x2 layout for plots
par(mfrow = c(2, 2))

# Convert air traffic data to time series format
air_traffic_ts <- ts(air_traffic$Pax, start = c(2003, 1), frequency = 12)

# Plot the time series data
plot(air_traffic_ts)

# Plot the autocorrelation function (ACF)
Acf(air_traffic_ts)

# Plot the partial autocorrelation function (PACF)
Pacf(air_traffic_ts)

# Conduct Augmented Dickey-Fuller test for stationarity
adf.test(air_traffic_ts)

# First transformation: apply natural logarithm to remove increasing variance
ltraf <- log(air_traffic_ts)
par(mfrow = c(2, 2))
plot(ltraf, main = "Logarithm of Traffic at U.S. Airport")
acf(ts(ltraf, frequency = 1), main = "ACF")
pacf(ts(ltraf, frequency = 1), main = "PACF")
adf.test(ltraf)

# Second transformation: compute first-order difference to remove trend
d1_ltraf <- diff(ltraf, 1)
par(mfrow = c(2, 2))
plot(d1_ltraf, main = "First Difference of Logarithm of Traffic")
acf(ts(d1_ltraf, frequency = 1), main = "ACF")
pacf(ts(d1_ltraf, frequency = 1), main = "PACF")
adf.test(d1_ltraf)

# Third transformation: compute difference of order 12 to remove seasonality
d12_ltraf <- diff(d1_ltraf, 12)
par(mfrow = c(2, 2))
plot(d12_ltraf, main = "Without Trend and Seasonality (Log)")
acf(ts(d12_ltraf, frequency = 1), main = "ACF")
pacf(ts(d12_ltraf, frequency = 1), main = "PACF")
adf.test(d12_ltraf)

# Perform first-order difference without logarithmic transformation
d1_traf <- diff(air_traffic_ts, 1)

# Perform order of difference 12 without logarithmic transformation
d12_traf <- diff(d1_traf, 12)
par(mfrow = c(2, 2))
plot(d12_traf, main = "Without Trend and Seasonality")
acf(ts(d12_traf, frequency = 1), main = "ACF")
pacf(ts(d12_traf, frequency = 1), main = "PACF")
adf.test(d12_traf)

#---------------------Box-Jenkins methodology and ARMA--------------------------

# Identify the orders p and q using the ACF and PACF
# Comment the significance of the coefficients. If necessary, modify the model
# until all the coefficients are significant.

# Define and validate ARIMA models

# Model 1: ARIMA(1,1,2) with seasonal ARIMA(1,1,1)(1,1,1) s=12
mod1 <- stats::arima(ltraf, c(1, 1, 2), 
                     seasonal = list(order = c(1, 1, 1), period = 12), method = 'ML')

# Model 1 validation 
model_validation(mod1, ltraf)
residual_diagnostic(mod1)
calculate_and_plot_cb(mod1, ltraf)

# Model 2: ARIMA(2,1,1) with seasonal ARIMA(1,1,1)(1,1,1) s=12
mod2 <- stats::arima(ltraf, c(2, 1, 1), 
                     seasonal = list(order = c(1, 1, 1), period = 12), method = 'ML')

# Model 2 validation
model_validation(mod2, ltraf)
residual_diagnostic(mod2)
calculate_and_plot_cb(mod2, ltraf)

# Model 3: ARIMA(1,1,2) with seasonal ARIMA(2,1,3)(2,1,3) s=12
mod3 <- stats::arima(air_traffic_ts, c(1, 1, 2), 
                     seasonal = list(order = c(2, 1, 3), period = 12), method = 'ML')

# Model 3 validation 
model_validation(mod3, air_traffic_ts)
residual_diagnostic(mod3)
calculate_and_plot_cb(mod3, air_traffic_ts)

# Model 4: ARIMA(2,1,1) with seasonal ARIMA(2,1,3)(2,1,3) s=12
mod4 <- stats::arima(air_traffic_ts, c(2, 1, 1), 
                     seasonal = list(order = c(2, 1, 3), period = 12), method = 'ML')

# Model 4 validation
model_validation(mod4, air_traffic_ts)
residual_diagnostic(mod4)
calculate_and_plot_cb(mod4, air_traffic_ts)

# Model 5: ARIMA(0,1,1) with seasonal ARIMA(2,1,3)(2,1,3) s=12
mod5 <- stats::arima(air_traffic_ts, c(0, 1, 1), 
                     seasonal = list(order = c(2, 1, 3), period = 12), method = 'ML')

# Model 5 validation
model_validation(mod5, air_traffic_ts)
residual_diagnostic(mod5)
calculate_and_plot_cb(mod5, air_traffic_ts)

# Model 6: ARIMA(1,1,2) with seasonal ARIMA(0,1,1)(0,1,1) s=12
mod6 <- stats::arima(ltraf, c(1, 1, 2), 
                     seasonal = list(order = c(0, 1, 1), period = 12), method = 'ML')

# Model 6 validation 
model_validation(mod6, ltraf)
residual_diagnostic(mod6)
calculate_and_plot_cb(mod6, ltraf)

#---------------------Model Validation and Forecast-----------------------------

# Perform model validation with in-sample and out-of-sample analysis

# Define a custom function for plotting forecasts with confidence bounds
prediction_plot_bis = function (prediction, ts_to_fit, xlim = c(2019, 2023)){
  plot1 = plot(
    ts_to_fit, 
    xlim = xlim, 
    ylim = c(0, 120000000), 
    main = "Forecast Model 5"
  )
  lines((prediction$pred), 
        col = "red")
  lines((prediction$pred - 1.28 * prediction$se), 
        col = 4, lty = 2)
  lines((prediction$pred + 1.28 * prediction$se), 
        col = 4, lty = 2)
}

# Train Model 5 using ARIMA(0,1,1) with seasonal ARIMA(2,1,3)(2,1,3) s=12
mod5_train <- stats::arima(
  forcasting_data(air_traffic_ts)$train, 
  c(0, 1, 1), 
  seasonal = list(order = c(2, 1, 3), period = 12),
  method = "ML"
)

# Perform out-of-sample prediction for Model 5
pred5_test = predict(mod5_train, n.ahead = 49)
accuracy(pred5_test$pred, forcasting_data(ltraf)$test)
prediction_plot_bis(pred5_test, air_traffic_ts)

# Train Model 6 using ARIMA(1,1,2) with seasonal ARIMA(0,1,1)(0,1,1) s=12
mod6_train <- stats::arima(
  forcasting_data(ltraf)$train,
  c(1, 1, 2), 
  seasonal = list(order = c(0, 1, 1), period = 12), 
  method = 'ML'
)

# Perform out-of-sample prediction for Model 6
pred6_test = predict(mod6_train, n.ahead = 49)
accuracy(pred6_test$pred, forcasting_data(ltraf)$test)
prediction_plot(pred6_test, air_traffic_ts)

# Forecast for the next three periods

# Forecast using Model 5 for the next 36 periods
pred5 = predict(mod5, n.ahead = 36)
prediction_plot_bis(pred5, air_traffic_ts, xlim = c(2010, 2027))

# Forecast using Model 6 for the next 36 periods
pred6 = predict(mod6, n.ahead = 36)
prediction_plot(pred6, air_traffic_ts, xlim = c(2010, 2027))


