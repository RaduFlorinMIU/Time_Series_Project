#--------------------Air Traffic Time Series Project Group 4--------------------
library(lubridate)
library(readr)
library(forecast)
library(tseries)
library(magrittr)

library(ggfortify)
library(ggplot2)

# Read the data
air_traffic <- read_csv("air traffic.csv")
summary(air_traffic)
dim(air_traffic)

#---------------------Data Cleaning---------------------------------------------

# Check for NA values
sum(is.na(air_traffic))
# Check for duplicate rows
sum(duplicated(air_traffic))

# Create a new Date column
air_traffic$Date <- make_date(air_traffic$Year, air_traffic$Month) 
# Convert 'Pax' to numeric without comma
air_traffic$Pax <- as.numeric(gsub(",", "", air_traffic$Pax))

str(air_traffic)

#---------------------Description of the Time Series----------------------------

# Time series plot for total passengers over time

# Grouping by Year and Month to calculate total passengers
monthly_totals <- air_traffic %>%
  group_by(Year, Month) %>%
  summarise(Total_Pax = sum(Pax))

# Identifying the highest and lowest months
highest_month <- monthly_totals[which.max(monthly_totals$Total_Pax), ]
lowest_month <- monthly_totals[which.min(monthly_totals$Total_Pax), ]

# Plotting the time series with highlighted points
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

# We can find that

#---------------------Seasonality Analysis--------------------------------------

# Seasonal plot with different month over time
air_traffic %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  ggplot(aes(x = Month, y = Pax, group = Year, color = Year)) +
  geom_line() +
  labs(title = "Seasonal Plot of Air Traffic Passengers", x = "Month", 
       y = "Number of Passengers")

# We detect the seasonality....
# We suppose the seasonality is due to the impact of COVID-19 on the aviation industry

#---------------------Stationarity Transformation-------------------------------

# Stationarity test 
# ACF and PACF plots
par(mfrow=c(2,2))
Acf(air_traffic_ts)
Pacf(air_traffic_ts)

# adf.test(air_traffic_ts)

# First transformation: log (to remove increasing variance)
ltraf <- log(air_traffic_ts)
par(mfrow=c(2,2))
plot(ltraf, 
     main="log(traffic) at U.S airport")
acf(ts(ltraf, 
       frequency=1), main="ACF")
pacf(ts(ltraf, 
        frequency=1), main="PACF")

# Second transformation: 1st order difference (to remove trend)
d1_ltraf <- diff(ltraf, 1)
par(mfrow=c(2,2))
plot(d1_ltraf, 
     main="1st diff of log(traffic)")
acf(ts(d1_ltraf, 
       frequency=1), main="ACF")
pacf(ts(d1_ltraf, 
        frequency=1), main="PACF")

# Third transformation: difference of order 12 (to remove seasonality)
d12_ltraf <- diff(d1_ltraf, 12)
par(mfrow=c(2,2))
plot(d12_ltraf, 
     main="without trend and seasonality")
acf(ts(d12_ltraf, 
       frequency=1), main="ACF")
pacf(ts(d12_ltraf, 
        frequency=1), main="PACF")

# Seasonality has been removed, but we observe some significant coefficients at 
# lag 12 for the ACF and up to lag 24 for the PACF
# We also observe on the plot some very large values corresponding
# to perturbations of the traffic at specific dates.

#---------------------Box-Jenkins methodology and ARMA--------------------------

# Identify the orders p and q using the ACF and PACF

# Comment the significance of the coefficients. If necessary, modify the model
# until all the coefficients are significant.

# Residual diagnostic

# Comment the information criteria values (AIC, SBC) to select the best model

#---------------------Model Validation and Forecast-----------------------------

# Model validation with an in-sample and out-of-sample analysis 

# Forecast for the next three periods