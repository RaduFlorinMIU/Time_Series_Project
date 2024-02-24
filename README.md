# Airport Traffic Time Series Forecasting

## Objectives

The objective of this project is to utilize the Box-Jenkins methodology to forecast airport traffic time series data. Specifically, the project involves:

- Analyzing the monthly data of air traffic from a specific airport.
- Developing an ARMA (AutoRegressive Moving Average) process on the stationary data.
- Validating the model using in-sample and out-of-sample analysis.
- Providing forecasts for the next three periods.

## Data Set

The data set consists of monthly observations of air traffic from a specific airport. It contains approximately 200 observations.

## Presentation

### Description of the Time Series

The time series data will be described in terms of its trends, seasonality, and any notable patterns or anomalies.

### Stationarity

#### Initial Analysis
- Autocorrelation Function (ACF) plot
- Partial Autocorrelation Function (PACF) plot
- Time series plot

#### Transformation Steps
1. Logarithmic transformation to stabilize variance.
2. First-order differencing to remove trend.
3. Additional differencing to eliminate seasonality.

### Box-Jenkins Methodology

#### Identification of Model Orders
- Analyzing ACF and PACF plots to determine orders (p and q) for the ARMA process.

#### Coefficient Significance
- Commenting on the significance of coefficients and modifying the model if necessary to ensure all coefficients are significant.

#### Residual Diagnostic
- Checking for normality, non-autocorrelation, and homoscedasticity of residuals.

#### Information Criteria Values
- Interpretation of information criteria values (AIC, SBC) to select the best model.

### Model Validation and Forecast

#### In-Sample and Out-of-Sample Analysis
- Validating the model using in-sample and out-of-sample analysis.

#### Forecasting
- Providing forecasts for the next three periods.

## Conclusion

The project aims to provide a comprehensive analysis of airport traffic time series data, including data transformation, model development, validation, and forecasting. It seeks to offer insights into the patterns and trends in air traffic, facilitating better decision-making for stakeholders.
