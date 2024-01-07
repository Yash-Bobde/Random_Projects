# Load necessary libraries
library(forecast)
library(ggplot2)

# Load historical sales data for hair oil (replace with actual data)
hair_oil_sales <- c(...)  # Replace with actual data

# Convert data into a time series object
hair_oil_ts <- ts(hair_oil_sales, start = c(2020, 1), frequency = 12)

# Perform time series analysis
plot(hair_oil_ts)

# Decompose the time series
decomposed_ts <- stl(hair_oil_ts, s.window = "periodic")
plot(decomposed_ts)

# Perform ARIMA modeling
arima_model <- auto.arima(hair_oil_ts)
summary(arima_model)

# Forecast future sales
future_sales <- forecast(arima_model, h = 12)
plot(future_sales)

# Evaluate model performance
accuracy(future_sales)

