
library(ggplot2)
library(forecast)
library(tseries)

data("AirPassengers")
df <- data.frame(
  Date = as.Date(time(AirPassengers)),
  Passengers = as.numeric(AirPassengers)
)

ggplot(df, aes(x = Date, y = Passengers)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Monthly Air Passengers Data", x = "Year", y = "Number of Passengers") +
  theme_minimal()

ts_data <- ts(df$Passengers, start = c(1949, 1), frequency = 12)
adf_result <- adf.test(ts_data)

if (adf_result$p.value > 0.05) {
  ts_data_diff <- diff(ts_data)
} else {
  ts_data_diff <- ts_data
}

arima_model <- auto.arima(ts_data)
ets_model <- ets(ts_data)

forecast_horizon <- 24
forecast_arima <- forecast(arima_model, h = forecast_horizon)
forecast_ets <- forecast(ets_model, h = forecast_horizon)

autoplot(forecast_arima) +
  labs(title = "Forecast using ARIMA Model", x = "Year", y = "Predicted Passengers") +
  theme_minimal()

autoplot(forecast_ets) +
  labs(title = "Forecast using ETS Model", x = "Year", y = "Predicted Passengers") +
  theme_minimal()

train_data <- window(ts_data, end = c(1958, 12))
test_data <- window(ts_data, start = c(1959, 1))

arima_train_model <- auto.arima(train_data)
ets_train_model <- ets(train_data)

arima_forecast_test <- forecast(arima_train_model, h = length(test_data))
ets_forecast_test <- forecast(ets_train_model, h = length(test_data))

calculate_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  list(MAE = mae, RMSE = rmse)
}

metrics_arima <- calculate_metrics(test_data, arima_forecast_test$mean)
metrics_ets <- calculate_metrics(test_data, ets_forecast_test$mean)

cat("\nModel Performance:\n")
cat("ARIMA - MAE:", round(metrics_arima$MAE, 2), "RMSE:", round(metrics_arima$RMSE, 2), "\n")
cat("ETS   - MAE:", round(metrics_ets$MAE, 2), "RMSE:", round(metrics_ets$RMSE, 2), "\n")
