# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(tseries)
# Load data
df <- read.csv("D:/Book1.csv")
colnames(df) <- tolower(colnames(df))

# data cleaning
df$date <- as.Date(df$date)

df <- df %>%
  filter(!is.na(date), !is.na(weekly_sales)) %>%
  arrange(date) %>%
  select(date, weekly_sales)

=
# add promotion feature
df$promo <- ifelse(month(df$date) %in% c(11,12), 1, 0)

# reduce data
df <- tail(df, 5000)

# visualization
ggplot(df, aes(x = date, y = weekly_sales)) +
  geom_line(color = "orange") +
  ggtitle("Weekly Sales Over Time")

# time series conversion
ts_data <- ts(df$weekly_sales, frequency = 52)

# decomposition
decomp <- stl(ts_data, s.window = "periodic")
plot(decomp)

# ARIMA  model (with promotion)
model_arima <- auto.arima(ts_data,
                          xreg = df$promo,
                          stepwise = TRUE,
                          approximation = TRUE)

future_promo <- rep(0, 30)

forecast_arima <- forecast(model_arima,
                           xreg = future_promo,
                           h = 30)

autoplot(forecast_arima) +
  ggtitle("ARIMAX Forecast (with Promotion)")

# ETS model
model_ets <- ets(ts_data)

forecast_ets <- forecast(model_ets, h = 30)

autoplot(forecast_ets) +
  ggtitle("ETS Forecast")


# train-test split
n <- length(ts_data)
split_index <- floor(0.8 * n)

train <- ts(ts_data[1:split_index], frequency = 52)
test  <- ts(ts_data[(split_index + 1):n], frequency = 52)

train_promo <- df$promo[1:split_index]
test_promo  <- df$promo[(split_index + 1):n]

# ARIMAX on train
model_train_arima <- auto.arima(train,
                                xreg = train_promo,
                                stepwise = TRUE,
                                approximation = TRUE)

pred_arima <- forecast(model_train_arima,
                       xreg = test_promo,
                       h = length(test))


# ETS on train
model_train_ets <- ets(train)
pred_ets <- forecast(model_train_ets, h = length(test))

# accuracy comparison
cat("ARIMA Accuracy:\n")
acc_arima <- accuracy(as.numeric(pred_arima$mean), as.numeric(test))
print(acc_arima)

cat("ETS Accuracy:\n")
acc_ets <- accuracy(as.numeric(pred_ets$mean), as.numeric(test))
print(acc_ets)


# final model decision
rmse_arima <- acc_arima[2]
rmse_ets   <- acc_ets[2]

if (rmse_arima < rmse_ets) {
  print("Final Conclusion: ARIMA performs better")
} else {
  print("Final Conclusion: ETS performs better")
}

# visualization comparison
autoplot(pred_arima) +
  autolayer(test, series = "Actual", color = "red") +
  ggtitle("ARIMAX: Forecast vs Actual")

autoplot(pred_ets) +
  autolayer(test, series = "Actual", color = "blue") +
  ggtitle("ETS: Forecast vs Actual")
