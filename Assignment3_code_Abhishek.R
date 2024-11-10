##Read in the dataset
df <- read.csv("C:/JAGSOM TERM 3/Business Forecasting/Assignment 3/Case_4_CAT_CORP_dataset_1.csv", stringsAsFactors = FALSE)
# dtata is monthly
#Reading in the df
head(df)
tail(df)
str(df)
names(df)

#converting the data into time-series data
install.packages("forecast")
library(forecast)

df_ts  <- ts(df$Requirement,start=c(2006,1),end=c(2010,12),frequency = 12)
str(df_ts)

#regression model for a time-series model
ts_lm <- tslm(df_ts~trend)     # Fit the linear regression model
summary(ts_lm)                # Get the regression equation  # time series sales model=783.3056+6.28*(trend)

library(ggplot2)
library(forecast)

# Create an autoplot of the time series
autoplot(df_ts) +
  geom_line(aes(y = ts_lm$fitted), color = "red", size = 1.5) +
  ggtitle("Time Plot: Transformer Requirements_Sale fig of voltage regulators") +
  ylab("Millions of voltage regulator units")

## Data has strong TREND. Investigate transformations
#Take the first difference of the data to remove the trend_component

DY <- diff(df_ts)

# Time plot of differenced data
# Create an autoplot of the differenced data
# Create an autoplot of the differenced data
# Calculate the midpoint of the y-axis range
midpoint <- mean(range(DY))

# Create an autoplot of the differenced data
plot <- autoplot(DY) +
  geom_line(aes(y = DY)) +  # Plot the differenced data
  ggtitle("Time Plot: Transformer Requirements_Sale fig of voltage regulators") +
  ylab("Millions of voltage regulator units")
# Add a dynamic midline
plot + geom_hline(yintercept = midpoint, linetype = "dashed", color = "blue")
# Series appears trend-stationary, use to investigate seasonality

# Create a seasonal plot using ggseasonplot
season_plot <- ggseasonplot(DY) +
  ggtitle("Seasonal Plot: Transformer Requirements_Sale fig of voltage regulators") +
  ylab("Millions of voltage regulator units")
# Display the seasonal plot
season_plot

# looking at another seasonal plot, the subseries plot
ggsubseriesplot(DY)

####Checking the plots - Visualization
plot(df_ts)
plot(cycle(df_ts)) # It seems there is CYCLICITY across the years
plot(aggregate(df_ts,FUN=mean)) #Aggregate the cycles and display a year on year trend
boxplot(df_ts~cycle(df_ts))

## null hypothesis: series is non- stationary for ADF
####Augmented Dickey-Fuller Test
install.packages("tseries")
library(tseries)
adf_test <- adf.test(df_ts)
print("ADF Test: Sales")
print(adf_test)
## p-value 0.01525 < alpha 0.05, we reject the null hypothesis of the ADF test. Therefore, time series df_ts is stationary.
##Since the data is stationary we do not go for log and diff transformations
##############But graph shows both trend and seasonality

##########################################################################################################

#Use a benchmark method to forecast.
# Using the seasonal Naive method as benchmark
# y_t = y_{t-s} + e_t
##########
fit<- snaive(DY)
print(summary(fit))
checkresiduals(fit)  #Residual SD: 108.4776

################
# Fit ETS method
################
fit_ets<- ets(DY)
print(summary(fit_ets))
checkresiduals(fit_ets) #Residual SD: 91.8186

###################
#Fit an ARIMA Model
###################
fit_arima <- auto.arima(DY,d=1,D=1,stepwise = FALSE,approximation= FALSE, trace= TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)  ##residual SD = sqrt(12306) = 110.93

#############################
#Forecast with ARIMA Model
#############################
fcst <- forecast(fit_arima,h =24)
autoplot(fcst, include =100)
print(summary(fcst))            # Best model: ARIMA(4,1,0)(0,1,1)[12] 

#################
# Using SMA (simple moving average)
################

##applying smoothing
install.packages("TTR")
library(TTR)

# Remove NA values from the time series data
df_ts <- na.omit(df_ts)

# Calculate the SMA using n = 4
sm <- SMA(df_ts, n = 4)

# Plot the SMA series
plot.ts(sm)

# Get the fitted values directly from SMA
fitted_values <- sm

# Calculate residuals by subtracting fitted values from original data
residuals_sm <- df_ts - fitted_values

# Check for NA values in residuals
if (any(is.na(residuals_sm))) {
  stop("Residuals contain NA values. Check data and SMA calculation.")
}

# Calculate residual standard deviation
residual_sd <- sd(residuals_sm, na.rm = TRUE)

# Print the residual standard deviation
print(residual_sd)                          ## Residual SD_sma = 111.0158

# Summary of the residual standard deviation
summary(residual_sd)



########################################
# Using Simple exponential smoothing
########################################

# Fit Simple Exponential Smoothing using Holt-Winters method
SES <- HoltWinters(df_ts, beta = FALSE, gamma = FALSE)
plot(SES)
# Plot the SES model with customized appearance
plot(SES, main = "SES Model", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Sales", ylim = c(min(df_ts), max(df_ts) * 1.2))

# Add a legend for clarity
legend("topleft", legend = c("Original", "Fitted"), col = c("red", "blue"), lty = c(1, 1), lwd = c(2, 1))
SES

# Fit Holt-Winters ES model
SES<- HoltWinters(df_ts,beta = FALSE, gamma = FALSE)

# Get the fitted values from the Holt-Winters model
fitted_values <- fitted(SES)

# Calculate residuals by subtracting fitted values from original data
residuals_SES <- df_ts - fitted_values

# Calculate residual standard deviation
residual_sd <- sd(residuals_SES)

# Print the residual standard deviation
print(residual_sd)                        # Residual_sd_HW = 114.7746
summary(residual_sd)

#######################################
#Now using Holt's Exponential Smoothing
#######################################

h<- HoltWinters(df_ts, gamma = FALSE)
plot(h)
# Plot the HES model with customized appearance
plot(h, main = "Holt's ES Model", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Sales", ylim = c(min(df_ts), max(df_ts) * 1.2))

# Add a legend for clarity
legend("topleft", legend = c("Original", "Fitted"), col = c("red", "blue"), lty = c(1, 1), lwd = c(2, 1))
h

# Fit Holt-Winters ES model
h<- HoltWinters(df_ts, gamma = FALSE)

# Get the fitted values from the Holt-Winters model
fitted_values <- fitted(h)

# Calculate residuals by subtracting fitted values from original data
residuals_h <- df_ts - fitted_values

# Calculate residual standard deviation
residual_sd <- sd(residuals_h)

# Print the residual standard deviation
print(residual_sd)                        # Residual_sd_HW = 477.156
summary(residual_sd)







#################################################################################
# Holt-Winters exponential smoothing with trend and additive seasonal components
#################################################################################

HW <- HoltWinters(df_ts)
# Plot the HWES model with customized appearance
plot(HW, main = "Holt-Winters ES Model", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Sales", ylim = c(min(df_ts), max(df_ts) * 1.2))

# Add a legend for clarity
legend("topleft", legend = c("Original", "Fitted"), col = c("red", "blue"), lty = c(1, 1), lwd = c(2, 1))
HW

# Fit Holt-Winters ES model
HW <- HoltWinters(df_ts)

# Get the fitted values from the Holt-Winters model
fitted_values <- fitted(HW)

# Calculate residuals by subtracting fitted values from original data
residuals_hw <- df_ts - fitted_values

# Calculate residual standard deviation
residual_sd <- sd(residuals_hw)

# Print the residual standard deviation
print(residual_sd)                        # Residual_sd_HW = 524.2878
summary(residual_sd)

################################
##Seasonal-Trend decomposition using LOESS (STL):

decomposed_data <- stl(df_ts, s.window = "periodic")
plot(decomposed_data)
print(decomposed_data)

# Access the decomposed components

trend_component <- decomposed_data$time.series[, "trend"]
seasonal_component <- decomposed_data$time.series[, "seasonal"]
irregular_component <- decomposed_data$time.series[, "remainder"]

#AUTOREGRESSIVE MODEL
plot(df_ts)
plot(decompose(df_ts))
print(decompose(df_ts))
decom<- decompose(df_ts)
library(forecast)
deseason <- seasadj(decom)
plot(deseason)
ddseason <- diff(deseason)
plot(ddseason)
## both seasonality and trend have been removed  in ddseason for further analysis

#################  OR (for decompose)  #######################################################################

#we can do this to remove seasonality and trend_component
install.packages("tseries")
library(tseries)
lg_df_ts <- log(df_ts)
adf_test <- adf.test(lg_df_ts)
print("ADF Test: log of Sales")
print(adf_test)

diff_df_ts <- diff(df_ts)
adf_test <- adf.test(diff_df_ts)
print("ADF Test: diff of Sales")
print(adf_test)

##Log transformations can sometimes make non-stationary data more stationary by stabilizing the variance and removing multiplicative trends. Difference transformations can remove non-constant mean or linear trends, making the data more stationary.
##Log transformations can reduce exponential growth or decay trends, while difference transformations (e.g., first differences, second differences) can remove linear trends by differencing consecutive observations.
#AUTOREGRESSIVE MODEL
plot(df_ts)
plot(decompose(df_ts))

####Check the ACF and PACF plots
acf(df_ts)  # 
acf(lg_df_ts)  # 
acf(diff_df_ts)  # 
acf(diff(lg_df_ts))  #
pacf(df_ts)
## 
#arima

## ARIMA (Auto-regressive Integrated Moving Average - defines the irregular components of a stationary time series with non-zero autocorrelation.)
## ARIMA model is represented by ARIMA(p,d,q) where
# p = autoregression order
# d = degree of differencing
# q = moving average order

install.packages("forecast")
library(forecast)

df_ts_arima <- auto.arima(df_ts)
df_ts_arima

acf(ts(df_ts_arima$residuals))
pacf(ts(df_ts_arima$residuals))

df_diffts_arima <- auto.arima(diff_df_ts)
df_diffts_arima

fitARIMA <- arima(diff_df_ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 4),method="ML")
library(lmtest)
coeftest(fitARIMA) 

df_ts_arima_forecast <- forecast(df_ts_arima,level=95,h=12)
df_ts_arima_forecast
par(mfrow=c(1,1))
plot(df_ts_arima_forecast)
summary(df_ts_arima_forecast)   #Residual SD =sqrt(7238)= 82.07, ARIMA(1,0,0)(1,1,0)[12] with drift 

