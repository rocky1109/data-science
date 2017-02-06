
#install.packages("fpp")
library(fpp)

#install.packages("data.table")
library(data.table)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("xts")
library(xts)

#install.packages("zoo")
library(zoo)


# Clearing the global environment
rm(list = ls())


base_dir = getwd()
in_dir = file.path(base_dir, "in")
out_dir = file.path(base_dir, "out")
in_file = file.path(in_dir, "in.csv")


dir.create(out_dir, showWarnings = FALSE)


# Read the input from csv file.
demand_df <- read.csv(file.path(in_dir, "in.csv"), header = TRUE, sep = ",", na.strings= c("$null$", " "))
# Parse the date into year month format.
demand_df$Date <- strptime(paste(strftime(as.yearmon(as.character(demand_df$Date), "%Y%m")), "1"), "%Y-%m-%d")

# Get all product names.
product_names <- unique(demand_df$Product)

for(product_name in as.vector(product_names)){
  
  current_product = product_name
  
  current_dir = file.path(out_dir, current_product)
  dir.create(current_dir, showWarnings = FALSE)
  
  # Get the data frame for respective product.
  product = demand_df[demand_df$Product == current_product, ]
  
  #png(filename=file.path(current_dir, paste(current_product, ".png", sep = "")))
  # Plot the graph of Demand vs Time.
  #plot(data = product1, aes(x = Date, y = Demand)) + geom_line()
  
  
  # Make 'Date' as index of the data frame.
  rownames(product) <- product$Date
  
  # Slice for the required data.
  product = product['Demand']
  
  # Convert into TimeSeries.
  tseries <- ts(product[[1]], start = c(2013, 1), end = c(2015, 12), frequency = 12)
  png(filename=file.path(current_dir, paste(current_product, ".png", sep = "")))
  plot(tseries)
  dev.off()
  
  # Plot the graph of Demand vs Time.
  #plot(data = product1, aes(x = Date, y = Demand)) + geom_line()
  #
  
  # Sufficient large training set data.
  train_data <- window(tseries, start = c(2013, 1), end = c(2015, 5))
  
  # Testing data.
  test_data <- window(tseries, start = c(2015, 6), end = c(2015, 12))
    
  ### forecast with mean, naive, and seasonal naive models
  # mean model
  demand_mean <- meanf(train_data, h = 18)
  
  # naive model
  demand_naive <- naive(train_data, h = 18)
  
  # seasonal naive model
  demand_sn <- snaive(train_data, h = 18)
  plot(demand_sn, xlab = 'Time', ylab = 'Average Demand / Month')
  lines(test_data, lwd = 2, col = 'red')
  legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Seasonal Naive Forecast","Actual Counts"))
  
  ### plots of actual data and models
  plot(tseries)
  lines(demand_mean$mean, col = 'blue', lwd = 2)
  lines(demand_naive$mean, col = 'red', lwd = 2)
  lines(demand_sn$mean, col = 'green', lwd = 2)
  legend("topleft",lty=1, lwd = 2 ,col=c(2,3,4), legend=c("Mean method","Naive method","Seasonal naive method"))
  
  
  ### seasonal decomposition
  # breaking down the data into seasons and trend
  demand_stl <- stl(tseries, s.window = 'periodic', t.window = 12, robust = T)
  plot(demand_stl)
  summary(demand_stl)
  
  
  plot(tseries, col = 'grey', ylab = 'Average Demand / Month', main = 'Seasonally Adjusted Average Demand / Month')
  lines(seasadj(demand_stl),col="red",ylab="Seasonally adjusted")
  legend("topleft",lty=1, lwd = 2 ,col=c('grey', 'red'), legend=c("Original data","Seasonally-adjusted data"))
  
  seasonal <- demand_stl$time.series[,1]
  trend <- demand_stl$time.series[,2]
  remainder <- demand_stl$time.series[,3]
  
  plot(trend)
  plot(seasonal)
  plot(remainder)
  plot(trend+remainder, ylab = 'Average Demand / Month', main = 'Seasonally Adjusted Average Demand / Month')
  
  
  monthplot(tseries, xlab = 'Months', ylab = 'Average Demand / Month', main = 'Average Demand in each month')
  seasonplot(tseries, year.labels.left = T, xlab = 'Time', ylab = 'Average Demand / Month', main = 'Seasonality of Average Demand in each year')
  
  # stl model 
  demand_stltrain <- stl(train_data, s.window = 'periodic', t.window = 12, robust = T)
  plot(demand_stltrain)
  demand_stlf <- forecast(demand_stltrain, method = 'naive', h = 18)
  summary(demand_stltrain)
  
  plot(demand_stlf, xlab = 'Time', ylab = 'Average Demand / Month')
  lines(test_data, lwd = 2, col = 'red')
  legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("STL Forecast","Actual Demand"))
  
  
  ### holt-winters
  demand_hwf <- hw(train_data, seasonal = 'multiplicative', damped = T, initial = 'optimal', h = 18)
  lines(demand_hwf$mean, col = 'green', lwd = 2)
  plot(demand_hwf)
  
  demand_hwf2 <- hw(train_data, seasonal = 'additive', damped = T, initial = 'optimal', h = 18)
  
  dev.off()
  plot(demand_hwf2, xlab = 'Time', ylab = 'Average Demand / Month')
  lines(test_data, lwd = 2, col = 'red')
  legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Holt-Winters Forecast","Actual Counts"))
  
  ### arima
  demand_arima <- auto.arima(train_data, stepwise = F, approximation = F)
  demand_arimaf <- forecast(demand_arima, h = 18)
  lines(demand_arimaf$mean, col = 'purple', lwd = 2)
  summary(demand_arima)
  plot(demand_arimaf)
  
  # regression
  demand_reg <- tslm(train_data ~ trend + season)
  summary(demand_reg)
  demand_regf <- forecast(demand_reg, h = 18)
  lines(demand_regf$mean, col = 'blue', lwd = 2)
  
  plot(demand_regf, xlab = 'Time', ylab = 'Average Demand / Month')
  lines(test_data, lwd = 2, col = 'red')
  legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Regression Forecast","Actual Counts"))
  
  
  ### comparison plot and legend
  dev.off()
  par(oma = c(2, 1, 1, 1))
  
  png(filename=file.path(current_dir, paste(paste(current_product, "_forecast_all"), ".png", sep = "")))
  plot(tseries, xlab = 'Time', ylab = 'Average Demand / Month', main = 'Comparison of Forecasts to actual data')
  lines(demand_sn$mean, col = 'purple', lwd = 2)
  lines(demand_stlf$mean, col = 'red', lwd = 2)
  lines(demand_hwf2$mean, col = 'green', lwd = 2)
  lines(demand_regf$mean, col = 'blue', lwd = 2)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend('bottom','groups',c('Linear Regression', 'Seasonal Naive', "STL","Holt-Winters","Regression"), lty = 1, lwd = 2, col = c('orange', 'purple','red', 'green', 'blue'), ncol = 3, bty = 'n')
  dev.off()
  
  ### checking accuracy
  accuracy(demand_sn, test_data)[, 'MASE']
  accuracy(demand_stlf, test_data)[, 'MASE']
  accuracy(demand_hwf, test_data)[, 'MASE']
  accuracy(demand_hwf2, test_data)[, 'MASE']
  accuracy(demand_regf, test_data)[, 'MASE']
  
  accuracy(demand_sn, test_data)[, 'RMSE']
  accuracy(demand_stlf, test_data)[, 'RMSE']
  accuracy(demand_hwf2, test_data)[, 'RMSE']
  accuracy(demand_regf, test_data)[, 'RMSE']
  
  
  write.zoo(demand_sn$mean, sep = ",", file = file.path(current_dir, paste(paste(current_product, "_forecast_snaive"), ".csv", sep = "")))
  write.zoo(demand_stlf$mean, sep = ",", file = file.path(current_dir, paste(paste(current_product, "_forecast_stl"), ".csv", sep = "")))
  write.zoo(demand_hwf2$mean, sep = ",", file = file.path(current_dir, paste(paste(current_product, "_forecast_hw"), ".csv", sep = "")))
  write.zoo(demand_regf$mean, sep = ",", file = file.path(current_dir, paste(paste(current_product, "_forecast_reg"), ".csv", sep = "")))
  
}
