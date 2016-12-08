#install.packages("dplyr", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
library(dplyr)
library(ggplot2)

# читаем данные - тут нужно поменять директорию на свою, откуда файл читать
#setwd("C:/R/")
sales_data <- read.csv("series.csv", stringsAsFactors = FALSE, sep=";", dec=",", encoding = "UTF-8")

# линейный тренд по цене
price_lm <- lm(data = sales_data, Price~Number)

# доопределяем цену (не predict, потому что часть столбца уже есть)
for (i in 201:261){
  sales_data$Price[i] <- price_lm$coefficients["Number"]*i + price_lm$coefficients["(Intercept)"]
}
ggplot(sales_data, aes(x=Number, y=Price)) + geom_smooth(se=FALSE, method="lm") + geom_line(size = 1) +
  ggtitle("Линейная регрессия цены")

# скользящее среднее продаж
sales_data$MovingAverage <- NA
for (i in 27:174){
  moving_sum <- sum(sales_data$Sales[(i-25):(i+25)]) + (sales_data$Sales[(i-26)] + sales_data$Sales[(i+26)])/2
  sales_data$MovingAverage[i] <- moving_sum/52
}

# аддитивная сезонность
sales_data$Seasonality <- NA
for (i in 1:52){
  # выбираем соответствующие недели
  temp_data <- filter(sales_data,ISOWeek %% 100 == i)
  week_data <- temp_data$Number
  sales_data$Seasonality[week_data] <- mean(temp_data$Sales - temp_data$MovingAverage, na.rm = TRUE)
}
# 53 - отдельная история
extraweek <- filter(sales_data,ISOWeek %% 100 == 53)$Number
sales_data$Seasonality[extraweek] <- (sales_data$Seasonality[extraweek-1] + sales_data$Seasonality[extraweek+1])/2

# очищенные продажи
sales_data$CleanSales <- sales_data$Sales - sales_data$Seasonality

#=========================
# регрессия продаж по цене
#=========================

# линейная - the best one
linear_sales_price <- lm(data = sales_data, Sales~Price)
sales_data$LinearSales <- predict(linear_sales_price, sales_data)
rmse_linear <- sqrt(mean((sales_data$Sales - sales_data$LinearSales)^2, na.rm = TRUE))

ggplot(sales_data) + geom_point(aes(x=Price, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Price, y=LinearSales, color="Linear"), size = 1) +
  ggtitle("Линейная регрессия продаж по цене")

# экспоненциальная
exp_sales_price <- lm(data = sales_data, log(Sales)~Price)
sales_data$ExpSales <- exp(predict(exp_sales_price, sales_data))
rmse_exp <- sqrt(mean((sales_data$Sales - sales_data$ExpSales)^2, na.rm = TRUE))

ggplot(sales_data) + geom_point(aes(x=Price, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Price, y=ExpSales, color="Exp"), size = 1) +
  ggtitle("Экспоненциальная регрессия продаж по цене")

# логарифмическая
log_sales_price <- lm(data = sales_data, Sales~log(Price))
sales_data$LogSales <- predict(log_sales_price, sales_data)
rmse_log <- sqrt(mean((sales_data$Sales - sales_data$LogSales)^2, na.rm = TRUE))

ggplot(sales_data) + geom_point(aes(x=Price, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Price, y=LogSales, color="Log"), size = 1) +
  ggtitle("Логарифмическая регрессия продаж по цене")

ggplot(sales_data) + geom_point(aes(x=Price, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Price, y=LinearSales, color="Linear"), size = 1) +
  geom_line(aes(x=Price, y=ExpSales, color="Exp"), size = 1) +
  geom_line(aes(x=Price, y=LogSales, color="Log"), size = 1) +
  scale_x_continuous(breaks = round(seq(25, 75, by = 5),1)) +
  scale_y_continuous(breaks = round(seq(0, 40000, by = 2000),1)) +
  ggtitle("Все регрессии продаж по цене")

#================
# мультирегрессии
#================

# среднее продаж будем считать по фактическим продажам
sales_average <- mean(sales_data$Sales, na.rm = TRUE) 

# влияние тренда
additive_trend_sales <- lm(data = sales_data, CleanSales~Number)
sales_data$TrendEffect <- predict(additive_trend_sales, sales_data) - sales_average

# влияние цены
sales_data$PriceEffect <- sales_data$LinearSales - sales_average

# продажи (сезонность, тренд)
sales_season_trend <- lm(data = sales_data, Sales~Seasonality + TrendEffect)
sales_data$TrendForecast <- predict(sales_season_trend, sales_data)
rmse_sst <- sqrt(mean((sales_data$Sales - sales_data$TrendForecast)^2, na.rm = TRUE))

# продажи (сезонность, цена)
sales_season_price <- lm(data = sales_data, Sales~Seasonality + PriceEffect)
sales_data$PriceForecast <- predict(sales_season_price, sales_data)
rmse_ssp <- sqrt(mean((sales_data$Sales - sales_data$PriceForecast)^2, na.rm = TRUE))

# продажи (сезонность, тренд, цена)
sales_season_trend_price <- lm(data = sales_data, Sales~Seasonality + TrendEffect + PriceEffect)
sales_data$TPForecast <- predict(sales_season_trend_price, sales_data)
rmse_sstp <- sqrt(mean((sales_data$Sales - sales_data$TPForecast)^2, na.rm = TRUE))

ggplot(sales_data) + geom_line(aes(x=Number, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Number, y=TrendForecast, color="By Trend"), size = 1) +
  scale_x_continuous(breaks = round(seq(0, 275, by = 25),1)) +
  scale_y_continuous(breaks = round(seq(0, 40000, by = 2000),1)) +
  ggtitle("Прогноз по сезонности и тренду")

ggplot(sales_data) + geom_line(aes(x=Number, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Number, y=PriceForecast, color="By Price"), size = 1) +
  scale_x_continuous(breaks = round(seq(0, 275, by = 25),1)) +
  scale_y_continuous(breaks = round(seq(0, 40000, by = 2000),1)) +
  ggtitle("Прогноз по сезонности и цене")

ggplot(sales_data) + geom_line(aes(x=Number, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Number, y=TPForecast, color="By Trend and Price"), size = 1) +
  scale_x_continuous(breaks = round(seq(0, 275, by = 25),1)) +
  scale_y_continuous(breaks = round(seq(0, 40000, by = 2000),1)) +
  ggtitle("Прогноз по сезонности, тренду и цене")

ggplot(sales_data) + geom_line(aes(x=Number, y=Sales, color="Sales"), size = 1) + 
  geom_line(aes(x=Number, y=TrendForecast, color="By Trend"), size = 1) +
  geom_line(aes(x=Number, y=PriceForecast, color="By Price"), size = 1) +
  geom_line(aes(x=Number, y=TPForecast, color="By Trend and Price"), size = 1) +
  scale_x_continuous(breaks = round(seq(0, 275, by = 25),1)) +
  scale_y_continuous(breaks = round(seq(0, 40000, by = 2000),1)) +
  ggtitle("Все мультирегрессии")

#===========
# корреляции
#===========

data2012 <- filter(sales_data, floor(ISOWeek/100) == 2012)[, c("Sales", "Price")]
cor2012 <- cor(data2012$Sales, data2012$Price)
data2013 <- filter(sales_data, floor(ISOWeek/100) == 2013)[, c("Sales", "Price")]
cor2013 <- cor(data2013$Sales, data2013$Price)
data2014 <- filter(sales_data, floor(ISOWeek/100) == 2014)[, c("Sales", "Price")]
cor2014 <- cor(data2014$Sales, data2014$Price)
data2015 <- filter(sales_data, floor(ISOWeek/100) == 2015 & is.na(Sales) == FALSE)[, c("Sales", "Price")]
cor2015 <- cor(data2015$Sales, data2015$Price)

# записываем данные (csv2 - через ";", в числах "," - как нашему Excel надо)
write.csv2(sales_data, file = "newSeries.csv", fileEncoding = "UTF-8")
