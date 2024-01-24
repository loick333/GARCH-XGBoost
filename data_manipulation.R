library(reshape2)
library(lubridate)
library(ggplot2)
library(quantmod)
data <- read.csv('sp500.csv')

sp500 <- data
sp500$Close <- c(NA,exp(diff(log(data$Close))) - 1)

names(sp500)[1] <- 'Date'
colnames(sp500)[2] <- 'return'
sp500 <- sp500[-1,]

#De-mean and produce Log Returns
sp500$return <- sp500$return - mean(sp500$return)
sp500$return <- log(1 + sp500[,2:dim(sp500)[2]])

#returns_sq <- simpleReturns
#returns_sq$BAC <- simpleReturns$BAC^2

Date.ts <- ymd(sp500$Date)

ggplot(sp500, aes(x = Date.ts, y = return)) +
  geom_line() +
  labs(title = "Line Chart",
       x = "X Axis Label",
       y = "Y Axis Label")

