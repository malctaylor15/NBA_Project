setwd("C:/Users/board/Desktop/Kaggle/NBA_Project")
library(moments)
library(Quandl)
library(lubridate)
library(pls)
set.seed(2)

data <- read.csv("Curry2015-2016stats_clean2.csv")

# Data Cleaning 
data$Date <- as.Date(data$Date)
data$SecondsPlayed <- hms(data$MP)
data$SecondsPlayed <- 60*hour(data$SecondsPlayed) + minute(data$SecondsPlayed)
data$MP <- NULL
# There are some missing values where he doesn't shoot free throws for his FT % 
sum(is.na(data))
# Impute mean for free throw percentage 
data$FT.[is.na(data$FT.)] = mean(data$FT., na.rm=TRUE)

# Look at the data 
head(data)
tail(data)
pairs(data)

# Declare classes for UA data 
colClasses1 <- c("Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

# Import the financial data and calculate log returns 
UA_in_sample <- read.csv("UA.csv", colClasses = colClasses1, na.strings = 'null')
summary(UA_in_sample)
head(UA_in_sample)
numb_returns <- dim(UA_in_sample)[1]

# Create return for stock price 
UA_in_sample$return <- NA
for(i in 2:numb_returns){ 
  UA_in_sample$return[i] <- (UA_in_sample$Open[i]-UA_in_sample$Close[i-1])/UA_in_sample$Open[i]
}
# Add log return 
UA_in_sample$log_return <- log1p(UA_in_sample$return)

UA_in_sample <- UA_in_sample[complete.cases(UA_in_sample), ]
dim(UA_in_sample)
# Final check 
summary(UA_in_sample)
head(UA_in_sample)
tail(UA_in_sample)

# Let's examine the log returns 
hist(UA_in_sample$log_return, breaks = 30)
plot(UA_in_sample$Date, UA_in_sample$log_return)
UA_gains <- cumsum(UA_in_sample$log_return)
plot(UA_in_sample$Date, UA_gains, type = "l", lwd = 3)

# Extreme values 
extreme_high <- which(UA_in_sample$log_return == max(UA_in_sample$log_return))
extreme_low <- which(UA_in_sample$log_return == min(UA_in_sample$log_return))

UA_in_sample[extreme_high, ]
UA_in_sample[extreme_low, ]

# Dates that have both stock return and game
similar_dates <- as.Date(UA_in_sample$Date[UA_in_sample$Date %in% data$Date],format = '%m-%d-%Y')
(numb_dates_in_both <- length(similar_dates))

# Create testing dataframe 
data1 <- data[data$Date %in% UA_in_sample$Date ,]
data1$UA_LogReturns <- UA_in_sample$log_return[UA_in_sample$Date %in% data$Date]
data1$Date <- NULL

# Peak at new data frame 
head(data1)
tail(data1)

# Linear Regression and analysis 
fit1 <- lm(UA_LogReturns ~., data= data1)
summary(fit1)
plot(fit1)

# Prediction 
linear_predictions <- predict(object = fit1, data1)
quantile(linear_predictions)



# PCA 
prince <- prcomp(data1, center = TRUE, scale = TRUE)
summary(prince)
plot(prince)

# Principal Components Regression 
# ISLR 256 

pcr.fit <- pcr(UA_LogReturns ~., data =data1,scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
# Smallest error at 11 components 


