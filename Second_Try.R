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

# Function for transforming financial data 
fin_data_transform <- function(tickerData){
  numb_returns <- dim(tickerData)[1]
  
  #Pre allocate space 
  transformed_df <- data.frame(matrix(nrow = (numb_returns-1),ncol = 3))
  
  # Place date into data frame 
  transformed_df[,1] <- tickerData[-1,1]
  
  # Over night effect on stock price 
  for(i in 1:(numb_returns-1)){ 
    transformed_df[i,2] <- (tickerData$Open[i+1]-tickerData$Close[i])/tickerData$Open[i+1]
  }
  
  # Calculate log returns 
  transformed_df[,3] <- log1p(transformed_df[,2])
  names(transformed_df)[1:3] <- c("Date","Overnight_Returns", "Log_Overnight_Returns")
  return (transformed_df)
}

# Import the financial data and calculate log returns 
UA_in_sample <- Quandl("YAHOO/UA", start_date = "2015-10-27 ", end_date = "2016-04-13")
numb_returns <- dim(UA_in_sample)[1]
UA_in_return <- fin_data_transform(UA_in_sample)

# Dates that have both stock return and game
similar_dates <- as.Date(UA_in_return$Date[UA_in_return$Date %in% data$Date],format = '%m-%d-%Y')
numb_both <- length(similar_dates)

# Create testing dataframe 
data1 <- data[data$Date %in% UA_in_return$Date ,]
data1$UA_LogReturns <- UA_in_return$Log_Overnight_Returns[UA_in_return$Date %in% data$Date]
data1$Date <- NULL

# Peak at new data frame 
head(data1)
tail(data1)

# Linear Regression and analysis 
fit1 <- lm(UA_LogReturns ~., data= data1)
summary(fit1)
plot(fit1)

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

# Try adding XLF ETF 
XLF_in_sample <- Quandl("GOOG/NYSE_XLF", start_date = "2015-10-27 ", end_date = "2016-04-13")

