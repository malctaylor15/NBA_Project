setwd("C:/Users/board/Desktop/Kaggle/NBA_Project")
library(moments)

data <- read.csv("Curry2015-2016stats_clean.csv")
head(data)

df<- data.frame(as.Date(data$Date,format = "%m/%d/%Y"), data$W.L,data$MP,data$GmSc)
names(df)[1:4] <-c("Date","WL","MP","GmSc")
numb_games <- dim(df)[1]
GmSc <- data$GmSc

makeHist <- function(x, color = "blue", title = "Histogram"){
  h<-hist(x,main=title) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col=color, lwd=2)
}

summary(GmSc)
sd(GmSc)
makeHist(GmSc)
qqnorm(GmSc)
qqline(GmSc)
jarque.test(GmSc)

#GmSc might be normally distributed 

library(Quandl)
# After the playoffs to beginning of next season
# UA_out_sample <- Quandl("YAHOO/UA", start_date = "2015-06-17 ", end_date = "2015-10-26")
UA_in_sample <- Quandl("YAHOO/UA", start_date = "2015-10-27 ", end_date = "2016-04-13")
numb_returns <- dim(UA_in_sample)[1]

# Get S&P500 Data 
SP_Benchmark <- Quandl("YAHOO/INDEX_GSPC", start_date = "2015-10-27 ", end_date = "2016-04-13")

# Over night effect on stock price 
UA_in_return <- data.frame(matrix(nrow=(numb_returns-1),ncol=3))
for(i in 1:(numb_returns-1)){ 
  UA_in_return[i,1] <- (UA_in_sample$Open[i+1]-UA_in_sample$Close[i])/UA_in_sample$Open[i+1]
}

# S&P 500 Transformation 
SP_edit <- data.frame(matrix(nrow=(numb_returns-1),ncol=2))
for(i in 1:(numb_returns-1)){ 
  SP_edit[i,1] <- (SP_Benchmark$Open[i+1]-SP_Benchmark$Close[i])/SP_Benchmark$Open[i+1]
}


# Calculate stock log returns 
for(i in 1:(numb_returns-1)){ 
  UA_in_return[i,2] <- log(1+UA_in_return[i,1])
}

# Add the Date to UA in return 
UA_in_return[,3] <- UA_in_sample[-1,1]

# Rename the columns 
names(UA_in_return)[1:3] <- c("Return","log Return", "Date")


# Dates that have both stock return and game
similar_dates <- as.Date(UA_in_return$Date[df$Date %in% UA_in_return$Date ],format = '%m/%d/%Y')
numb_both <- length(similar_dates)


df_final <- data.frame(matrix(nrow=numb_both, ncol=6))
df_final[,1] <- similar_dates

for(i in 1:numb_both){ 
df_final[i,2:3] <- UA_in_return[which(UA_in_return$Date == similar_dates[i]),1:2]
}
# Need to preserve the order and relationships of the returns and dates - 



UA_index <- rep(0,length(similar_dates))
for (i in 1:length(similar_dates)){
 UA_index[i] <- which(UA_in_return$Date == similar_dates[i])
}

df_final[,2:3] <- UA_in_return[UA_index,1:2]
df_final[,4:6] <- df[UA_index,2:4]
df_final[,7] <- SP_edit[UA_index,1]

head(df_final)
mean(df_final[,6])

names(df_final)[1:7] <- c("Dates", "UA_Returns","UA_log_Returns","WL", "MP","GmSc","SP")

fit0 <- lm(UA_log_Returns ~GmSc, data = df_final)
summary(fit0)

fit1 <- lm(UA_Returns ~ SP+ GmSc, data= df_final)
summary(fit1)
pairs(df_final)

# Does not look like there is much 


XLF_in_sample <- Quandl("GOOG/NYSE_XLF", start_date = "2015-10-27 ", end_date = "2016-04-13")
head(XLF_in_sample)



# Ticker Data should be in the format of Date, Open, High, Low, Close 
# This is the format for the results of a Quandl Query 
# Return data frame with Date, Overnight effect, Log over night effect 
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


XLF_Returns <- fin_data_transform(XLF_in_sample)
names(XLF_Returns)[2:3] <- c("XLF_Overnight_Returns","XLF_Log_Overnight_Returns")

df_final[,c(8,9)] <- XLF_Returns[UA_index,c(2,3)]

fit2 <- lm(UA_Returns ~ Overnight_Returns + GmSc, data = df_final)
summary(fit2)

############## New Ideas ##############

# Bad practice removing data points 
df_new <- data.frame(matrix(nrow = (dim(df_final)[1])-2,ncol = 3))
df_new[,1] <- scale(df_final[c(-1,-22),2])
df_new[,2] <- scale(df_final[c(-1,-22),6])
df_new[,3] <- scale(df_final[c(-1,-22),7])
names(df_new)[1:3] <- c("UA_Returns","GmSc","SPY_Returns")



scale_fit <- lm(UA_Returns~SPY_Returns,data = df_new)
summary(scale_fit)
# plot(scale_fit)

scale_fit2 <- lm(UA_Returns~SPY_Returns + GmSc, data = df_new)
summary(scale_fit2)


plot(df_new$SPY_Returns,df_new$UA_Returns)
abline(scale_fit)
abline(h=0, col="blue")
  
  
  