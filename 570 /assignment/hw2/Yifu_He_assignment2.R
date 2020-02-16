getwd()
setwd("/Users/yifuhe/Desktop")
library("fGarch")

# Question 1

# define a function to calculate geometric sequence
geomsum <- function(a,r,n){
  x=0
  for (i in 1:n){
    x= x + a*r^(i-1)
    return(x)
  }
}

# input and process data
n <- 22
beta=2/(n+1)
data <- read.table("GE_2007-2017.csv", header = TRUE, sep=",")
with(data,{
  log.return <<- c()
  log.return[1] <<- 0
  for( i in 2:length(Adj.Close)){
    log.return[i] <<- log(Adj.Close[i]/Adj.Close[i-1])
  }
})
sigma0 <- c()
for (i in 1:(length(log.return)-(n-1))){
  sigma0[i] <- sd(log.return[i:(i+(n-1))])
}
sigma1 <- c()
sigma2 <- c()
sigma3 <- c()

# Random Walk Forecast
sigma1[1] <- 0
for (i in 1:length(sigma0)){
  sigma1[i+1] <- sigma0[i]
}

# Exponential Smoothing Average (EMA)
sigma2[1] <- 0
for(i in 1:length(sigma0)){
  sigma2[i+1] <- sigma2[i]*beta + sigma0[i]*(1-beta)
}

# Exponential Weighted Moving Average (EWMA)
sigma3[1:n] <- NA
denominator <- geomsum(beta,beta,22)
for (i in 23:length(sigma0)){
  sum <- 0
  for(j in 1:22){
    sum <- sum + (beta ^ j) * sigma0[i - j] 
  }
  sigma3[i] <- sum/denominator
}

# generate plots
par(mfrow= c(2,2))
ts.plot(sigma0,main="Original Applied Volatility")
ts.plot(sigma1,main="Random Walk Forecast")
ts.plot(sigma2,main="Exponential Moving Average")
ts.plot(sigma3,main="Exponential Weighted Moving Average")
par(mfrow= c(1,1))

# Question 2 (Generalized Autoregressive Conditional Heteroskedastic)
# use the package of fGarch
garch11 <- garchFit(~garch(1,1), data= log.return)
# get the coefficients
co <- coef(garch11)
co
# get the predictions
date <- c("1","2","3","4","5")

forecast <- c()
sigma.t <- garch11@sigma.t
for (i in 1:5){
  temp <- (co[3]+co[4])^i*((sigma.t[length(sigma.t)])^2-co[2]/(1-co[3]-co[4]))+co[2]/(1-co[3]-co[4])
  forecast[i] <- sqrt(temp)
}
outcome <- data.frame(date, forecast)
outcome

# compare the outcome 
forecast.EWMA <- 0
for (i in 1:22){
  forecast.EWMA <- forecast.EWMA + (beta^i) * sigma0[length(sigma0)-22+i]/denominator
}
forecast.EWMA


# Question 3 (use acf----Auto and Cross Covariance)
delta.price <- acf(diff(data$Adj.Close),lag.max = 1,type = "covariance")
gama0 <- delta.price$acf[1]
gama1 <- delta.price$acf[2]
fundamental.volatility <- sqrt(gama0 + 2 * gama1)
spread <- sqrt(-gama1)
roll <- data.frame(fundamental.volatility, spread)
roll
