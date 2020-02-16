getwd()
setwd("/Users/yifuhe/Desktop")
library("quantmod")

# Question 1
# 1.1MACD
data <- read.table("sp500hst.txt",header = TRUE, sep = ",")
BAC <- subset(data, Ticker == "BAC")
# MACD 
# (MACD = ema(p, fast) - ema(p, slow))
# singal_line = ema(MACD, sig)
MACD <- MACD(BAC$Close,nFast = 11, nSlow = 22, nSig = 9, maType=EMA)
MACD
MACD <- na.omit(MACD)
plot(MACD[,1], type = "l", col = "1", ylab = "MACD", main = "Ticker:BAC Daily Closing MACD")
lines(MACD[,2],type =  "l", col = "2")
legend(165, 3, c("MACD", "Singal_line"), lty = c(1,1), col = c(1, 2), bty = "n")

# find the signal
n <- length(BAC$Close) - length(MACD[,1])
temp <- rep(0, n)
for (i in 1:length(MACD[,1])){
  # below
  if(MACD[i,1] - MACD[i,2] < 0){
    temp[i+n] <- -1
  }
  # above
  if(MACD[i,1] - MACD[i,2] > 0){
    temp[i+n] <- 1
  }
}
signal <- rep(0,length(BAC$Close))
for (i in 2:length(signal)){
  # below to above
  if(temp[i]-temp[i-1] == 2){
    signal[i] <- "buy"
  }
  # above to below
  if(temp[i]-temp[i-1] == -2){
    signal[i] <- "sell"
  }
}
BACoutcome <- cbind(BAC,signal)
BACoutcome <- subset(BACoutcome,signal != 0)
BACoutcome

# 1.2 RSI relative strength index
RSI <- RSI(BAC$Close,n=14)
plot(RSI, type = "l", col = "1", ylab = "RSI",ylim = c(0,100), main = "Ticker: BAC Close Price RSI")
abline(a = 20, b = 0, col = "red")
abline(h = 80, col = "red")

# Question 2

AA <- subset(data, Ticker == "AA")
plot(AA$Close, type = "l",  ylab = "Close Price", main = "Ticker: AA Close Price")
