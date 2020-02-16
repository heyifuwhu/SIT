# set the path of file & library the package
getwd()
setwd("/Users/yifuhe/Desktop")
library(fBasics) # use to calculate the summary statistics
library(ggplot2) # to draw plots

# input data
data <- read.table("Lect-1-TradingTS.csv", sep = ",", header = TRUE)
time <- as.vector(as.matrix(data[1])) # also can use time <- unlist(data[1])
price <- unlist(data[2])

# process the data
min.lnr <- c()
second.lnr <- c()
len <- length(price)

# calculate the log return of min and second
second.lnr[1] <- 0 
for (i in 2:(len/5)){
  second.lnr[i] <- log(mean(price[(5*i-4):(5*i)]))-log(mean(price[(5*(i-1)-4):(5*(i-1))]))
}

min.lnr[1] <- 0
for(i in 2:floor(len/300)){
  min.lnr[i] <-log(mean(price[(300*i-299):(300*i)]))-log(mean(price[(300*(i-1)-299):(300*(i-1))]))
}

# show the summary statistics
Data <- c("second", "minute")
Mean <- c(mean(second.lnr), mean(min.lnr))
Median <- c(median(second.lnr), median(min.lnr))
Min <- c(min(second.lnr), min(min.lnr))
Max <- c(max(second.lnr), max(min.lnr))
StandardDivation <- c(sd(second.lnr), sd(min.lnr))
Skewness <- c(skewness(second.lnr), skewness(min.lnr))
Kurtosis <- c(kurtosis(second.lnr), kurtosis(min.lnr))
outcome <- data.frame(Data, Mean, Median, Min, Max, StandardDivation, Skewness, Kurtosis)
outcome

# draw the plot
plot(second.lnr,xlab="time",ylab="log_return",main="scatter plot of log_return(second)",pch=1,cex=0.5)
hist(second.lnr,main="Log return of time(seconds)",xlab="log return",ylab="density")
plot(density(second.lnr))

plot(min.lnr,xlab="time",ylab="log_return",main="scatter plot of log_return(minute)")
hist(min.lnr,main="Log return of time(minute)",xlab="log return",ylab="density")
plot(density(min.lnr))
