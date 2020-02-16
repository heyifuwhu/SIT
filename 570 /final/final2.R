w## install the package of MACD
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("xts")
install.packages("zoo")
install.packages("TTR")
require("TTR")
require("xts")
require("zoo")
require("quantmod")
require("PerformanceAnalytics")



## read the file
getwd()
setwd("/Users/yifuhe/Desktop")
File <-read.csv("XOM-1.csv")

###----------------------------------------- Question1

XOMclose <-unlist(File[6])

##get the plot of MACD
macd <-MACD(XOMclose,nFast=12,nSlow=26,nSig=9,maType="EMA",percent=TRUE)
macd


ts.plot(macd[,1],main="Ticker:XOM Daily Closing:AUG 21,2009 to AUG 20,2010.")
lines(macd[,2],col="red",lty=2)
legend("topright",c("MACD","signal_line"),col=c(1,2),lty=c(1,2),cex=0.7)


cal <- c(rep(0,33))
for (i in 34 : nrow(macd))
{
  if (macd[i,1] > macd[i,2]){cal[i] <- 1} 
  else {cal[i] <- -1}
}
cal
signalXOM <- rep(0,250) 
for(i in 34 : nrow(macd)) {
  if ((cal[i] - cal[i - 1]) == 2) {signalXOM[i] <- 'buy'}
  else if((cal[i] - cal[i - 1]) == -2) {signalXOM[i] <- 'sell'}
}
XOM <-cbind(File,signalXOM)
Finalsignal <- subset(XOM, signalXOM != 0) 
Finalsignal
nrow(Finalsignal)
##
profit <-rep(0,15)
acmprofit<-rep(0,15)
logr <-rep(0,15)
shares <-rep(0,15)
money <-rep(0,15)
shares[1]=10000%/%Finalsignal[1,5]
money[1]=10000-(shares[1]*Finalsignal[1,5])
shares
money
for (i in 1:15){
  if((i %% 2) ==1 & (i>2)){
    shares[i]=(money[i-1]) %/% (Finalsignal[i,5])
    money[i]=money[i-1]-Finalsignal[i,5]*shares[i]
  }
  else if((i %% 2)==0){
    shares[i]=0
    money[i]=money[i-1]+shares[i-1]*Finalsignal[i,5]
    logr[i]=log(money[i])-log(money[i-1])
    profit[i]=money[i]-money[i-1]
    total=0
    for(j in 1:i){
      total=total+profit[j]
      
    }
    acmprofit[i]=total
  }
}
Finalsignal <-cbind(Finalsignal,profit)
Finalsignal<-cbind(Finalsignal,logr)
Finalsignal<-cbind(Finalsignal, acmprofit)
Finalsignal <-cbind(Finalsignal,money)
Finalsignal <-cbind(Finalsignal,shares)
Finalsignal
###----------------------------------------Question 2
calcu <- subset(Finalsignal, logr != 0) 
calcu
ri<- mean(calcu[,10])
std<-sd(calcu[,10])
sharp <- (ri-0.0511)/std
sharp


###_______________-question3
macd <-MACD(XOMclose,nFast=12,nSlow=26,nSig=7,maType="EMA",percent=TRUE)
macd


ts.plot(macd[,1],main="Ticker:XOM Daily Closing:AUG 21,2009 to AUG 20,2010.")
lines(macd[,2],col="red",lty=2)
legend("topright",c("MACD","signal_line"),col=c(1,2),lty=c(1,2),cex=0.7)


cal <- c(rep(0,31))
for (i in 32 : nrow(macd))
{
  if (macd[i,1] > macd[i,2]){cal[i] <- 1} 
  else {cal[i] <- -1}
}
cal
signalXOM <- rep(0,250) 
for(i in 32 : nrow(macd)) {
  if ((cal[i] - cal[i - 1]) == 2) {signalXOM[i] <- 'buy'}
  else if((cal[i] - cal[i - 1]) == -2) {signalXOM[i] <- 'sell'}
}
XOM <-cbind(File,signalXOM)
Finalsignal <- subset(XOM, signalXOM != 0) 
Finalsignal
nrow(Finalsignal)
##
profit <-rep(0,15)
acmprofit<-rep(0,15)
logr <-rep(0,15)
shares <-rep(0,15)
money <-rep(0,15)
shares[1]=10000%/%Finalsignal[1,5]
money[1]=10000-(shares[1]*Finalsignal[1,5])
shares
money
for (i in 1:15){
  if((i %% 2) ==1 & (i>2)){
    shares[i]=(money[i-1]) %/% (Finalsignal[i,5])
    money[i]=money[i-1]-Finalsignal[i,5]*shares[i]
  }
  else if((i %% 2)==0){
    shares[i]=0
    money[i]=money[i-1]+shares[i-1]*Finalsignal[i,5]
    logr[i]=log(money[i])-log(money[i-1])
    profit[i]=money[i]-money[i-1]
    total=0
    for(j in 1:i){
      total=total+profit[j]
      
    }
    acmprofit[i]=total
  }
}
Finalsignal <-cbind(Finalsignal,profit)
Finalsignal<-cbind(Finalsignal,logr)
Finalsignal<-cbind(Finalsignal, acmprofit)
Finalsignal <-cbind(Finalsignal,money)
Finalsignal <-cbind(Finalsignal,shares)
Finalsignal
###----------------------------------------Question 2
calcu <- subset(Finalsignal, logr != 0) 
calcu
ri<- mean(calcu[,10])
std<-sd(calcu[,10])
sharp <- (ri-0.0511)/std
sharp

###_________________________
macd <-MACD(XOMclose,nFast=12,nSlow=26,nSig=11,maType="EMA",percent=TRUE)
macd


ts.plot(macd[,1],main="Ticker:XOM Daily Closing:AUG 21,2009 to AUG 20,2010.")
lines(macd[,2],col="red",lty=2)
legend("topright",c("MACD","signal_line"),col=c(1,2),lty=c(1,2),cex=0.7)


cal <- c(rep(0,35))
for (i in 36 : nrow(macd))
{
  if (macd[i,1] > macd[i,2]){cal[i] <- 1} 
  else {cal[i] <- -1}
}
cal
signalXOM <- rep(0,250) 
for(i in 36 : nrow(macd)) {
  if ((cal[i] - cal[i - 1]) == 2) {signalXOM[i] <- 'buy'}
  else if((cal[i] - cal[i - 1]) == -2) {signalXOM[i] <- 'sell'}
}
XOM <-cbind(File,signalXOM)
Finalsignal <- subset(XOM, signalXOM != 0) 
Finalsignal
nrow(Finalsignal)
##
profit <-rep(0,15)
acmprofit<-rep(0,15)
logr <-rep(0,15)
shares <-rep(0,15)
money <-rep(0,15)
shares[1]=10000%/%Finalsignal[1,5]
money[1]=10000-(shares[1]*Finalsignal[1,5])
shares
money
for (i in 1:15){
  if((i %% 2) ==1 & (i>2)){
    shares[i]=(money[i-1]) %/% (Finalsignal[i,5])
    money[i]=money[i-1]-Finalsignal[i,5]*shares[i]
  }
  else if((i %% 2)==0){
    shares[i]=0
    money[i]=money[i-1]+shares[i-1]*Finalsignal[i,5]
    logr[i]=log(money[i])-log(money[i-1])
    profit[i]=money[i]-money[i-1]
    total=0
    for(j in 1:i){
      total=total+profit[j]
      
    }
    acmprofit[i]=total
  }
}
Finalsignal <-cbind(Finalsignal,profit)
Finalsignal<-cbind(Finalsignal,logr)
Finalsignal<-cbind(Finalsignal, acmprofit)
Finalsignal <-cbind(Finalsignal,money)
Finalsignal <-cbind(Finalsignal,shares)
Finalsignal
###----------------------------------------Question 2
calcu <- subset(Finalsignal, logr != 0) 
calcu
ri<- mean(calcu[,10])
std<-sd(calcu[,10])
sharp <- (ri-0.0511)/std
sharp
