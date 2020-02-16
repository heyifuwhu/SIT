## install the package of MACD
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("xts")
install.packages("zoo")
require("xts")
require("zoo")
require("quantmod")
require("PerformanceAnalytics")



## read the file
getwd()
setwd("/Users/yifuhe/Desktop")
File <- read.table("sp500hst.txt",sep=",",header=TRUE)
ticker <- unlist(File[2])
## output <- matrix(File,ncol=7,byrow=TRUE)

###----------------------------------------- Question1

##find the data of BAC
beginIndex <- match("BAC",ticker)
endIndex<-NA
for(i in beginIndex:length(ticker) ){
  if(ticker[i]!="BAC"){
    endIndex <- i-1
    break
  }
}

## get the close price of BAC
close <-unlist(File[6])
BACclose <-close[beginIndex:endIndex]

##get the plot of MACD
macd <-MACD(BACclose,nFast=11,nSlow=22,nSig=9,maType="EMA",percent=TRUE)
macd
ts.plot(macd[,1],main="Ticker:BAC Daily Closing:AUG 21,2009 to AUG 20,2010.")
lines(macd[,2],col="red",lty=2)
legend("topright",c("MACD","signal_line"),col=c(1,2),lty=c(1,2),cex=0.7)

## get the plot of RSI
rsi <- RSI(BACclose,n=14)
ts.plot(rsi,ylim=c(0,100),main="Ticker:BAC Daily Closing:AUG 21,2009 to AUG 20,2010.")
abline(h=20,col=3,lty=2)
abline(h=80,col=2,lty=3)
legend("topright",c("RSI","top_line = 80","bottom_line = 20"),col=c(1,3,2),lty=c(1,2,3),cex=0.7)



###----------------------------------------Question 2

##find the data of AA
AAbeginIndex <- match("AA",ticker)
AAendIndex<-NA
for(i in AAbeginIndex:length(ticker) ){
  if(ticker[i]!="AA"){
    AAendIndex <- i-1
    break
  }
}

## get the close price of AA
AAclose <-close[AAbeginIndex:AAendIndex]
ts.plot(AAclose,ylab="AAclose",main="Ticker:AA Daily Closing:AUG 21,2009 to AUG 20,2010.")

## smoothing 
point<-matrix(0,nrow=245,ncol=2)
point[,1]<-1:245
point[,2]<-EMA(AAclose,n=14)
ts.plot(point[,2],ylim=c(10,17),ylab="AAclose_EWA",main="Ticker:AA Daily Closing:AUG 21,2009 to AUG 20,2010.")

E3<-point[last(order(AAclose)),2]

find_E1<-point[order(point[1:50,2],decreasing = T),]
E1<-find_E1[1,2]

range_E2<-point[50:90,]
find_E2<-range_E2[order(range_E2[,2]),]
E2<-find_E2[1,2]

range_E4<-point[100:150,]
find_E4<-range_E4[order(range_E4[,2]),]
E4<-find_E4[1,2]

range_E5<-point[140:170,]
find_E5<-range_E5[order(range_E5[,2],decreasing = T),]
E5<-find_E5[1,2]

points(last(order(AAclose)),E3,col=2)
text(last(order(AAclose)),E3,pos=1,"E3")
points(find_E1[1,1],E1,col=2)
text(find_E1[1,1],E1,pos=3,"E1")
points(find_E2[1,1],E2,col=2)
text(find_E2[1,1],E2,pos=3,"E2")
points(find_E4[1,1],E4,col=2)
text(find_E4[1,1],E4,pos=3,"E4")
points(find_E5[1,1],E5,col=2)
text(find_E5[1,1],E5,pos=3,"E5")

slope<-(E4-E2)/(find_E4[1,1]-find_E2[1,1])
x<-c(1:245)
y<-E2+slope*(x-find_E2[1,1])
slope <- (12.972 - 13.176) / (61 - 104)
intercept <- 12.972 - 61 * slope 
points(x,y,type="l",col=5)

x < c(1 : 245)
line <- slope * x + intercept
dis <- 16.538 - line[83]
dis
which(AAmvt[,2] < line)
priceobjective <- line[167] - dis
priceobjective
