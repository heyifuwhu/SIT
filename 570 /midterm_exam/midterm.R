getwd()
setwd("/Users/yifuhe/Desktop")
mydata <- read.csv("NFLX-2013_2018.csv")

###Q1
n <- 20
beta <- 2/(n+1)
price <- mydata$Adj.Close

## create sigma_ori 
sigma_ori <- rep(0,n) 

for (i in n:length(price)){
  r_mean <- mean(price[(i-n+1):i]) 
  sum <-0
  for (j in 1:n){
    sum=sum + (price[i-n+j] - r_mean)^2
  }
  sigma_ori[i]<-sqrt(sum/n)
}

## create sigma 
sigma_expect <- rep(0,n)

down_sub <- 0 
for (i in 1:n){
  down_sub <- down_sub + beta^i 
  }

for (i in (n+1):length(sigma_ori)){ 
  total <- 0
  for (j in seq(n)){
  total <- total+ (beta^j)*sigma_ori[i-j]/down_sub 
  }
  sigma_expect[i] <- total 
  }
## create the plot of sigma_expect 

ts.plot(sigma_expect)


##Question3 
gamma<-acf(diff(price),type="covariance",lag.max=1,plot=FALSE) gamma0<-gamma$acf[1]
gamma1<-gamma$acf[2]
sigma_u<-sqrt(gamma0+2*gamma1)
sigma_u
##question3 sigma_roll <- c(NA)
for (i in n:(length(price))){ gamma<-acf(diff(price[(i-n+1):(i)]),type="covariance",lag.max=1,plot=FALSE) gamma0<-gamma$acf[1]
gamma1<-gamma$acf[2]
sigma_u<-sqrt(gamma0+2*gamma1)

sigma_roll[i] <-sigma_u }
par
ts.plot(sigma_roll)