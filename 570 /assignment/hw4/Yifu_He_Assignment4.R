library(tseries)
#Problem 1 xom<-read.csv((file.choose())) head(xom) cvx<-read.csv((file.choose())) head(cvx)
xom_close<-xom$Close
cvx_close<-cvx$Close
n1<-length(xom_close)
n2<-length(cvx_close) xt<-log(xom_close[-1]/xom_close[-n1]) yt<-log(cvx_close[-1]/cvx_close[-n2]) plot(xt,yt,main="scatter plot for xt and yt") fit<-lm(yt~xt)
summary(fit)
coef<-fit$coefficients
coef
alpha<-as.vector(coef)[2]
alpha
const<-as.vector(coef)[1]
const
abline(fit,col="red")
legend("topleft", lty=c(1), col=c("red"),
       legend=c("regression line")) zt<-fit$residuals
plot(fit,which=1)
#the non-linear trend is not obvious adf.test(zt)
3
#p-value less than 0.01,rejected the null hypothesis. Under alpha=0.01, it's statio
delta<-2*sd(zt) date<-as.Date(xom$Date[-1])
yt_axt<-yt-alpha*xt
table<-as.data.frame(cbind(xt,yt,yt_axt))
table$Date=date
table<-table[,c('Date','xt','yt','yt_axt')]
table$signal=c(rep(0,nrow(table))) table[abs((table$yt_axt+delta-const))<=0.001,]$signal<-1 table[abs((table$yt_axt-delta-const))<=0.001,]$signal<--1
order<-c(1:nrow(table))
table$order=order
table$act=c(rep("act",nrow(table)))
table[table$signal==1,]$act<-"long"
table[table$signal==-1,]$act<-"short" trade<-subset(table[,c(-2,-3,-4,-5)],table$act!="act")
trade
#calculate the pnl at the accuracy of 0.001
cal_xp<-xom_close[(trade$order+1)]
cal_cp<-cvx_close[(trade$order+1)]
#Assuming that we long 1 porfolio is that we short 1 shares B and long ratio shars #we short 1 porfolio is that we buy 1 shares B and short ratio shares A
#calculate the pnl under the accuracy of 0.01
transaction
sig<-trade$act
sig
sig[sig=="short"]=1
sig[sig=="long"]=-1
sig<-as.numeric(sig)
sig_A<-sig*alpha
sig_A[length(sig_A)]=-2*alpha
sig_A
sig_B<-sig*(-1)
sig_B[length(sig_B)]=2
sig_B
pnl_A<-sig_A%*%cal_xp
pnl_B<-sig_B%*%cal_cp
pnl_A
pnl_B
cal_xp
pnl<-pnl_A+pnl_B
pnl


