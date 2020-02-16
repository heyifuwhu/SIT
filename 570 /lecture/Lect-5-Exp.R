setwd("/Users/syang14/Dropbox/Stevens/FE570-2018F/Lecture-05/R-Code")

#Load Financial Time Series Package 'fBasics;
require(graphics)
require(zoo) 
require(ggplot2)

# Set up the basic variables
c = 1
time = 1:100
epsilon = rnorm(time)
prices = cumsum(epsilon)
m_t = zoo(prices)
a_t = m_t + c
b_t = m_t - c

df <- data.frame(time,a_t,b_t)

ggplot(df, aes(time)) +                    # basic graphical object
  geom_line(aes(y=a_t), colour="red") +  # first layer
  geom_line(aes(y=b_t), colour="green")  # second layer

#We can simulate trade prices by adding a normally distributed trade sign variable:
q_t = sign(rnorm(time))
p_t = m_t + (c * q_t)

df <- data.frame(time,a_t,b_t, p_t)

ggplot(df, aes(time)) +                    # basic graphical object
  geom_line(aes(y=a_t), colour="red", lty=2) +  # first layer
  geom_line(aes(y=b_t), colour="blue", lty=2) +  # second layer
  geom_line(aes(y=p_t), colour="black", lty=1)  # third layer

#So we just need to compute the variance and covariance of returns in order to estimate the expected cost. An example of estimating these values in R using daily data.

returns <- function(p_t) (p_t/lag(p_t))-1
rets <- na.omit(returns(m_t))
gamma_0 <- var(rets)
gamma_1 <- cov(rets[1:length(rets)-1], lag(rets, na.pad=FALSE))

sigma_2 <- gamma_0 + 2 * gamma_1
cost <- sqrt(2 * gamma_1)

