# includes ..
library('quantmod')
library('zoo')

setwd('/Users/gregorychevalley/Documents/R/bitcoin/')

# delete all we have done until now
rm(list=ls())

# load data from http://www.quandl.com/BITCOIN-Bitcoin-Charts/MTGOXUSD-Bitcoin-Markets-mtgoxUSD
fileName <- './data/MTGOXUSD.csv'

# we had an error, so we try to use the fail-safe
ticker <- read.csv(fileName, header=TRUE)

# convert string dates to R dates
ticker$Date <- as.Date(ticker$Date)

from <- as.Date("2013-01-01")
#from <- as.Date("2011-06-08")
to <- as.Date("2013-04-11")
#to <- as.Date("2011-11-21")

# restrict ticker between interest days: Jan 1 and Apr 11
rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)

# see closing graph
plot(rTicker$Close, typ='l')

#rTicker$X <- 1:(nrow(rTicker)-1)
rTicker$X <- 1:(nrow(rTicker))
# now we have the data as we wish: tc = 0, and the X column represents (tc - t)

# fit linear parameters
LPPL <- function(data, m=1, omega = 1, phi = 0) {
  data$Xm <- data$X ** m
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X) - phi)
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos, data=data))
}

lm.result <- LPPL(rTicker)
summary(lm.result)

# get the fitted values using the parameters
FittedLPPL <- function(data, lm.result, m=1, omega=1, phi=0) {
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C <- lm.result$coefficients[3]
  result <- exp(A + B * (data$X ** m) + C * (data$X ** m) * cos(omega * log(data$X) - phi))
  return(result)
}

lines(FittedLPPL(rTicker, lm.result), col="red")

# plot everything 
tryParams <- function (m, omega, phi) {  
  lm.result <- LPPL(rTicker, m, omega, phi)
  plot(rTicker$Close, typ='l')
  lines(FittedLPPL(rTicker, lm.result, m, omega, phi), col="red")
}

# the sum of squared residuals, to evaluate the fitness of m, omega, phi
residuals <- function(m, omega, phi) {
  lm.result <- LPPL(rTicker, m, omega, phi)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, phi) - rTicker$Close) ** 2))
}

# some values of the parameters
m <- seq(0.005, 0.015, 0.001)
omega <- seq(0.04, 0.05, 0.0001)
phi <- seq(1.4, 1.6, 0.01)

# try all combinations
params <- expand.grid(m, omega, phi)
app <- apply(params, 1, function (x) { residuals(x[1], x[2], x[3]) })

# let's see where is the minimum
wm <- which.min(app)
print(app[wm])
print(params[wm, 1:3])
tryParams(params[wm, 1], params[wm, 2], params[wm, 3])

computeB <- function(A, B, C, m, omega, phi) {
  return(B * m - abs(C) * sqrt(m ** 2 + omega ** 2))
}

