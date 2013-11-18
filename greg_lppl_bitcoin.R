library('quantmod')
library('zoo')

setwd('/Users/gregorychevalley/Documents/R/bitcoin/')
rm(list=ls())

fileName <- './data/MTGOXUSD.csv'
ticker <- read.csv(fileName, header=TRUE)
ticker$Date <- as.Date(ticker$Date)

from <- as.Date("2013-01-01")
to <- as.Date("2013-04-11")

rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
plot(rTicker$Close, typ='l')
rTicker$X <- 1:(nrow(rTicker))

LPPL <- function(data, m=1, omega = 1, phi = 0) {
  data$Xm <- data$X ** m
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X) - phi)
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos, data=data))
}

FittedLPPL <- function(data, lm.result, m=1, omega=1, phi=0) {
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C <- lm.result$coefficients[3]
  result <- exp(A + B * (data$X ** m) + C * (data$X ** m) * cos(omega * log(data$X) - phi))
  return(result)
}


tryParams <- function (m, omega, phi) {  
  lm.result <- LPPL(rTicker, m, omega, phi)
  plot(rTicker$Close, typ='l')
  lines(FittedLPPL(rTicker, lm.result, m, omega, phi), col="red")
}

residuals <- function(m, omega, phi) {
  lm.result <- LPPL(rTicker, m, omega, phi)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, phi) - rTicker$Close) ** 2))
}


computeB <- function(A, B, C, m, omega, phi) {
  return(B * m - abs(C) * sqrt(m ** 2 + omega ** 2))
}


getcoeff_regLPPL <- function(m, omega, phi) {
  lm.result <- LPPL(rTicker, m, omega, phi)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3])
}





m=0.005
omega=0.0414
phi=1.59

print(getcoeff_regLPPL(m,omega,phi))



A=-42387.8
B=42294.8
C=-5096.4


print(computeB(A,B,C,m,omega,phi))