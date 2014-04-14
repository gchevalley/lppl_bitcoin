library('quantmod')
library('zoo')

setwd('/Users/gregorychevalley/Documents/R/lppl_bitcoin/')
rm(list=ls())

fileName <- './data/MTGOXUSD.csv'
ticker <- read.csv(fileName, header=TRUE)
ticker$Date <- as.Date(ticker$Date)

from <- as.Date("2011-03-01")
to <- as.Date("2011-06-07")

rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
plot(rTicker$Close, typ='l')
rTicker$X <- 1:(nrow(rTicker))

LPPL <- function(data, m=1, omega = 1) {
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

FittedLPPL <- function(data, lm.result, m=1, omega=1) {
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}


tryParams <- function (m, omega) {  
  lm.result <- LPPL(rTicker, m, omega)
  plot(rTicker$Close, typ='l')
  lines(FittedLPPL(rTicker, lm.result, m, omega), col="red")
}

residuals <- function(m, omega) {
  lm.result <- LPPL(rTicker, m, omega)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega) - rTicker$Close) ** 2))
}


computeB <- function(A, B, C, m, omega) {
  return(B * m - abs(C) * sqrt(m ** 2 + omega ** 2))
}


getcoeff_regLPPL <- function(m, omega) {
  lm.result <- LPPL(rTicker, m, omega)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}


# some values of the parameters
#m <- seq(0.005, 0.015, 0.001)
#omega <- seq(0.04, 0.05, 0.0001)


m <- seq(0.1, 0.9, 0.05)
omega <- seq(6, 13, 0.05)


# try all combinations
params <- expand.grid(m, omega)
app <- apply(params, 1, function (x) { residuals(x[1], x[2]) })

# let's see where is the minimum
wm <- which.min(app)
print(app[wm])
print(params[wm, 1:2])

m <- params[wm, 1]
omega <- params[wm, 2]

tryParams(params[wm, 1], params[wm, 2]) #last call for chart


estimatorsABC1C2 <- getcoeff_regLPPL(m, omega)


A <- estimatorsABC1C2[1]
B <- estimatorsABC1C2[2]
C1 <- estimatorsABC1C2[3]
C2 <- estimatorsABC1C2[4]
phi <- atan(1/(C1/C2))
C <- C1 / cos(phi)


test <- (-10:max(rTicker$X))
#expected <- exp(A + B * ((-10:max(rTicker$X)) ** m) + C1 * ((-10:max(rTicker$X)) ** m) * cos(omega * log((-10:max(rTicker$X)))) + C2 * ((-10:max(rTicker$X)) ** m) * sin(omega * log((-10:max(rTicker$X))))) 

#compute bubble
print(computeB(A, B, C, m, omega))