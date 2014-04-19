library('cmaes')
library('quantmod')
library('zoo')

#setwd('/Users/gregorychevalley/Documents/R/lppl_bitcoin/')
setwd('C:/Users/Gregory Chevalley/RStudio/lppl_bitcoin/') # notebook


rm(list=ls())

fileName <- './data/DJA.csv'
ticker <- read.csv(fileName, header=TRUE, sep=",")


ticker$Date <- as.Date(ticker$Date)


from <- as.Date("1927-06-30")
to <- as.Date("1929-08-24")


#from <- as.Date("1921-06-30")
#to <- as.Date("1929-09-01")


rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
plot(rTicker$t, rTicker$Close, typ='l')

LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

FittedLPPL <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}



FittedLPPLwithexpected <- function(data, lm.result, x_vector, m=1, omega=1, tc=0) {
  tmp_vector <- tc - x_vector
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(omega * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(omega * log(tmp_vector))) 
  return(result)
  
}


tryParams <- function (m, omega, tc) {  
  lm.result <- LPPL(rTicker, m, omega, tc)
  plot(rTicker$t, rTicker$Close, typ='l') #base graph based on data
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc), col="red")
}

residuals <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, tc) - rTicker$Close) ** 2))
}


residual_obj <- function(x) {
  return(residuals(x[1], x[2], x[3]))
}



getcoeff_regLPPL <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}


vec_control <- data.frame(maxit = c(100))   
test <- cma_es(c(0.01, 5, max(rTicker$t)+0.002), residual_obj, lower=c(0.01, 5, max(rTicker$t)+0.002), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)

test$par

test$par[3]

tryParams(test$par[1], test$par[2], test$par[3])