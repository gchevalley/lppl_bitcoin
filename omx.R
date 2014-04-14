library('quantmod')
library('zoo')

setwd('/Users/gregorychevalley/Documents/R/lppl_bitcoin/')
rm(list=ls())

fileName <- './data/omx.csv'
ticker <- read.csv(fileName, header=TRUE, sep=";")


ticker$Date <- as.Date(ticker$Date)


from <- as.Date("1996-02-01")
to <- as.Date("1998-07-20")
to <- as.Date("1998-04-15")


rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
plot(rTicker$t, rTicker$price, typ='l')

LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$price)
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
  plot(rTicker$t, rTicker$price, typ='l') #base graph based on data
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc), col="red")
}

residuals <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, tc) - rTicker$price) ** 2))
}



getcoeff_regLPPL <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}


#tryParams(0.3424, 5.518, 1998.686) # soluce paper
m <- seq(0.1, 0.5, 0.05)
omega <- seq(6, 13, 0.5)
tc <- seq(max(rTicker$t)+0.002, max(rTicker$t)+0.25, 0.0135) # a 3 mois


# try all combinations
params <- expand.grid(m, omega, tc)

app <- apply(params, 1, function (x) { residuals(x[1], x[2], x[3]) })

# let's see where is the minimum
wm <- which.min(app)
print(app[wm])
print(params[wm, 1:3])

m <- params[wm, 1]
omega <- params[wm, 2]
tc <- params[wm, 3]

tryParams(params[wm, 1], params[wm, 2], params[wm, 3]) #last call for chart



estimatorsABC1C2 <- getcoeff_regLPPL(m, omega, tc)

A <- estimatorsABC1C2[1]
B <- estimatorsABC1C2[2]
C1 <- estimatorsABC1C2[3]
C2 <- estimatorsABC1C2[4]
phi <- atan(1/(C1/C2))
C <- C1 / cos(phi)

