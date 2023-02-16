library(simhelpers)
library(dplyr)
library(tibble)
library(knitr)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(stargazer)
library(data.table)

#set.seed(13436962)
set.seed(13449499)
#monte carlo simulation
n <- c(500) #sample size
m <- 100 #number of simulation iterations
#part i) simulate predictors X

betaMatrix <- matrix(NA,100,3)
listMSE <- c()
for(i in 1:5)
{
  x1 <- rnorm(n=n[i],mean = 2, sd =0.4)
  x2 <- rnorm(n=n[i],mean = 1,sd = 0.1)
  for(j in 1:m){
    #simulate the error term m=100 times...
    error <- rnorm(n = n[i], mean =0,sd = 1)
    y <-  5 + 1.2*x1 + 3*x2 + error
    lm.out <- lm(y~x1+x2)
    betaMatrix[j,] <- lm.out$coefficients
    mysummary <- summary(lm.out)
    listMSE[j] <- mysummary$sigma^2 #get MSE per iteration
  }
  beta_mean <- apply(betaMatrix,2,mean)
  estimation_bias <- beta_mean - c(5,1.2,3)
  beta_var <- apply(betaMatrix,2,var)
  beta_mse <- estimation_bias^2 + beta_var
  cat("Sample size = ",n[i],"\n")
  cat("beta_mean = ",beta_mean,"\nestimation_bias = ",estimation_bias,"\nbeta_var = ",beta_var,"\nbeta_mse = ",beta_mse,"\nModel MSE = ",mean(listMSE))
  cat("\n\n")
}
## Sample size =  200 
## beta_mean =  4.966803 1.215706 3.012984 
## estimation_bias =  -0.03319671 0.0157064 0.01298392 
## beta_var =  0.6107492 0.03867446 0.5604804 
## beta_mse =  0.6118512 0.03892115 0.560649 
## Model MSE =  1.006853
## 
## Sample size =  1000 
## beta_mean =  4.991543 1.187003 3.036881 
## estimation_bias =  -0.00845691 -0.0129968 0.03688108 
## beta_var =  0.1525668 0.006953769 0.1139906 
## beta_mse =  0.1526383 0.007122686 0.1153508 
## Model MSE =  1.003811
## 
## Sample size =  2000 
## beta_mean =  5.002824 1.203964 2.99046 
## estimation_bias =  0.00282426 0.003963609 -0.009540441 
## beta_var =  0.05191324 0.002815025 0.04446706 
## beta_mse =  0.05192122 0.002830735 0.04455808 
## Model MSE =  1.006716
## 
## Sample size =  5000 
## beta_mean =  4.998425 1.200324 3.0004 
## estimation_bias =  -0.001574968 0.0003243634 0.0003997949 
## beta_var =  0.02850822 0.001264033 0.02254752 
## beta_mse =  0.0285107 0.001264138 0.02254768 
## Model MSE =  0.999705
## 
## Sample size =  10000 
## beta_mean =  5.013478 1.19792 2.988708 
## estimation_bias =  0.01347842 -0.002080048 -0.01129225 
## beta_var =  0.01240872 0.0005731851 0.009740077 
## beta_mse =  0.01259039 0.0005775117 0.009867591 
## Model MSE =  0.9996117