library(simhelpers)
library(dplyr)
library(tibble)
library(knitr)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(stargazer)
library(data.table)

# a-c no forloop
  X1 <- runif(n = 500, min = 0, max = 2)
  X1
  
  X2 <- runif(n = 500, min = 0, max = 2)
  X2
  
  X3 <- runif(n = 500, min = 0, max = 2)
  
  hist(X1, xlab = "Random value (X)", col = "grey",
       main = "", cex.lab = 1.5, cex.axis = 1.5)
  
  
  hist(X2, xlab = "Random value (X)", col = "grey",
       main = "", cex.lab = 1.5, cex.axis = 1.5)
  
  hist(X3, xlab = "Random value (X)", col = "grey",
       main = "", cex.lab = 1.5, cex.axis = 1.5)
  
  epsilon <- runif(n = 500, min = 0, max = 1)
  epsilon
  
  
  
  # Dependent variable Y
  
  Y1 = X1^3-3.5*X1^2+3*X1+epsilon
  Y1
  
  df <- data.frame(Y1,X1,X2,X3,epsilon)
  df
  
  df %>%
    glimpse()
  
  summary(df)
  
  
  #b) Estimate three different regression models using the data generated under part
  #1. Model 1: The OLS regression of Y on a constant and X1
  
  
  model1 <- as.formula(Y1~X1)
  model2 <- as.formula(Y1~X1+X1^2+X1^3)
  model3 <- as.formula(Y1~X1+X2+X3
                       +X1+X2^2+X3^0
                       +X1+X2^0+X3^2
                       +X1^2+X2^0+X3
                       +X1^0+X2^2+X3
                       +X1^0+X2+X3^2
                       +X1^2+X2+X3^0)
  
  reg1 <- feols(model1, data=df, vcov = 'hetero')
  reg2 <- feols(model2, data=df, vcov = 'hetero')
  reg3 <- feols(model3, data=df, vcov = 'hetero')
  
  reg1
  reg2
  reg3
  
  #c) Compute the estimate ˆ fm(x) from each model:
  #m = 1, 2, 3 for 
  #x = (0.1, 0, 0) and
  #x = (1.1, 0, 0). 
  
  #Compute the true value f(x).
  # Add new observation
  new1 <- tibble(X1=0.1,X2=0,X3=0)
  new2 <- tibble(X1=1.1,X2=0,X3=0)
  
  # Standard errors of residuals
  p1 <- predict(reg1, df)
  resid_p1 <- p1-df$Y1
  summary(resid_p1)
  
  # Standard errors of residuals
  p2 <- predict(reg2, df)
  resid_p2 <- p2-df$Y1
  summary(resid_p2)
  
  # Standard errors of residuals
  p3 <- predict(reg3, df)
  resid_p3 <- p3-df$Y1
  summary(resid_p3)
  
  
  #Model 1
  # predict value for newly added obs
  pred1_new <- predict(reg1, newdata = new1 ,se.fit = TRUE, interval = "prediction")
  p1A<- pred1_new$fit
  p1A
  
  # predict value for newly added obs
  pred1B_new <- predict(reg1, newdata = new2 ,se.fit = TRUE, interval = "prediction")
  p1B<- pred1B_new$fit
  p1B
  
  #Model 2
  # predict value for newly added obs
  pred2A_new <- predict(reg2, newdata = new1 ,se.fit = TRUE, interval = "prediction")
  p2A<- pred2A_new$fit
  p2A
  
  # predict value for newly added obs
  pred2B_new <- predict(reg2, newdata = new2 ,se.fit = TRUE, interval = "prediction")
  p2B<- pred2B_new$fit
  p2B
  
  #Model 3
  # predict value for newly added obs
  pred3A_new <- predict(reg3, newdata = new1 ,se.fit = TRUE, interval = "prediction")
  p3A<- pred3A_new$fit
  p3A
  
  # predict value for newly added obs
  pred3B_new <- predict(reg3, newdata = new2 ,se.fit = TRUE, interval = "prediction")
  p3B<- pred3B_new$fit
  p3B
  

#d) Repeat steps a) through c) many times, say, R = 5000. Then you’ll have R possible
#estimates ˆ fm(x) from each model for the two value of x. Compute bias[ ˆ fm(x)],
#V ar[ ˆ fm(x)] and MSE[ ˆ fm(x)] = bias2[ ˆ fm(x)] + V ar[ ˆ fm(x)] for all models and x.
#Organize your results into a table.
#set.seed(13436962)
set.seed(13449499)
#monte carlo simulation
n <- c(500) #sample size
m <- 5000 #number of simulation iterations
#part i) simulate predictors X

betaMatrix <- matrix(NA,100,3)
listMSE <- c()
listpredictionsA <-c()
listpredictionsB <-c()
new1 <- tibble(X1=0.1,X2=0,X3=0)
new2 <- tibble(X1=1.1,X2=0,X3=0)

for(i in 1:500)
{
  x1 <- runif(n =n[i], min = 0, max = 2)
    #rnorm(n=n[i],mean = 2, sd =0.4)
  x2 <- runif(n =n[i], min = 0, max = 2)
  x3 <- runif(n =n[i], min = 0, max = 2)
  for(j in 1:m){
    #simulate the error term m=100 times...
    error <- runif(n = n[i], min =0,max = 1)
    y <-  x1^3-3.5*x1^2+3*x1+error
    lm.out <- lm(y~x1)
    betaMatrix[j,] <- lm.out$coefficients
    mysummary <- summary(lm.out)
    listMSE[j] <- mysummary$sigma^2 #get MSE per iteration
    pred1_new <- predict(lm.out, newdata = new1 ,se.fit = TRUE, interval = "prediction")
    listpredictionsA[j]<- pred1_new$fit
    pred1_new <- predict(lm.out, newdata = new2 ,se.fit = TRUE, interval = "prediction")
    listpredictionsB[j]<- pred1_new$fit
  
  }
  beta_mean <- apply(betaMatrix,2,mean)
  estimation_bias <- beta_mean - c(5,1.2,3)
  beta_var <- apply(betaMatrix,2,var)
  beta_mse <- estimation_bias^2 + beta_var
  cat("Sample size = ",n[i],"\n")
  cat("beta_mean = ",beta_mean,"\nestimation_bias = ",estimation_bias,"\nbeta_var = ",beta_var,"\nbeta_mse = ",beta_mse,"\nModel MSE = ",mean(listMSE))
  cat("\n\n")
}


listMSE
listpredictionsA
listpredictionsB

dfoutputdf <- data.frame(Y1,X1,X2,X3,epsilon,listMSE,listpredictionsA,listpredictionsB)
dfoutputtable <- data.table(Y1,X1,X2,X3,epsilon,listMSE,listpredictionsA,listpredictionsB)

stargazer::stargazer(dfoutputdf)
kable(dfoutputdf)

hist(dfoutputdf$listMSE, xlab = "MSE", col = "grey",
     main = "", cex.lab = 1.5, cex.axis = 1.5)

#e) Pick 10 of your training samples and produce three separate figures where you
#plot the 10 estimates x1 7→ ˆ fm(x1, 0, 0) over a fine grid of x1 values (go from 0 to
                                                                          2 in small steps) for m = 1, 2, 3. Add the plot of f(x1, 0, 0) to each figure. This
#will help you visualize and understand the results under part d).
#f) Interpret the results you have obtained under part d) and e). Discuss bias-variance
#tradeoff, underfit, overfit, etc.

#######Model 2

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