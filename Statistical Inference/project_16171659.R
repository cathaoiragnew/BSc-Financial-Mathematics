
#Q1 lines roughly 1-550
#Q2 lines roughly 550 - 793
#Q3 lines roughly 793 - 865
#Q4 lines roughly 865- 1023
#Q5 lines roughly 1023 - 



# Q1 (i)
#Creating function
meanpois <- function(n,lambda) { 
  
  sample_pois <- rpois(n,lambda)
  meanpois_sample <- mean(sample_pois)
  return(meanpois_sample)
}

#Q1(ii)
set.seed(16171659)
meanpois_10 <- meanpois(10,1)

#Q1(iii)
set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(10,1)
}

mean(result)
var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias = mean(result) - 1
mse = var(result) + (bias)^2

#Q1 (iv)
# n = 20 

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(20,1)
}

mean_20 = mean(result)
var_20 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_20 = mean_20 - 1
mse_20 = var_20 + (bias_20)^2

# graphing bias vs sample size
plot(bias_20,20, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=20 , lambda = 1
eff_20 = 20 / var_20
# graphing efficiency vs sample size
plot(eff_20,20, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_20,20, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)




########## n = 50 

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(50,1)
}

mean_50 = mean(result)
var_50 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_50 = mean_50 - 1
mse_50 = var_50 + (bias_50)^2

# graphing bias vs sample size
plot(bias_50,50, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=50 , lambda = 1
eff_50 = 50 / var_50
# graphing efficiency vs sample size
plot(eff_50,50, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_50,50, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)


############# n = 200

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(200,1)
}

mean_200 = mean(result)
var_200 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_200 = mean_200 - 1
mse_200 = var_200 + (bias_200)^2

# graphing bias vs sample size
plot(bias_200,200, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=200 , lambda = 1
eff_200 = 200 / var_200
# graphing efficiency vs sample size
plot(eff_200,200, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_200,200, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

################ n = 400

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(400,1)
}

mean_400 = mean(result)
var_400 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_400 = mean_400 - 1
mse_400 = var_400 + (bias_400)^2

# graphing bias vs sample size
plot(bias_400,400, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=400 , lambda = 1
eff_400 = 400 / var_400
# graphing efficiency vs sample size
plot(eff_400,400, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_400,400, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

################# n = 1000

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(1000,1)
}

mean_1000 = mean(result)
var_1000 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_1000 = mean_1000 - 1
mse_1000 = var_1000 + (bias_1000)^2

# graphing bias vs sample size
plot(bias_1000,1000, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=1000 , lambda = 1
eff_1000 = 1000 / var_1000
# graphing efficiency vs sample size
plot(eff_1000,1000, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_1000,1000, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

#Q1(iv)
#n =20 , lambda = .5

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(20,.5)
}

mean_20_.5 = mean(result)
var_20_.5 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_20_.5 = mean_20_.5 - .5
mse_20_.5 = var_20_.5 + (bias_20_.5)^2

# graphing bias vs sample size
plot(bias_20_.5,20, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=20 , lambda = .5
eff_20_.5 = 40 / var_20_.5
# graphing efficiency vs sample size
plot(eff_20_.5,20, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_20_.5,20, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

######################### n = 50 

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(50,.5)
}

mean_50_.5 = mean(result)
var_50_.5 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_50_.5 = mean_50_.5 - .5
mse_50_.5 = var_50_.5 + (bias_50_.5)^2

# graphing bias vs sample size
plot(bias_50_.5,50, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=50 , lambda = .5
eff_50_.5 = 100 / var_50_.5
# graphing efficiency vs sample size
plot(eff_50_.5,50, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_50_.5,50, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

##################### n = 200


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(200,.5)
}

mean_200_.5 = mean(result)
var_200_.5 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_200_.5 = mean_200_.5 - .5
mse_200_.5 = var_200_.5 + (bias_200_.5)^2

# graphing bias vs sample size
plot(bias_200_.5,200, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=200 , lambda = .5
eff_200_.5 = 400 / var_200_.5
# graphing efficiency vs sample size
plot(eff_200_.5,200, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_200_.5,200, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

################ n = 400

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(400,.5)
}

mean_400_.5 = mean(result)
var_400_.5 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_400_.5 = mean_400_.5 - .5
mse_400_.5 = var_400_.5 + (bias_400_.5)^2

# graphing bias vs sample size
plot(bias_400_.5,400, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=400 , lambda = .5
eff_400_.5 = 800 / var_400_.5
# graphing efficiency vs sample size
plot(eff_400_.5,400, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_400_.5,400, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

################# n = 1000

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(1000,.5)
}

mean_1000_.5 = mean(result)
var_1000_.5 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_1000_.5 = mean_1000_.5 - .5
mse_1000_.5 = var_1000_.5 + (bias_1000_.5)^2

# graphing bias vs sample size
plot(bias_1000_.5,1000, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=1000 , lambda = .5
eff_1000_.5 = 2000 / var_1000_.5
# graphing efficiency vs sample size
plot(eff_1000_.5,1000, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_1000_.5,1000, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)


#####################################
#n =20 , lambda = 4

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(20,4)
}

mean_20_4 = mean(result)
var_20_4 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_20_4 = mean_20_4 - 4
mse_20_4 = var_20_4 + (bias_20_4)^2

# graphing bias vs sample size
plot(bias_20_4,20, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=20 , lambda = 4
eff_20_4 = 5 / var_20_4
# graphing efficiency vs sample size
plot(eff_20_4,20, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_20_4,20, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

############################## n = 50


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(50,4)
}

mean_50_4 = mean(result)
var_50_4 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_50_4 = mean_50_4 - 4
mse_50_4 = var_50_4 + (bias_50_4)^2

# graphing bias vs sample size
plot(bias_50_4,50, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=50 , lambda = 4
eff_50_4 = 50/4  / var_50_4
# graphing efficiency vs sample size
plot(eff_50_4,50, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_50_4,50, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)


############################## n = 200

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(200,4)
}

mean_200_4 = mean(result)
var_200_4 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_200_4 = mean_200_4 - 4
mse_200_4 = var_200_4 + (bias_200_4)^2

# graphing bias vs sample size
plot(bias_200_4,200, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=200 , lambda = 4
eff_200_4 = 50  / var_200_4
# graphing efficiency vs sample size
plot(eff_200_4,200, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_200_4,200, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

######################## n = 400

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(400,4)
}

mean_400_4 = mean(result)
var_400_4 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_400_4 = mean_400_4 - 4
mse_400_4 = var_400_4 + (bias_400_4)^2

# graphing bias vs sample size
plot(bias_400_4,400, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=400 , lambda = 4
eff_400_4 = 100  / var_400_4
# graphing efficiency vs sample size
plot(eff_400_4,400, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_400_4,400, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)

################# n = 1000


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanpois(1000,4)
}

mean_1000_4 = mean(result)
var_1000_4 = var(result)

# mean square error = var(thetha tilda) + [ bias(thetha tilda)]^2
# bias(thetha tilda) = E(thetha tilda) - thetha 
bias_1000_4 = mean_1000_4 - 4
mse_1000_4 = var_1000_4 + (bias_1000_4)^2

# graphing bias vs sample size
plot(bias_1000_4,1000, main="Bias vs sample size" , xlab="bias", ylab="sample size", pch=19)

#efficiency eff(lambda tilda) = crlb/ var(lambda tilda)
# crlb = n/lambda , n=1000 , lambda = 4
eff_1000_4 = 250  / var_1000_4
# graphing efficiency vs sample size
plot(eff_1000_4,1000, main="Efficiency vs sample size" , xlab="efficiency", ylab="sample size", pch=19)

# graphing mean square error vs sample size
plot(mse_1000_4,1000, main="MSE vs sample size" , xlab="MSE", ylab="sample size", pch=19)



############### plotting all bias vs sample size togther
# plotting them all together for lambda = 1 
biasPlot <- c(bias_20,bias_50,bias_200,bias_400,bias_1000)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, biasPlot, main ="Bias vs Sample size" , xlab = "sample size", ylab="bias",pch=19)

# plotting them all together for lambda = .5, outlier is explained by the n=50 cause where mean was = .5
bias_.5_Plot <- c(bias_20_.5,bias_50_.5,bias_200_.5,bias_400_.5,bias_1000_.5)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot,bias_.5_Plot, main ="Bias lambda=.5 vs Sample size" , xlab = "sample size", ylab="bias",pch=19)


# plotting them all together for lambda = 4
bias_4_Plot <- c(bias_20_4,bias_50_4,bias_200_4,bias_400_4,bias_1000_4)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot,bias_4_Plot, main ="Bias lambda=4 vs Sample size" , xlab = "sample size", ylab="bias",pch=19)

################# plotting all efficiency vs sample size together
# plotting them all together for lambda = 1 
effPlot <- c(eff_20,eff_50,eff_200,eff_400,eff_1000)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, effPlot, main ="Effeciency vs Sample size" , xlab = "sample size", ylab="Effeciency",pch=19)

# plotting them all together for lambda = .5 
eff_.5_Plot <- c(eff_20_.5,eff_50_.5,eff_200_.5,eff_400_.5,eff_1000_.5)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, eff_.5_Plot, main ="Effeciency lambda=.5 vs Sample size" , xlab = "sample size", ylab="Effeciency",pch=19)

# plotting them all together for lambda = 4 
eff_4_Plot <- c(eff_20_4,eff_50_4,eff_200_4,eff_400_4,eff_1000_4)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, eff_4_Plot, main ="Effeciency lambda=4 vs Sample size" , xlab = "sample size", ylab="Effeciency",pch=19)


################################ plotting mse vs sample size 
# plotting them all together for lambda = 1 
msePlot <- c(mse_20,mse_50,mse_200,mse_400,mse_1000)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, msePlot, main ="Mean square error vs Sample size" , xlab = "sample size", ylab="MSE",pch=19)


# plotting them all together for lambda = .5 
mse_.5_Plot <- c(mse_20_.5,mse_50_.5,mse_200_.5,mse_400_.5,mse_1000_.5)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, mse_.5_Plot, main ="Mean square error lambda=.5 vs Sample size" , xlab = "sample size", ylab="MSE",pch=19)


# plotting them all together for lambda = .5 
mse_4_Plot <- c(mse_20_4,mse_50_4,mse_200_4,mse_400_4,mse_1000_4)
samplesize_plot <- c(20,50,200,400,1000)
plot(samplesize_plot, mse_4_Plot, main ="Mean square error lambda=4 vs Sample size" , xlab = "sample size", ylab="MSE",pch=19)












#Q2 
#Creating function
meannormal <- function(n,mean,sd) { 
  
  sample_normal <- rnorm(n,mean,sd)
  meannormal_sample <- mean(sample_normal)
  return(meannormal_sample)
}

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meannormal(5,1,1)
}

mean(result)

#histogram of the 1000 mean results 
hist(result) 

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')

########################## n = 20

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meannormal(20,1,1)
}

mean(result)

#histogram of the 1000 mean results 
hist(result) # this looks normally distributed

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')

######################## n = 50

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meannormal(50,1,1)
}

mean(result)

#histogram of the 1000 mean results 
hist(result) # this looks normally distributed

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')


# Q2 (ii)

#Creating function
meanexp <- function(n,lambda) { 
  
  sample_exp <- rexp(n,lambda)
  meanexp_sample <- mean(sample_exp)
  return(meanexp_sample)
}

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanexp(5,1)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')

########################## n = 20

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanexp(20,1)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')

##################### n = 50

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanexp(50,1)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)
# red line is expected values if data is normally distributed
qqline(result, col='red')


############################

#Creating function
meanber <- function(n,size,p) { 
  
  sample_ber <- rbinom(n,size,p)
  meanber_sample <- mean(sample_ber)
  return(meanber_sample)
}

set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(5,1,.5)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)


########################## n = 20


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(20,1,.5)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)


################### n = 50


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(50,1,.5)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)


##################### n = 5 p = .05


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(5,1,.05)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)

####################### n= 20 p=.05


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(20,1,.05)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)


######################## n= 50 p=.05


set.seed(16171659)
result = rep(NA,1000)

for(i in 1:1000) {
  result[i] = meanber(50,1,.05)
}
mean(result)

#histogram of the 1000 mean results 
hist(result)

#qq plot
qqnorm(result)



################################ Q3
#(i)
  set.seed(16171659)
  sample_gamma <- rgamma(100,3,1)
  #(ii)
  # mle = n * alpha / sum of xi
  # mle = alpha / xbar
  
  #(iii)
  #################################################################
  # likelihood function of gamma
   
  like <- function(alpha, x) {
   a = alpha
   n=length(x)
  lambda = (n * a)/sum(x)
  
    
    like = n*a*log(lambda) + (a-1)* sum(log(x)) - lambda*sum(x) - n*log(gamma(a))
    like
  }
  
  alpha = seq(from=0.5, to = 10, by = .2)
  plot(alpha,like(alpha,sample_gamma))

  
   # by eye id say ahat ~ 3.5 and lambdahat ~ 1.2
##################################################################
  
#Q3(iv)
  # gamma minus log likelihood

set.seed(16171659) ; alpha = 3 ; lambda = 1

sample_gamma <- rgamma(100,shape = alpha ,scale = lambda )
  
  gll <- function(thetha,datta)
  {
    a <- thetha[1] ; b<- thetha[2]
    n = length(datta)

    sumg = sum(datta)
    sumlogg <- sum(log(datta))
    
    gll <- n*a*log(b) + n *lgamma(a) + sumg/b - (a-1)*sumlogg
    gll
    
  }
  
  momalpha <- mean(sample_gamma)^2 / var(sample_gamma) 
  momlambda <- var(sample_gamma)/ mean(sample_gamma)
  
  gammasearch = nlm(gll,c(momalpha,momlambda), hessian =T, datta = sample_gamma)
  
  V_hat = solve(gammasearch$hessian)

  SEalphahat <- sqrt(V_hat[1,1])  
  SElambdahat <- sqrt(V_hat[2,2])
  
  alphahat = gammasearch$estimate[1]
  lambdahat = gammasearch$estimate[2]
  
  Lalpha = alphahat - 1.96* SEalphahat
  Ualpha = alphahat + 1.96* SEalphahat
  
  Llambda = lambdahat - 1.96*SElambdahat
  Ulambda = lambdahat + 1.96*SElambdahat
  cat("\nEstimated alpha = " , round(alphahat,3) , " 95 % CI is ", round(Lalpha,3), " to " , round(Ualpha,3), "\n\n")
  
  cat("\nEstimated lambda = " , round(lambdahat,3) , " 95 % CI is ", round(Llambda,3), " to " , round(Ulambda,3), "\n\n")
  
  
  ############################################################
  #Q4
  #(i)
  
  normal_95CI <- function (n,u,s)
  {
    #n is sample size
    #u is sample mean 
    #s is sample standard deviation
    sample <- rnorm(n,u,s)
    
    x_bar = mean(sample)
    s_d = sd(sample)
  
    error <- qnorm(.975) * s_d/sqrt(n)
    
    Z_Lbound = x_bar - error
    Z_Ubound = x_bar + error
    
    
    cat(" 95 % CI is ", round(Z_Lbound,3), " to " , round(Z_Ubound,3), "\n\n")
  }
  
  #(ii)
  
  t_95CI <- function(n,u,s)
  {
    #n is sample size
    #u is sample mean 
    #s is sample standard deviation
    sample <- rnorm(n,u,s)
    
    x_bar = mean(sample)
    s_d = sd(sample)
    
    error <- qt(.975, df = n-1) * s_d/sqrt(n)
    
    T_Lbound <- x_bar - error
    T_Ubound <- x_bar + error
    
    cat(" 95 % CI is ", round(T_Lbound,3), " to " , round(T_Ubound,3), "\n\n")
    
    
  }
  
  #(iii) Use t distribution if sample size is smaller than 30 ( n < 30 ). The t-distribution incorporates the fact that for smaller sample sizes the distribution will be more spread out, using degrees of freedom.
  # Thats why it is better to use t-distribution for samples of size less than 30, rather than z.
  
  #(iv)
  
  set.seed(16171659)
  
  count_z_10 = 0 #count the number of times true mean is containing with z CI
  
  #we know true mean is 1,since we set it, so for it to be contained in interval, our lower bound must be less than one and upper greater than 1
  
  # n=10 u = 1 s = 1, using z 
  
  for(i in 1:1000 )
  {
     # intervals attained in (i) by
    x <- rnorm(10,1,1)
    x_bar = mean(x)
    s_d = sd(x)
    error <- qnorm(.975) * s_d/sqrt(10)
    
    l = x_bar - error
    ub = x_bar + error
    
    if( l<= 1 &&  ub >= 1 ) count_z_10 = count_z_10+1
    else count_Z_10 = count_z_10
    
    
  }
   count_z_10
  
   ##################################################################
   

   
   # n=10 , u = 1, s = 1  using t 
   
   count_t_10 = 0
   
  for(j in 1:1000)
  {
    # intervals attained in (ii) by
    sample <- rnorm(10,1,1)
    
    x_bar = mean(sample)
    s_d = sd(sample)
    
    error <- qt(.975, df = 10-1) * s_d/sqrt(10)
    
    T_Lb <- x_bar - error
    T_Ub <- x_bar + error
    
    if( T_Lb <= 1 &&  T_Ub >= 1 ) count_t_10 = count_t_10+1
    else count_t_10 = count_t_10  
  
  }
  count_t_10
  
  
  
  ####################################################################
  #Q4(v)
  
  # n=500 u = 1 s = 1, using z 
  count_z_500 = 0
  
  for(k in 1:1000 )
  {
    # intervals attained in (i) by
    x <- rnorm(500,1,1)
    x_bar = mean(x)
    s_d = sd(x)
    error <- qnorm(.975) * s_d/sqrt(500)
    
    l = x_bar - error
    ub = x_bar + error
    
    if( l <= 1 &&  ub >= 1 ) count_z_500 = count_z_500+1
    else count_Z_500 = count_z_500
  
  }
  count_z_500
  
  ##################################################################
  
  
  
  # n=500 , u = 1, s = 1  using t 
  
  count_t_500 = 0
  
  for(m in 1:1000)
  {
    # intervals attained in (ii) by
    sample <- rnorm(500,1,1)
    
    x_bar = mean(sample)
    s_d = sd(sample)
    
    error <- qt(.975, df = 500-1) * s_d/sqrt(500)
    
    T_Lb <- x_bar - error
    T_Ub <- x_bar + error
    
    if( T_Lb <= 1 &&  T_Ub >= 1 ) count_t_500 = count_t_500+1
    else count_t_500 = count_t_500  
    
  }
  count_t_500
  
  
  #############################################################################
  #Q5
  #(i)
  
  #Bootstraping - is a method that generates multiple samples of size n  by sampling a sample with replacement.
  #This technique allows estimation of the sampling distribution of almost any statistic using random sampling methods
  # example, say sample is 1,2,3,4,5 , then possible bootstrap samples are:
  # 1,2,3,4,4
  # 2,3,4,5,2
  # 3,3,3,3,3
  # 4,4,2,2,1,
  
  
  
  ################################################
  
  #(ii)
  set.seed(16171659)

   # sample of 100 random exp distribution
    sample_exp = rexp(100,1)
  
  #obtaining 1000 bootstrap values
  resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 

  r.median <- sapply(resamples, median, na.rm = TRUE)  
   quantile(r.median, c(.025,.975))

  ####################  
  
  # (iii)
  
   
  set.seed(16171659)
  
# true median for exp distribution is, median = lambda * ln2 
  
  true_median = 1 * log(2)
  count_exp= 0
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(100,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.median <- sapply(resamples, median, na.rm = TRUE)  
    
    
    E_Lb = quantile(r.median,.025)
    E_Ub = quantile(r.median,.975)
    
    if( E_Lb <= true_median  &&  E_Ub >= true_median ) count_exp = count_exp+1
    else count_exp = count_exp
    
  }
  
  count_exp

  ##########################################################################
  
  #Q5(iv)
# Asymptotic distribution of the mle (wald).
# I(lambda) = 1/ lambda^-2
# So CI is :
# lambda +- 1.96 sqrt(lambda^2 /n)  
# Using mle for lambda in above 
# xbar +- 1.96 sqrt(xbar^2 /n)  
  
  
  set.seed(16171659)
  
  # true lambda is 1 
  true_lambda = 1
  true_median = 1 * log(2)
  
  count_med =0
  count_lambda= 0
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(100,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.mean <- sapply(resamples, mean, na.rm = TRUE)  
    
    mle <- mean(r.mean)

    lambda_Lb= mle - 1.96 * sqrt(mle^2 / 100)
    lambda_Ub= mle + 1.96 * sqrt(mle^2 / 100)
    
  
    if( lambda_Lb <= true_lambda  &&  lambda_Ub >= true_lambda )
      count_lambda = count_lambda+1
    else 
      count_lambda = count_lambda
    
    # now using for median 
    #median = ln(2) / lambda 
    median_exp = log(2)/ (mle) 
    
    # now use median_exp instead of mle in CI formula

  med_Lb = median_exp - 1.96 * sqrt(median_exp^2 / 100)
  med_Ub =  median_exp + 1.96 * sqrt(median_exp^2 / 100)
    
    
  if( med_Lb <= true_median  &&  med_Ub >= true_median )
      count_med = count_med+1
  else 
      count_med = count_med
    
  }
  
  count_lambda
  count_med
  

  ##################################################
  
  #(iii) n =10
  

  set.seed(16171659)
  
  # true median for exp distribution is, median = lambda * ln2 
  
  true_median = 1 * log(2)
  count_exp_10 = 0
  
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(10,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.median <- sapply(resamples, median, na.rm = TRUE)  
    
    
    E_Lb = quantile(r.median,.025)
    E_Ub = quantile(r.median,.975)
    
    if( E_Lb <= true_median  &&  E_Ub >= true_median ) count_exp_10 = count_exp_10+1
    else count_exp_10 = count_exp_10
    
  }
  
  count_exp_10
  
  ##################################
  
  #(iii) n = 50
  
  
  
  set.seed(16171659)
  
  # true median for exp distribution is, median = lambda * ln2 
  
  true_median = 1 * log(2)
  count_exp_50 = 0
  
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(50,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.median <- sapply(resamples, median, na.rm = TRUE)  
    
    
    E_Lb = quantile(r.median,.025)
    E_Ub = quantile(r.median,.975)
    
    if( E_Lb <= true_median  &&  E_Ub >= true_median ) count_exp_50 = count_exp_50+1
    else count_exp_50 = count_exp_50
    
  }
  
  count_exp_50
  
  ########################################
  
  #(iv) n = 10
  
  
  set.seed(16171659)
  
  # true lambda is 1 
  true_lambda = 1
  true_median = 1 * log(2)
  
  count_med_10 =0
  count_lambda_10 = 0
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(10,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.mean <- sapply(resamples, mean, na.rm = TRUE)  
    
    mle <- mean(r.mean)
    
    lambda_Lb= mle - 1.96 * sqrt(mle^2 / 10)
    lambda_Ub= mle + 1.96 * sqrt(mle^2 / 10)
    
    
    if( lambda_Lb <= true_lambda  &&  lambda_Ub >= true_lambda )
      count_lambda_10 = count_lambda_10 +1
    else 
      count_lambda_10 = count_lambda_10
    
    # now using for median 
    #median = ln(2) / lambda 
    median_exp = log(2)/ (mle) 
    
    med_Lb = median_exp - 1.96 * sqrt(median_exp^2 / 10)
    med_Ub =  median_exp + 1.96 * sqrt(median_exp^2 / 10)
    
    
    if( med_Lb <= true_median  &&  med_Ub >= true_median )
      count_med_10 = count_med_10 +1
    else 
      count_med_10 = count_med_10
    
  }
  
  count_lambda_10
  count_med_10
  
  #####################################
  
  #(iv) n = 50 
  
  
  set.seed(16171659)
  
  # true lambda is 1 
  true_lambda = 1
  true_median = 1 * log(2)
  
  count_med_50 =0
  count_lambda_50 = 0
  
  
  for(n in 1:1000)
  {
    
    # sample of 100 random exp distribution
    sample_exp = rexp(50,1)
    
    #obtaining 1000 bootstrap values
    resamples <- lapply(1:1000, function(i) sample(sample_exp,length(sample_exp),replace = TRUE)) 
    
    r.mean <- sapply(resamples, mean, na.rm = TRUE)  
    
    mle <- mean(r.mean)
    
    lambda_Lb= mle - 1.96 * sqrt(mle^2 / 50)
    lambda_Ub= mle + 1.96 * sqrt(mle^2 / 50)
    
    
    if( lambda_Lb <= true_lambda  &&  lambda_Ub >= true_lambda )
      count_lambda_50 = count_lambda_50 +1
    else 
      count_lambda_50 = count_lambda_50
    
    # now using for median 
    #median = ln(2) / lambda 
    median_exp = log(2)/ (mle) 
    
    med_Lb = median_exp - 1.96 * sqrt(median_exp^2 / 50)
    med_Ub =  median_exp + 1.96 * sqrt(median_exp^2 / 50)
    
    
    if( med_Lb <= true_median  &&  med_Ub >= true_median )
      count_med_50 = count_med_50+1
    else 
      count_med_50 = count_med_50
    
  }
  
  count_lambda_50
  count_med_50
  
  ################################################################
  