###################################################(1)#################################################
#16171659 Cathaoir Agnew 

library(TSA)
library(quantmod)
library(tseries)
library(forecast)
library(MLmetrics)

#Netflix 
## today's date
today <- Sys.Date()

getSymbols("NFLX", from = "2010-01-01", to = today)
yrmonNFLX <- format(index(NFLX), "%Y-%m")

avopenNFLX <- aggregate(NFLX$NFLX.Open~yrmonNFLX, NFLX, mean, na.rm=TRUE)
openNFLXts <- ts(avopenNFLX$NFLX.Open, frequency=12, start=c(2010,1))

plot(openNFLXts)

acf(openNFLXts, lag.max=36)
acf(diff(openNFLXts),lag.max = 36)

###################################################(2)#################################################

## remove 10% of data, roughly 10 years so remove 12 months

openNFLXts_90 <- ts(openNFLXts, start=c(2010, 1), end=c(2019, 4), freq=12)
openNFLXts_90
plot(openNFLXts_90)


###################################################(3)#################################################


#Augmented Dickey-Fuller Test for stationarity
adf.test(openNFLXts_90)   #since has non significant p-value, data is non stationary

#decomposing data to see what it looks like
openNFLXT_90_decom = decompose(openNFLXts_90)
plot(openNFLXT_90_decom)

# having a look to see if any clear seasonality
plot(openNFLXts_90)
points(x=as.vector(time(openNFLXts_90)), y=as.vector(openNFLXts_90), pch=as.vector(season(openNFLXts_90)),cex=0.8 , col=4) # no clear seasonality 

# clear there is trend in our data so need to remove it to make stationary
plot(diff(openNFLXts_90))   # looking at the differenced data which will elimate trend, varability is increasing as time increases so transform is needed

#using box cox to decide what transformation is needed
BC <- BoxCox.ar(openNFLXts_90, lambda = seq(-2,2,0.1))
BC$mle
BC$ci

# going to take the transform x^(0.2) as this is mle 

# taking the transform to reduce variablilty 
transformed_openNFLTXts_90 <- (openNFLXts_90)^(0.2)
plot(transformed_openNFLTXts_90)

acf(transformed_openNFLTXts_90)  #since all are significant need to more than likely difference
adf.test(transformed_openNFLTXts_90) #non significant p-value so still not stationary so difference

#trend is still very evident so now differencing to get rid of it 
diff_transformed_openNFLTXts_90 <- diff(transformed_openNFLTXts_90)
plot(diff_transformed_openNFLTXts_90)

adf.test(diff_transformed_openNFLTXts_90) # significant p-value so data is now stationary


###################################################(4)#################################################


#acf pacf eacf subsets to get an idea for models
acf(diff_transformed_openNFLTXts_90, lag.max=24)    
pacf(diff_transformed_openNFLTXts_90,lag.max=24)
eacf(diff_transformed_openNFLTXts_90, ar.max = 24 , ma.max=24)


ntflxsub <- armasubsets(diff_transformed_openNFLTXts_90, nar=24, nma=24)
plot(ntflxsub)

#using transformed data and doing differencing in the arima 

nflt_ima1 <- arima(transformed_openNFLTXts_90, order=c(0,1,1))   #best model aic most negative 
nflt_ima1

nflt_ima2 <- arima(transformed_openNFLTXts_90, order=c(0,1,2))   #over fitting on purpose, ma(2) coefficient is not statistically significant as expected 
nflt_ima2

nflt_ari1 <- arima(transformed_openNFLTXts_90, order=c(1,1,0))   # not within +- 2 of aic of ima(1,1) so not statistically significant
nflt_ari1

nflt_ima24 <- arima(transformed_openNFLTXts_90, order=c(0,1,24)) #not within +- 2 of aic of ima(1,1) so not statistically significant
nflt_ima24

nflt_arima11 <- arima(transformed_openNFLTXts_90, order=c(1,1,1)) #ar part not statistical significant 
nflt_arima11

nflt_arima151 <- arima(transformed_openNFLTXts_90, order=c(15,1,1)) #not within +- 2 of aic of ima(1,1) so not statistically significant, ar part not statistical significant 
nflt_arima151


###################################################(5)##################################################
# ima1 was the best aic model, checking residuals for ima1

resid <- rstandard(nflt_ima1)
fit <- fitted(nflt_ima1)

hist(resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid, main="Normal Q-Q plot of Residuals")
qqline(resid)
shapiro.test(resid) #non significant p-value so residuals are approximately normally distributed


plot(as.vector(fit), as.vector(resid) , main ="Fitted Versus Residuals", xlab="Fitted Values", ylab="Residuals") # no obvious trends/patterns


plot(resid, main="Residuals", ylab="Residuals", type="o")


acf(resid, main="ACF of Residuals")  # no significant autocorrelations in the residual series 



LBpvals <- rep(NA, 15)

for(i in 5:15){
  LBpvals[i] <- LB.test(nflt_ima1, lag=i)$p.value
}

LBpvals

plot(LBpvals, ylim=c(0,1), main="P-Values from Ljung-Box Test",
     ylab="P-Values", xlab="Lag")
abline(h=0.05, lty=2)

# The p-values for the Ljung-Box test are all above  0.05, so its white noise 


###################################################(5)##################################################
# ari1, checking residuals for ari1, however is not statistically significant compared to ima1 , as +- 2 outside of AIC 

resid_ar <- rstandard(nflt_ari1)
fit_ar <- fitted(nflt_ari1)

hist(resid_ar, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid_ar, main="Normal Q-Q plot of Residuals")
qqline(resid_ar)
shapiro.test(resid_ar) #non significant p-value so residuals are approximately normally distributed


plot(as.vector(fit_ar), as.vector(resid_ar) , main ="Fitted Versus Residuals", xlab="Fitted Values", ylab="Residuals") # no obvious trends/patterns


plot(resid_ar, main="Residuals", ylab="Residuals", type="o")


acf(resid_ar, main="ACF of Residuals")  # no significant autocorrelations in the residual series 



LBpvals_ar <- rep(NA, 15)

for(i in 5:15){
  LBpvals_ar[i] <- LB.test(nflt_ari1, lag=i)$p.value
}

LBpvals_ar

plot(LBpvals_ar, ylim=c(0,1), main="P-Values from Ljung-Box Test",
     ylab="P-Values", xlab="Lag")
abline(h=0.05, lty=2)

# The p-values for the Ljung-Box test are all above  0.05, so its white noise 


###################################################(6)##################################################
# arima11, checking residuals for arima11, is within +- 2 outside of AIC of ima1,  but ar term is non significant  

resid_arima <- rstandard(nflt_arima11)
fit_arima <- fitted(nflt_arima11)

dev.new(width=8, height=4)

hist(resid_arima, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid_arima, main="Normal Q-Q plot of Residuals")
qqline(resid_arima)
shapiro.test(resid_arima) #non significant p-value so residuals are approximately normally distributed


plot(as.vector(fit_arima), as.vector(resid_arima) , main ="Fitted Versus Residuals", xlab="Fitted Values", ylab="Residuals") # no obvious trends/patterns


plot(resid_arima, main="Residuals", ylab="Residuals", type="o")


acf(resid_arima, main="ACF of Residuals")  # no significant autocorrelations in the residual series 



LBpvals_arima <- rep(NA, 15)

for(i in 5:15){
  LBpvals_arima[i] <- LB.test(nflt_arima11, lag=i)$p.value
}

LBpvals_arima

plot(LBpvals_arima, ylim=c(0,1), main="P-Values from Ljung-Box Test",
     ylab="P-Values", xlab="Lag")
abline(h=0.05, lty=2)

# The p-values for the Ljung-Box test are all above  0.05, so its white noise 

###################################################################(7)###############################################

#plotting predicitions, while transforming back to original scale 

plot(nflt_ima1 , n.ahead = 12 , transform=function(x){x^5} )


pred <- predict(nflt_ima1, n.ahead =12)

predictions = pred$pred

#predicted values transformed back to original scale
predictions_trans = predictions^5


###################################################################(7b)###############################################

# plot of predicitions vs actual data, where the actual data is in red 
dev.new(width=8, height=4)
plot(nflt_ima1 , n.ahead = 12 , transform=function(x){x^5} )
lines(openNFLXts, col="red")

# last 10% of the actual data
nflx_10= openNFLXts[113:124]

#calculating the differnce in the actual values vs the predicitied values 
diff_pred_actual = predictions_trans - nflx_10  

#calculating mean absolute percentage error to get an idea of how the model is performing 
mape_data = MAPE(predictions_trans, nflx_10)

