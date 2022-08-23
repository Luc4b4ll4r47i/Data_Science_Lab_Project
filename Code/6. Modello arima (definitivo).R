###############################################################################################################################################################################################################################################################################################################################################################################################################################################################
testnorm<-function(residui, nclassi)
{
  res.stand<- (residui - mean(residui))/sd(residui)
  hist(res.stand,freq=F,main="", col= "lightblue", ylim=c(0,0.6), nclass = nclassi)
  curve(dnorm, add=TRUE, col= "red", lwd=2)
  library(tseries)
  jb<- jarque.bera.test(res.stand)
  legend("topright", legend= paste("JB test p.value = ", round(jb$p.value,3)), bty="n",
         cex=1.2)
  jb
}
#####################################################################################################################################################################################################################
#import librerie, lettura dati
library("forecast")
library("lubridate")
library("tseries")
library("zoo")
library("ggplot2")
library("tsoutliers")
library("car")
library("dplyr")



library(readr) 
df_no_cov <- read_csv("C:/Users/danie/OneDrive/Desktop/DSLAB/Dati/3. dataset finali usati per il progetto_dal clustering/per_boxplot.csv")
########################################################################################################################################


####################STUDIO R1##############################
my_ts_R1 <- ts(df_no_cov$`Vendite R1`, start=1,frequency=7)

#gestione outliers
myts1 <- tsclean(my_ts_R1)
plot(myts1)

#studio autocorrelazione ACF

par(mfrow=c(2,1)) 
acf(myts1)
pacf(myts1)

## dal grafico si evince autocorrelazione.
#adesso faccio durbin watson per vedere meglio questa cosa.
library("lmtest")
dwtest(myts1[-826] ~ myts1[-1]) #rifiuto ipotesi nulla. C'è autocorrelazione



#Costruzione modello arima
mod_r1 <- auto.arima(myts1) # ARIMA(0,0,2) (0,1,1)
summary(mod_r1)

#Autocorrelazione residui
par(mfrow=c(2,1)) 
acf(mod_r1$residuals)
pacf(mod_r1$residuals)
dwtest(mod_r1$residuals[-826] ~ mod_r1$residuals[-1]) #non c'è autocorrelazione

#Normalità residui
par(mfrow=c(2,1))
testnorm(mod_r1$residuals, 30) #i residui non sono normali 
qqnorm(mod_r1$residuals, pch = 1, frame = FALSE)
qqline(mod_r1$residuals, col = "steelblue", lwd = 2)


#Previsione: grafici e valori. 12 settimane di previsione
arimafore_r1 <- forecast(mod_r1, h=91, level=95) #h non è in funzione dei cicli ma è giornaliero
par(mfrow=c(2,1))
plot(arimafore_r1)
plot(arimafore_r1, xlim = c(117,135))
arimafore_r1$mean  #12 settimane di previsione



#Grafico fit del modello mod_r1
par(mfrow=c(1,1))
fit <- mod_r1
upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
plot(myts1, type="n", ylim=range(lower,upper))
polygon(c(time(myts1),rev(time(myts1))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
lines(myts1)
lines(fitted(fit),col='red')
out <- (myts1 < lower | myts1 > upper)
points(time(myts1)[out], myts1[out], pch=19)


#fare divisione in training e test set e valutare bontà adattamento

#data train
mytstrain1 <- window(myts1, start = 1, end = c(93,7)) #80% dati train
plot(mytstrain1)
time(mytstrain1)

#modello da valutare
mod_evaluation_1 <- auto.arima(mytstrain1)
forec_eval_1 <- forecast(mod_evaluation_1,h=175)

#data test
mytest1 <-window(myts1, start = 94)
time(mytest1)

#valutazione modello
accuracy(forec_eval_1, mytest1)
###############################################

####################STUDIO R2###################
my_ts_R2 <- ts(df_no_cov$`Vendite R2`, start=1,frequency=7)
#gestione outliers
myts2 <- tsclean(my_ts_R2)
plot(myts2)

#studio autocorrelazione ACF

par(mfrow=c(2,1)) 
acf(myts2)
pacf(myts2)

## dal grafico si evince autocorrelazione.
#adesso faccio durbin watson per vedere meglio questa cosa.
library("lmtest")
dwtest(myts2[-826] ~ myts2[-1]) #rifiuto ipotesi nulla. C'è autocorrelazione


#Costruzione modello arima
mod_r2 <- auto.arima(myts2) # ARIMA(4,0,3) (0,1,1)
summary(mod_r2)

#Autocorrelazione residui
par(mfrow=c(2,1)) 
acf(mod_r2$residuals)
pacf(mod_r2$residuals)
dwtest(mod_r2$residuals[-826] ~ mod_r2$residuals[-1]) #non c'è autocorrelazione

#Normalità residui
par(mfrow=c(2,1))
testnorm(mod_r2$residuals, 30) #i residui non sono normali 
qqnorm(mod_r2$residuals, pch = 1, frame = FALSE)
qqline(mod_r2$residuals, col = "steelblue", lwd = 2)


#Previsione: grafici e valori. 12 settimane di previsione
arimafore_r2 <- forecast(mod_r2, h=91, level=95) #h non è in funzione dei cicli ma è giornaliero
par(mfrow=c(2,1))
plot(arimafore_r2)
plot(arimafore_r2, xlim = c(117,135))
arimafore_r2$mean  #12 settimane di previsione



#Grafico fit del modello mod_r2
par(mfrow=c(1,1))
fit <- mod_r2
upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
plot(myts2, type="n", ylim=range(lower,upper))
polygon(c(time(myts2),rev(time(myts2))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
lines(myts2)
lines(fitted(fit),col='red')
out <- (myts2 < lower | myts2 > upper)
points(time(myts2)[out], myts2[out], pch=19)


#fare divisione in training e test set e valutare bontà adattamento

#data train
mytstrain2 <- window(myts2, start = 1, end = c(93,7)) #80% dati train
plot(mytstrain2)
time(mytstrain2)

#modello da valutare
mod_evaluation_2 <- auto.arima(mytstrain2)
forec_eval_2 <- forecast(mod_evaluation_2,h=175)

#data test
mytest2 <-window(myts2, start = 94)

#valutazione modello
accuracy(forec_eval_2, mytest2)

###############################################

####################STUDIO R6###################
my_ts_R6 <- ts(df_no_cov$`Vendite R6`, start=1,frequency=7)
#gestione outliers
myts6 <- tsclean(my_ts_R6)
plot(myts6)

#studio autocorrelazione ACF

par(mfrow=c(2,1)) 
acf(myts6)
pacf(myts6)

## dal grafico si evince autocorrelazione.
#adesso faccio durbin watson per vedere meglio questa cosa.
library("lmtest")
dwtest(myts6[-826] ~ myts6[-1]) #rifiuto ipotesi nulla. C'è autocorrelazione





#Costruzione modello arima
mod_r6 <- auto.arima(myts6) # ARIMA(2,0,3) (0,1,2)
summary(mod_r6)

#Autocorrelazione residui
par(mfrow=c(2,1)) 
acf(mod_r6$residuals)
pacf(mod_r6$residuals)
dwtest(mod_r6$residuals[-826] ~ mod_r6$residuals[-1]) #non c'è autocorrelazione

#Normalità residui
par(mfrow=c(2,1))
testnorm(mod_r6$residuals, 30) #i residui non sono normali 
qqnorm(mod_r6$residuals, pch = 1, frame = FALSE)
qqline(mod_r6$residuals, col = "steelblue", lwd = 2)


#Previsione: grafici e valori. 12 settimane di previsione
arimafore_r6 <- forecast(mod_r6, h=91, level=95) #h non è in funzione dei cicli ma è giornaliero
par(mfrow=c(2,1))
plot(arimafore_r6)
plot(arimafore_r6, xlim = c(117,135))
arimafore_r6$mean  #12 settimane di previsione



#Grafico fit del modello mod_r6
par(mfrow=c(1,1))
fit <- mod_r6
upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
plot(myts6, type="n", ylim=range(lower,upper))
polygon(c(time(myts6),rev(time(myts6))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
lines(myts6)
lines(fitted(fit),col='red')
out <- (myts6 < lower | myts6 > upper)
points(time(myts6)[out], myts6[out], pch=19)


#fare divisione in training e test set e valutare bontà adattamento

#data train
mytstrain6 <- window(myts6, start = 1, end = c(93,7)) #80% dati train
plot(mytstrain6)
time(mytstrain6)

#modello da valutare
mod_evaluation_6 <- auto.arima(mytstrain6)
forec_eval_6 <- forecast(mod_evaluation_6,h=175)

#data test
mytest6 <-window(myts6, start = 94)

#valutazione modello
accuracy(forec_eval_6, mytest6)




