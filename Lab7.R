
install.packages("TTR")
install.packages("tseries")
library("tseries")
library("TTR")
install.packages("forecast")
library("forecast")
library("forecast")

births<- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
births
birthstimeserie<- ts(births , frequency=12, start=c(1813))
birthstimeserie
plot.ts(birthstimeserie)
birthstimeseries<- log(birthstimeseries)
plot.ts(log(birthstimeseries))
acf(birthstimeseries)
dif<- diff(birthstimeserie)
plot.ts(dif)
acf(dif)
dif2<-diff(dif, lag=12)
acf(dif2)
#---------Decompose------#
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents)
birthstimeseriescomponents$seasonal
#------------SubSeassons------#

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted<- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
#-------Korelogram-----#
acf(birthstimeseries)
pacf(birthstimeseries)

adf.test(birthstimeseriesseasonallyadjusted)
kpss.test(birthstimeseriesseasonallyadjusted)
fit <- auto.arima(dif2)
fit
tsdisplay(residuals(fit))
Box.test(residuals(fit)) #формальный тест скоррелированности остатков
my_forecast <- forecast(fit, h=3) #прогноз на 3 периода вперед
plot(forecast(fit, h=3))

Acf(residuals(fit),na.action = na.omit)
d<-auto.arima(birthstimeseriesseasonallyadjusted, stepwise=FALSE, approximation=FALSE)
d
p <- auto.arima(birthstimeseriesseasonallyadjusted)
p
tsdisplay(residuals(d))
Box.test(residuals(d))
shapiro.test(d$residuals)

souvenirtimeseriesforecasts <- HoltWinters(log(temps))
souvenirtimeseriesforecasts 
plot(souvenirtimeseriesforecasts)

souvenirtimeseriesforecasts2 <-forecast:::forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
forecast:::plot.forecast(souvenirtimeseriesforecasts2)
souvenirtimeseriesforecasts2$residuals
plot.ts(souvenirtimeseriesforecasts2$residuals)
forecast:::plotForecastErrors(souvenirtimeseriesforecasts2$residuals)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20, na.action = na.omit)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

tempstimeseriesseasonallyadjusted <- temps - tempscomponents$seasonal
plot(tempstimeseriesseasonallyadjusted)