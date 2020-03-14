 plotForecastErrors <- function(forecasterrors)
  {
    # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors, na.rm = TRUE)/4
    mysd   <- sd(forecasterrors, na.rm = TRUE)
    mymin  <- min(forecasterrors, na.rm = TRUE) - mysd*5
    mymax  <- max(forecasterrors, na.rm = TRUE) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm, na.rm = TRUE)
    mymax2 <- max(mynorm, na.rm = TRUE)
    if (mymin2 < mymin ) { mymin <- mymin2}
    if (mymax2 > mymax) { mymax <- mymax2}
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
install.packages("TTR")
install.packages("tseries")
library("tseries")
library("TTR")
install.packages("forecast")
library("forecast")
library("forecast")

#1
temp<- scan("http://robjhyndman.com/tsdldata/monthly/co2.dat", skip =1)
temps<- ts(temp , frequency=12, start=c(1965))
temps
plot.ts(temps, ylab = "CO2")
#tempis <- log(temps)
#plot.ts(tempis, ylab = "CO2")
#2
tempscomponents<- decompose(temps)
plot(tempscomponents)

tempswithoutseassons <- temps- tempscomponents$seasonal
plot(tempswithoutseassons)

tempsSMA<- SMA(temps, n=5)
plot(tempsSMA)
#3
nsdiffs(temps)
acf(temps)

#4
tempsforecast<-HoltWinters(temps)
tempsforecast
tempsforecast$SSE
plot(tempsforecast)
#5
tempsforecastfuture<-forecast:::forecast.HoltWinters(tempsforecast, h = 12)
forecast:::plot.forecast(tempsforecastfuture)
#6
tsdisplay(tempsforecastfuture$residuals)
Box.test(tempsforecastfuture$residuals, type="Ljung-Box")
plot.ts(tempsforecastfuture$residuals) 
plotForecastErrors(tempsforecastfuture$residuals) 

#ARIMA
temps1 <- diff(temps, lag=frequency(temps), differences=1) 
plot(temps1, type="l", main="Seasonally Differenced")

temps2 <- diff(temps, differences= 2)
plot(temps2 , type="l", main="Differenced and Stationary")
adf.test(temps2) 

arm <- auto.arima(temps)
arm
plot(forecast(arm, h=20))
tsdisplay(residuals(arm))

tsdisplay(arm$residuals)
Box.test(arm$residuals, type="Ljung-Box")
plot.ts(arm$residuals) 
plotForecastErrors(arm$residuals) 
 