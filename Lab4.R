s<-read.csv(file="C:/Users/Julia/Desktop/salary.csv", header=FALSE, skip=1)
#1
plot(s$V1~s$V2, xlab="Views", ylab="Comments")
#2 параметри регресійної моделі
summary(lm(s$V1~s$V2))

#3
model<-lm(s$V1~s$V2)$coefficients
abline(model,col="green")

#4 спостереження-прогноз
model<-lm(s$V1~s$V2)
plot(s$V1, model$fitted.values, xlab="Observation", ylab="Prediction")
abline(0, 1,col="green")

#5прогноз-залишки
plot(model$fitted.values,model$residuals, xlab="Prediction",ylab="Residuals")
abline(0,0,col="red")

#6 QQplot 
qqnorm(model$residuals)
qqline(model$residuals,col="red")
