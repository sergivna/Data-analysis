s<-read.csv(file="C:/Users/Julia/Desktop/heart.csv", header=FALSE, skip=1)
years<-as.vector(s$V1)
median(years) 
mean(years)
frequency<-table(years) 
sort(unique(years))[which.max(frequency)] 
var(years)
sd(years)
sd(years)/mean(years)*100
max(years)-min(years)
IQR(years)
quantile(years)
quantile(years, prob = c(0.1, 0.9))
install.packages("moments")
library(moments)
kurtosis(years)
skewness(years)
boxplot(V1~V2, data = s,
	      xlab = "Sex",
            ylab = "Age",
            main = "Heart disase", col="green")
hist(years, col = "lightblue", main = "Heart disase")