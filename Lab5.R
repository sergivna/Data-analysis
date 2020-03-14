install.packages("plotrix")
install.packages("rcompanion")
library(plotrix)
library(rcompanion)
require(DescTools)

suicide <- read.csv("C:/Users/Julia/Desktop/suicide.csv")
suicides = suicide[,c("suicides.100k.pop")]
generation = suicide[,c("generation")]
generation = c(generation, "Means")
suicides = c(suicides, 1)
stripchart(suicides~generation, xlab="Generations", ylab="Number of suicides in 100k", 
           col=c("red","orange", "yellow", "green", "skyblue", "blue", "white"), vertical = TRUE, 
           group.names=c("Boomers", "G.I. Generation", "Generation X", "Generation Z", "Millenials", "Silent", "Means"))
m = tapply(suicides, generation, mean)

points(7, m[1], col="red", pch=19)
points(7, m[2], col="orange", pch=19)
points(7, m[3], col="yellow", pch=19)
points(7, m[4], col="green", pch=19)
points(7, m[5], col="skyblue", pch=19)
points(7, m[6], col="blue", pch=19)

summary(aov(suicides.100k.pop ~ generation, data = suicide))

aov1 = aov(suicides~generation)
M2 <- lm(suicides~generation)
aov1 = aov(suicides~generation)
summary(M2)

confint(aov1, level = 0.9)
contrasts(suicide$generation)

ScheffeTest(aov1)
