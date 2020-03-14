x1<-rnorm(1000 mean = 5 sd = 1)
x2-rnorm(1000 mean = 2 d = 1)
x3-rnorm(1000 mean = 0 sd = 1)
x<- c(x1, x2, x3)

col1<- c(1, 0.5, 0.4)
col2<- c(0, 0.7, 0.56)
col3<- c(0, 0, 0.72)


matrix <- cbind(col1, col2, col3)
matrix
matrix[1,]

k1 <- matrix[1,] * x
k2 <- matrix[2,] * x
k3 <- matrix[3,] * x

m <- data.frame(ghj = k1, jbj = k2, vb = k3)
m
y1 = matrix[1,] * x1 
y2 = matrix[1,] * x2
y3 = matrix[1,] * x3
y4 = matrix[2,] * x1
y5 = matrix[2,] * x2
y6 = matrix[2,] * x3
y7 = matrix[3,] * x1
y8 = matrix[3,] * x2
y9 = matrix[3,] * x3

y = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9)
y