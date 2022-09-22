data <- read.csv(file.choose(), header=T, sep=";")
#We're using multivariate data which variables are Daily Average Stream Flow, Flow Height, then Volume Water in Berbera Rivers with Over 100 Drainage Areakm2, 2015
head(data)
summary(data)

#Descriptive Statistics
X1 <- as.matrix(data[,1])
X2 <- as.matrix(data[,2])
X3 <- as.matrix(data[,3])
X4 <- as.matrix(data[,4])
d1 = mean(X1)
d2 = mean(X2)
d3 = mean(X3)
d4 = mean(X4)
mean = matrix(c(d1,d2,d3,d4), nrow=1, ncol=4)
mean
cor(data)
var(X1)
var(X2)
var(X3)
var(X4)
cov(data)

#Euclidean Distance
#Distance between X1 vector and X2 vector
X1.X2 = (X1-X2)^2
Jarak1 = sqrt(sum(X1.X2)); print(Jarak1)

#Distance between X1 vector and X3 vector
X1.X3 = (X1-X3)^2
Jarak2 = sqrt(sum(X1.X3)); print(Jarak2)

#Distance between X2 vector and X3 vector
X2.X3 = (X2-X3)^2
Jarak3 = sqrt(sum(X2.X3)); print(Jarak3)

#Distance between X1 vector and X4 vector
X1.X4 = (X1-X4)^2
Jarak4 = sqrt(sum(X1.X4)); print(Jarak4)

#Distance between X2 vector and X4 vector
X2.X4 = (X2-X4)^2
Jarak5 = sqrt(sum(X2.X4)); print(Jarak5)

#Distance between X3 vector and X4 vector
X3.X4 = (X3-X4)^2
Jarak6 = sqrt(sum(X3.X4)); print(Jarak6)

