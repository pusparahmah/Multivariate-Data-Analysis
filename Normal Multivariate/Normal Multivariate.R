#Concept of Normal Multivariate 
data <- read.csv(file.choose(), header=T, sep=";")
head(data)
str(data)
summary(data)
X1 <- data$Ikan
X2 <- data$Daging
X3 <- data$Telur.dan.susu
data1 <- data.frame(X1,X2,X3)
data1
summary(data1)

#1. X density has the same mean value as the mode
histx1 <- mvn(data=data1,mvnTest="royston",univariatePlot="histogram")
#2. The linear combination of the X variables is normally distributed
x1 <- as.matrix((X1), 10, 1)
x2 <- as.matrix((X2), 10, 1)
x3 <- as.matrix((X3), 10, 1)
linecomb <- 2*x1+5*x2+2*x3
library(mvnormtest)
shapiro.test(linecomb)
#3. Each of variable is normal distribution
mvnorm <- mvn(data=data1, mvnTest="mardia")
mvnorm
#4. The coefficient of variation is 0, which means that each variable independent distribution
cov(data)

#Multivariate Data Normality Distribution Test Using QQ Plot
data <- read.csv(file.choose()header=T, sep=";")
head(data)
str(data)
#Define each variable
Padi <- data$Padi.padian
Umbi <- data$Umbi.umbian
Ikan <- data$Ikan
Daging <- data$Daging
Telur.Susu <- data$telur.dam.susu
Sayur <- data$Sayur.sayuran
Kacang <- data$kacang.kacangan
Buah <- data$Buah.buahan
Minyak.Lemak <- data$Minyak.dan.lemak
Bumbu <- data$Bumbu.bumbuan
Fast.Food <- data$Makanan.dan.minuman.jadi
#Univariate data Normality Distribution Test
##Padi
qqnorm(Padi)
qqline(Padi, col="blue")
##Umbi
qqnorm(Umbi)
qqline(Umbi, col="blue")
##Ikan
qqnorm(Ikan)
qqline(Ikan, col="blue")
##Daging
qqnorm(Daging)
qqline(Daging, col="blue")
##Telur dan Susu
qqnorm(Telur.Susu)
qqline(Telur.Susu, col="blue")
##Sayur - sayuran
qqnorm(Sayur)
qqline(Sayur, col="blue")
##Kacang-kacangan
qqnorm(Kacng)
qqline(Kacang, col="blue")
##Buah-buahan
qqnorm(Buah)
qqline(Buah, col="blue")
##Minyak dan lemak
qqnorm(Minyak.Lemak)
qqline(Minya.Lemak, col="blue")
##Bumbu-bumbuan
qqnorm(Bumbu)
qqline(Bumbu, col="blue")
##Makanan dan Minuman Jadi
qqnorm(Fast.Food)
qqline(Fast.Food, col="blue")

Simultaneously Multivariate Data Normality Test
library(MVN)
mvnorm <- mvn(data=data, mvnTest="mardia")
mvnorm






