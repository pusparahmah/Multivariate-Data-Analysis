data <- read.csv(file.choose(), header=T, sep=";")
head(data)
str(data)

#Normality Assumption
library(MVN)
hasil = mvn(data=data, mvnTest="mardia")
hasil

#Homogenity Assumption
BM = boxM(data=data[,1:2], group=data1[,3])
BM

#MANOVA
lk <- data$lakiKota
pk <- data$PerempuanKota
ld <- data$LakiDesa
pd <- data$PerempuanDesa
n = 16
v=2
g=2

xbar.k = matrix(c(mean(lk), mean(pk)),2,1)
xbar.k
xbar.d <- matrix(c(mean(ld), mean(pd)), 2,1)
xbar.d
l <- matrix(c(lk, pd), 32,1)
p <- matrix(c(pk, pd),32, 1)
xbar <- matrix(c(mean(l), mean(p)),2,1)
xbar

kota <- t(matrix(c(lk, pk),16,2))
xbar.kota <- matrix(c(mean(lk), mean(pk)),2,16)
xbar.jk <- matrix(c(mean(l), mean(p)),2,16)
dkota.xbar.k <- (kota-xbar.kota)%*%t(kota-xbar.kota)
dkota.xbar.k

dkota.xbar.jk <- (kota-cbar.jk)%*%t(kota-xbar.jk)
dkota.xbar.jk

desa=t(matrix(c(ld,pd),16,2))
xbar.desa=matrix(c(mean(ld),mean(pd)),2,16)
xbar.jk=matrix(c(mean(l),mean(p)),2,16)
ddesa.xbar.d=(desa-xbar.desa)%*%t(desa-xbar.desa)
ddesa.xbar.d
ddesa.xbar.jd=(desa-xbar.jk)%*%t(desa-xbar.jk)
ddesa.xbar.jd
A=(det(dkota.xbar.k+ddesa.xbar.d))/(det(dkota.xbar.jk+ddesa.xbar.jd))
A
((1-sqrt(A))/(sqrt(A)))*((32-g-1)/(g-1))
alpha=0.05
F2=qf((1-alpha),2*(g-1),2*(32-g-1))
F2








