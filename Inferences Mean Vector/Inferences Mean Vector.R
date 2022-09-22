#Hypothesis Tesing - Scientific method-based means for using ssample data to
#evaluate conjectures about a population

##A natural generalization of the squareed univariate distance t is the 
#multivariate analog Hotelling's T^2
data <- read.csv(file.choose(), header=T, sep=";")
head(data)
as.matrix(data)
str(data)
summary(data)

#Define Variables
Daging <- data$Daging
Telur.Susu <- data$Telur.dan.susu
Kacang <- data$Kacang.kacangan
Buah <- data$Buah.buahan
Minyak.Lemak <- data$Minyak.dan.lemak
Bumbu <- data$Bumbu.bumbuan

#Normality Assumption
library(MVN)
library(mvnormtest)
mvn(data=data, mvnTest="mardia")

##Homogenity Assumption
variabel=as.factor(rep(c("Daging","Telur dan Susu","Kacang","Buah",
"Minyak dan Lemak","Bumbu"),each=10))
Pengeluaran=c(Daging, Telur.Susu, Kacang, Buah, Minyak.Lemak, Bumbu)
data2=data.frame(variabel,Pengeluaran)
bartlett.test(Pengeluaran~variabel,data2)

myu <- matrix(c(48,1526,1331,42),4,1)
myu
alpha <- 0.1
n=length(Tinggi)
n
p=length(data)
p
xbar<- as.matrix(colMeans(data))
xbar
s=cov(data)
s
solve(s)
T2=n*((t(xbar-myu))%*%(solve(s))%*%(xbar-myu))
T2
F=qf((1-alpha),p,n-p)
F
F2=(((n-1)*p)/(n-p))*F
F2
##KRITERIA UJI, REJECT H0 IF T2 > F2
T2>F2
##FALSE,  H0 ACCEPTED

##LIKELIHOOD RATIO TEST

##Likelihood Ratio Test
library(MVT)
fit <- studentFit(data)
z <- center.test(fit,center=c(5636,7345,4370,5837,5158,3291),test="LRT")
z
p.val = 1
alpha = 0.1
p.val < alpha
##H0 ACCEPTED




