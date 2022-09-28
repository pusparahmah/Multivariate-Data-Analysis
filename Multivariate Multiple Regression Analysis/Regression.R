# Here we're gonna use data from Badan Pusat Statistik Jawa Barat about
# "Biaya Produksi dan Penambahan Modal dan Konsumsi Rumah Tangga yang
# diperngaruhi olhe Indeks Harga Diterima, Indeks Harga Dibayar, 
# Nilai Tukar Petani, Dan Nilai Tukar Usaha Pertanian di Daerah Jawa Barat"
# We're separate dependent variables and independent variables:

# Dependent Variabels:
# Y1: Indeks Konsumen Harga Rumah Tangga
# Y2: Indeks Biaya Produksi dan Penambahan Modal

#Independent Variables:
# X1: Indeks Harga Diterima
# X2: Nilai Tukar Petani
# X3: Nilai Tukar Usaha Pertanian

# First, we need to estimate multivariae regression coefficient followed by this steps:

data <- read.csv(file.choose(), header=T, sep=";")
y1 <- data$y1
y2 <- data$y2
x1 <- data$z1
x2 <- data$z2
x3 <- data$z3

#----FULL MODEL----3
# Matrix X
x0 <- matrix(1, nrow=12)
X <- cbind(x1,x2,x3); X
# Matrix Y
Y <- cbind(y1,y2); Y
# Transpose matrix X
XT <- t(X); XT
# Multipliates the X Matrix with the X transpose matrix (XTX matrix)
XTX <- XT %*% X; XTX
# Calcluate the invers of XTX
invXTX <- solve(XTX); invXTX
#Multiplicates the X trasnpose matrix with the Y matrix
XTY <- XT %*% Y; XTY

# Calculate estimator of regression coefficient
Beta <- invXTX %*% XTY; Beta
# And we have the model OR We can use Package instead
data <- matrix(c(y1,y2,x1,x2,x3),ncol=5,nrow=12)
data <- data.frame(data)
colnames(data) <- c("y1","y2","x1","x2","x3")
data
model <- lm(cbind(y1, y2) ~ x1 + x2 + x3 , data =data)
summary(model)

# Significance Test
# Hypotesis
# H0 = simultaneously there are no significant parameters to the model
# H1 = simultaneously there are significant parameters to the model
# Folllowed Through the steps:

# YBAR Matrix
YBAR <- colMeans(Y); YBAR
YBAR <- matrix(c(YBAR),ncol=1,nrow=2); YBAR
#sample size
n <- 12
# E Matrix and determinant E Matrix
E <- t(Y) %*% Y - t(Beta)%*%(t(X)$&$Y); E
detE <- det(E); detE
#Calculate matrix E + H and Determinant EH Matrix
EH <- (t(Y)%*%Y) - (n*(YBAR%*%t(YBAR))); EH
detEH <- det(EH); detEH
# Calculate Wilks Lambda
WilksLambda <= detE/detEH; WIlksLambda

# Test Hypothesis Criteria
H0 rejected if Λcalculate ≤ Λ𝛼,𝑚(𝑑𝑓1,𝑑𝑓2) dengan df1=r dan df2 =n-r-1

# If we want to predict model, we can use the model we'e 
# define using package
predict <- predict(model); predict

# There are some assumptions that need to be fulfilled by the model so
# the model can be used corretly

# Normality Assumption
# Hypotesis
# H0 = Residue fulfil normal distribution
# H1 = Residue doesn't fulfil normal distribution
# Folllowed Through the steps:

##QQ Plot Mahalanobis
y <- as.matrix(residu)
z <- t(y)
mu <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- sort(mahalanobis(y, mu, cov))
j <- chisq(ppoints(n), df=p)
qqplot(j, d, main="QQ-Plot", ylab="Mahalanobis Distance")
abline(0,1)
##OR
library(mvnormtest)
mshapiro.test(z)

# Test Hypothesis Criteria
H0 rejected if p – value < α

#Linearity Assumption
# Hypotesis
# H0 = Model is linear
# H1 = Model is not linear
# Folllowed Through the steps:

library(lmtest)
resettest(model)

# Test Hypothesis Criteria
H0 rejected if p – value < α

#Non-Autocorrelation Assumption
# Hypotesis
# H0 = It non-Autocorrelation
# H1 = It has autocorrelation

dwtest(model)

# Test Hypothesis Criteria
H0 rejected if p – value < α

#Homoscedasticity Assumption
# Hypotesis
# H0 = It Homoscedasticity
# H1 = It Heteroscedasticity

bptest(model)

# Test Hypothesis Criteria
H0 rejected if p – value < α
































 
