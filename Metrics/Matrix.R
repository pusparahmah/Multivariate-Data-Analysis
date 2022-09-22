#Here we're using matrix operation

data <- read.csv(file.choose(), header=T, sep=";")
head(data)
summary(data)

#Define matrix
A <- as.matrix(data[1:4], nrow=4, ncol=4)
B <- as.matrix(data[5:8], nrow=4, ncol=4)
C <- as.matrix(data[9:12], nrow=4, ncol=4)

#Matrix Operation

##Matrix Addition
A+B
A+C
B+C
A+B+C

##Matrix Subtraction
A-B
B-C
A-C
A-B-C

##Matrix Multiplication
A%*%B
A%*%C
B%*%C
A%*%B%*%C

##Transpose Matrix
t(A)
t(B)
t(C)

##Matrix Invers
solve(A)
solve(B)
solve(C)

##Define Determinant
prod(svd(A)$d)
prod(svd(B)$d)
prod(svd(C)$d)

##Finding Eigen Value and Eigen Vector
eigen(A)
eigen(B)
eigen(C)
