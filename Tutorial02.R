#1.
# This code generate a sample of size n from the uniform distribution 
# Using that variable as input, it generates an output y where the true model 
# is a linear regression model
# y = 2.1 - 0.9*x + e
# where e has distribution N(0,1)

n = 100
a = 2.1
b = -0.9
x = runif(n)
y = b*x + rnorm(n, mean=a)  


#2.
# first: create the matrix of regressors
X = matrix( c(rep(1,n) , x ), n, 2)
#y = matrix(y, n, 1)

bhat = solve(t(X)%*%X)%*%t(X)%*%y

#3.
lm(y ~ x)
