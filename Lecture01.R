
x=seq (1 ,10)
x = 1:10
class(x)

A=matrix (1:16 ,4 ,4)

x1 = matrix( c(1,2,0,1), 2, 2)
x2 = matrix( c(1,1,1,0), 2, 2)
x1 + x2
x1*x2
x1 %*% x2

# matrix inverse
solve(x1)
solve(x1)%*%x1

x = rnorm(100)
y = 1+3*x + rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

hist(y, nclass=20)
plot(y, type='l')
plot(x,y)
