# This is only for information
# this is how the data were generated
n = 1000
x = 1+rnorm(n)
b1 = -5
b2 = +2
b3 = +1

u = runif(n)
e = log(u/(1-u))
y = (b1 + b2*x + b3*x^2 + e)>0
y = as.numeric(y)



data = data.frame(x,y)

write.csv(data, file='LR2.csv', row.names = FALSE)




# Exercise 1
LR2 = read.csv(file='LR2.csv', header=TRUE)
attach(LR2)


sum(y)
plot(x,y)


predp = function(x,b)
{
  return( 1/(1+exp(-b[1]-b[2]*x-b[3]*x^2)) )
}

# loglikelihood
loglik_logit2 = function(b, x, y)
{
  
  n = length(y)
  loglik = 0
  for (i in 1:n)
  {
    loglik = loglik + log(predp(x[i],b))*(y[i]==1) + log(1-predp(x[i],b))*(y[i]==0)
    
  }
  return(-loglik)
}

z = x^2
glmfit = glm(formula = y ~ x + z, family = "binomial")
summary(glmfit)



B = 100

b_boot = matrix(rep(0,3*B),B,3)

for (i in 1:B)
{
  ind_ = sample(n,n, replace=TRUE)
  xb = x[ind_]
  yb = y[ind_]
  
  obj = optim(c(0,0,0), loglik_logit2,x=xb,y=yb)
  #obj = optim(fn=loglik_logit, par=b, x=xb, y=yb, method  = "L-BFGS-B", lower=-5, upper=5)
  
  b_boot[i,1] = obj$par[1]
  b_boot[i,2] = obj$par[2]
  b_boot[i,3] = obj$par[3]
}
plot(b_boot[,1],type='l')
sd(b_boot[,1])

plot(b_boot[,2],type='l')
sd(b_boot[,2])

plot(b_boot[,3],type='l')
sd(b_boot[,3])

