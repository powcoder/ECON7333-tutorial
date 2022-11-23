# Monte Carlo Experiment 1
M=100000
mu = 2
sig = 0.3
x = rnorm(M, mean=mu, sd=sig)
mean(exp(x))
exp(mu + (sig^2)/2)

# Monte Carlo Experiment 2

M = 10000

x = runif(M)
y = runif(M)

plot(x,y,asp=1)
z = x^2+y^2 <=1
plot(x[z==1],y[z==1],col='red',asp=1)
points(x[z==0],y[z==0])


sum(z)/M
pi/4


# Monte Carlo Experiment 3

n = 1000
beta = 2
M = 100

bhat = rep(0,M)

for (i in 1:M)
{
  x = runif(n)
  y = x*beta + rnorm(n)
  bhat[i] = sum(x*y)/sum(x^2)
}
plot(bhat,type='l')
var(bhat)
3/n

# Bootstrap example 1

n = 1000
beta = 2
x = runif(n)
y = x*beta + rnorm(n)

B = 100

b_boot = rep(0,B)

for (i in 1:B)
{
  ind_ = sample(n,n, replace=TRUE)
  xb = x[ind_]
  yb = y[ind_]
  b_boot[i] = sum(xb*yb)/sum(xb^2)
}
plot(b_boot,type='l')
var(b_boot)
3/n


# Bootstrap example 2
n = 1000
x = 1+rnorm(n)
a = -5
b = +2

u = runif(n)
e = log(u/(1-u))
y = (a + b*x + e)>0 
y = as.numeric(y)


d = data.frame(x,y)
d[1:10,]

predp = function(x,b)
{
  return( 1/(1+exp(5-x*b)) )
}

# loglikelihood
loglik_logit = function(b, x, y)
{

  n = length(y)
  loglik = 0
  for (i in 1:n)
  {
    loglik = loglik + log(predp(x[i],b))*(y[i]==1) + log(1-predp(x[i],b))*(y[i]==0)
    
  }
  return(-loglik)
}

# compute the loglikelihood over a grid of points
G = 50
bg = seq(-5,5,length=G)
lg = rep(0,G)
for (g in 1:G)
{
  lg[g]=loglik_logit(bg[g],x,y)
}
plot(bg,lg,type='l')

# Which point maximizes the likelihood?
bg[which.min(lg)]

# Compare to the function glm
glmfit = glm(formula = y~ x, family = "binomial")
summary(glmfit)



B = 1000

b_boot = rep(0,B)

for (i in 1:B)
{
  ind_ = sample(n,n, replace=TRUE)
  xb = x[ind_]
  yb = y[ind_]
  
  
  obj = optimize(loglik_logit,c(-1,5), x=xb, y=yb)
  
  b_boot[i] = obj$minimum
}
plot(b_boot,type='l')
sd(b_boot)






##############################
# estimating both parameters
##############################


predp = function(x,b)
{
  return( 1/(1+exp(-b[1]-x*b[2])) )
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

glmfit = glm(formula = y~ x, family = "binomial")
summary(glmfit)



B = 100

b_boot = matrix(rep(0,2*B),B,2)

for (i in 1:B)
{
  ind_ = sample(n,n, replace=TRUE)
  xb = x[ind_]
  yb = y[ind_]
  
  obj = optim(c(0,0), loglik_logit2,x=xb,y=yb)
  #obj = optim(fn=loglik_logit, par=b, x=xb, y=yb, method  = "L-BFGS-B", lower=-5, upper=5)
  
  b_boot[i,1] = obj$par[1]
  b_boot[i,2] = obj$par[2]
}
plot(b_boot[,2],type='l')
sd(b_boot[,2])

plot(b_boot[,1],type='l')
sd(b_boot[,1])

