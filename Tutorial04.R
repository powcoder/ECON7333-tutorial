# This is only for information
# this is how the data were generated
# n = 1000
# x = 1+rnorm(n)
# a = -5
# b = +2

# u = runif(n)
# e = log(u/(1-u))
# y = (a + b*x + e)>0 
# y = as.numeric(y)

# data = data.frame(x,y)

# write.csv(data, file='LR1.csv', row.names = FALSE)




# Exercise 2
LR1 = read.csv(file='LR1.csv', header=TRUE)
attach(LR1)


sum(y)
plot(x,y)


# This is Pr{Y=1|X=x}
predp = function(x,b)
{
  return( 1/(1+exp(5-x*b)) )
}

# loglikelihood
loglik_logit = function(b, y, x)
{
  n = length(y)
  loglik = 0
  for (i in 1:n)
  {
    loglik = loglik + log(predp(x[i],b))*(y[i]==1) + log(1-predp(x[i],b))*(y[i]==0)
    
  }
  return(loglik)
}

# compute the loglikelihood over a grid of points
G = 1000
bg = seq(-5,5,length=G)
lg = rep(0,G)
for (g in 1:G)
{
  lg[g]=loglik_logit(bg[g],y,x)
}
plot(bg,lg,type='l')

# Which point maximizes the likelihood?
bg[which.max(lg)]

# Compare to the function glm
glm(formula = y~ x, family = "binomial")


