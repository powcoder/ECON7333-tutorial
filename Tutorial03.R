# load the input data
x = read.csv("MC1.csv", header=TRUE)
x = as.matrix(x)

# sample size
n = 1000

# true parameters
b0 = -1
b1 = 5.1

#1.
# generate a sample
y = b0 +b1*x + rnorm(n)

#2. Least squares estimates
fit = lm(y~x)

b0hat = fit$coefficients[1]
b1hat = fit$coefficients[2]
shat = sd(fit$residuals)

#3. Monte Carlo experiment
# number of generated samples
M = 100

# initialize three vectors of zeros that will store the estimates from the simulation
b0m = rep(0,M)
b1m = rep(0,M)
sm = rep(0,M)
for (m in 1:M)
{
  # new sample. x is the same but the output is new for each sample
  ym = b0 +b1*x + rnorm(n)
  
  fitm =  lm(ym~x)
  
  b0m[m] = fitm$coefficients[1]
  b1m[m] = fitm$coefficients[2]
  sm[m] = sd(fitm$residuals)
}

#4.a sample averages
mean(b0m)
mean(b1m)

mean(sm)


#4.b and c
var(b0m)
var(b1m)

#4 d
cov(b0m, b1m)

# 5
plot(b0m, type='l')
plot(b1m, type='l')
plot(sm, type='l')

# 6. Comparison with the theoretical formula
X=matrix(c( rep(1,n) , x), n, 2)
(shat^2) * solve( t(X) %*% X)
