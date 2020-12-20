## ----iris---------------------------------------------------------------------
data("iris")
x = iris[,1]
plot(x,type="l",xlab="n",ylab="Sepal.Length")

## ----table--------------------------------------------------------------------
x = iris[,1]
y = iris[,2]
x1 = x[1:5]
y1 = y[1:5]
t <- cbind(x1,y1)
xtabs(data = t)

## -----------------------------------------------------------------------------
set.seed(1111)
n <- 1000
u <- runif(n)
x <-2/sqrt(1-u) #F(x)=1-(2/x)^2, x>=2>0
hist(x,prob = TRUE, main = expression(f(x)==8/x^3))
y <- seq(2, 100, .1)
lines(y, 8/y^3)

## -----------------------------------------------------------------------------
set.seed(1111)
n <- 1e4
j <- k <- 0
u <- numeric(n)
while (k < n) {
  u2 <- runif(1, -1, 1); u3 <- runif(1, -1, 1)
  j <- j + 1
  u1 <- runif(1, -1, 1) # random variate from g(.)
  if (abs(u3) >= abs(u2) & abs(u3) >= abs(u1)){
    #we accept u2
    k <- k + 1
    u[k] <- u2
  }
  else{
    #we accept u3
    k <- k + 1
    u[k] <- u3
  }
}

hist(u,prob = TRUE, main = expression(f(x)==3/4*(1-x^2)))
t <- seq(-1, 1, .01)
lines(t, 3/4*(1-t^2))


## -----------------------------------------------------------------------------
set.seed(1111)
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta)
y <- rexp(n, lambda) # generate random observations from mixtrue with r = 4, beta = 2
hist(y,prob = TRUE, main = expression(f(y) == 2^6/(2+y)^5))
z <- seq(0, 10, .01)
lines(z, 2^6/(2+z)^5)

## -----------------------------------------------------------------------------
set.seed(11111)
n <- 1e4
x <- runif(n, min = 0, max = pi/3)
theta.hat <- mean(sin(x)) * pi/3
print(c(theta.hat, 1/2))


## -----------------------------------------------------------------------------
set.seed(11111)
n <- 2e4
x <- runif(n, min = 0, max = 1)
theta.hat <- mean(exp(x))
var_theta.hat <- var(exp(x))/n
n1 <- n/2
x1 <- x[1: n1]
x2 <- 1 - x1
theta.hat1 <- mean(exp(x1))
theta.hat2 <- mean(exp(x2))
theta.hat3 <- (theta.hat1 + theta.hat2)/2
var_theta.hat3 <- var((exp(x1) + exp(x2))/2)/n
ep_reduction <- (var_theta.hat - var_theta.hat3)/var_theta.hat
var_theta.th <- (3 * exp(2) - 6 * exp(1) + 1)/(4*n)
reduction <- (var_theta.th - var_theta.hat)/var_theta.th
cat(" The Monte Carlo simulation of theta is:",                             
"The antithetic variate approach of theta is:", theta.hat3, '\n',
"The variance of using Monte Carlo is:", var_theta.hat, '\n',
"The variance of using antitetic variate is:", var_theta.hat3, '\n',
"The percent of empirical estimate of variance reduction is:", ep_reduction, '\n',"The percent of theoretical estimate of variance reduction is:", reduction)


## -----------------------------------------------------------------------------
 t <- seq(0, 1, .01)
    w <- 2
    g <- exp(-1/(2 * t^2)) / (sqrt(2 * pi) * t^4)
    f1 <- (4 / pi) / (1 + t^2)
    f2 <- exp(- t) / (1 - exp(-1))
    gs <- c(expression(g(t)==exp(-1/(2 * t^2)) / (sqrt(2 * pi) * t^4)),
            expression(f[1](t)==(4 / pi) / (1 + t^2)),
            expression(f[2](t)==exp(- t) / (1 - exp(-1))))
    #for color change lty to col
    par(mfrow=c(1,1.5))
    #figure (a)
    plot(t, g, type = "l", ylab = "",
         ylim = c(0,2), lwd = w,col=1,main='(A)')
    lines(t, f1, lty = 2, lwd = w,col=2)
    lines(t, f2, lty = 3, lwd = w,col=3)
    legend("topright", legend = gs,
           lty = 1:3, lwd = w, inset = 0.02,col=1:3)

    #figure (b)
    plot(t, g/f1, type = "l", ylab = "",
        ylim = c(0,3.2), lwd = w, lty = 2,col=2,main='(B)')
    lines(t, g/f2, lty = 3, lwd = w,col=3)
    legend("topright", legend = gs[-1],
           lty = 2:3, lwd = w, inset = 0.02,col=2:3)
set.seed(11111)
m <- 10000
est <- var <- numeric(2)
g <- function(t) {
  exp(-1/(2 * t^2)) / (sqrt(2 * pi) * t^4) * (t > 0) * (t < 1)
  }
u <- runif(m) #using f1
t <- tan((pi * u) / 4)
g_f1 <- g(t) / ((4 / pi) / (1 + t^2))
est[1] <- mean(g_f1)
var[1] <- var(g_f1)
u <- runif(m) #using f2
t <- -log(1 - (1 -exp(-1)) * u)
g_f2 <- g(t) / (exp(- t) / (1 - exp(-1)))
est[2] <- mean(g_f2)
var[2] <- var(g_f2)
print(c(var))

## -----------------------------------------------------------------------------
set.seed(111111)
M <- 1e4
k <- 5
r <- M/k
N <- 50
T2 <- numeric(k)
est <- matrix(0, N, 2)
g <- function(t) {
  exp(-1/(2 * t^2)) / (sqrt(2 * pi) * t^4) * (t > 0) * (t < 1)
}
for (i in 1:N) {
  est[i, 1] <- mean(g(runif(M)))
  for(j in 1:k)T2[j] <- mean(g(tan(atan((j-1)/k) + (atan(j/k)-atan((j-1)/k)) * runif(M/k, (j - 1) / k, j / k))))
  est[i, 2] <- mean(T2)
}
theta_hat <- apply(est, 2, mean)
theta_hat1 <- theta_hat[2]
set.seed(111112)
M <- 5000
t2 <- runif(M)
theta_hat2 <- mean((g(t2) + g(1 - t2)) /2)
theta_hat1
theta_hat2

## -----------------------------------------------------------------------------
set.seed(111111)
n <- 20
x <- rlnorm(n, meanlog = 0, sdlog = 1)
y <- log(x)
mean_y <- mean(y)
qt1 <- qt(0.975, df = n - 1)
sd1 <- sd(y)
cl_lower <- mean_y - qt1 * (sd1 / n)
cl_upper <- mean_y + qt1 * (sd1 / n)
print(c(cl_lower, cl_upper))

## -----------------------------------------------------------------------------
set.seed(111111)
n <- 20
x <- rchisq(n, df = 2)
mean_x <- mean(x)
qchi1 <- qchisq(0.025, df = 40)
qchi2 <- qchisq(0.975, df = 40)
cl_lower <- qchi1 / n
cl_upper <- qchi2 / n
print(c(cl_lower, cl_upper))

## -----------------------------------------------------------------------------
set.seed(111111)
n <- 20
x <- rchisq(n, df = 2)
mean_x <- mean(x)
qt2 <- qt(0.975, df = n - 1)
sd2 <- sd(x)
cl_lower <- mean_x - qt2 * (sd2 / n)
cl_upper <- mean_x + qt2 * (sd2 / n)
print(c(cl_lower, cl_upper))

## -----------------------------------------------------------------------------

set.seed(11111)
n <- 30 #sample size
alpha1 <- 0.05
cv <- qnorm(1 - alpha1/2, 0, sqrt((6*(n - 2)) / ((n + 1)*(n + 3)))) 
sk <- function(x) {
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return(m3 / m2^1.5)
}
m <- 2500
alpha <- c(seq(1, 100, 1))
N <- length(alpha)
# beta
pwr_beta <- numeric(N)
for (i in 1:N) {
  sktests <- numeric(m) 
  for (j in 1:m) {
    x <- rbeta(n, alpha[i], alpha[i])
    sktests[j] <- as.integer(abs(sk(x)) >= cv)
  }
  pwr_beta[i] <- mean(sktests)
}
#t(nu)
nu <- c(seq(1, 100, 1))
N1 <- length(nu)
pwr_t <- numeric(N1)
for (i in 1:N1) {
  sktests <- numeric(m) 
  for (j in 1:m) {
    x <- rt(n, df = nu[i])
    sktests[j] <- as.integer(abs(sk(x)) >= cv)
  }
  pwr_t[i] <- mean(sktests)
}
library(ggplot2)
#plot power of beta(alpha, alpha)
plot(alpha, pwr_beta, type = "b", xlim = c(0, 100), ylim = c(0, 1), col = "red" ,xlab = "alpha", ylab = "power")
lines(alpha, pwr_t, type = "b", xlim = c(0, 100), ylim = c(0, 1), col = "blue", title(main = "Power beta vs t"))
legend("topright", c("beta", "t"), lty = c(1, 1), pch = c(1, 1), col = c("red", "blue"))


## -----------------------------------------------------------------------------
set.seed(111111)
n1 <- n2 <- 20
sigma1 <- 1
sigma2 <- 1.5
cout5test <- function(x, y){
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do notreject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
m <- 1e4
  x <- rnorm(n1, 0, sd = sigma1)
  y <- rnorm(n2, 0, sd = sigma2)
tests <- replicate(m , expr = {
  x <- rnorm(n1, 0, sd = sigma1)
  y <- rnorm(n2, 0, sd = sigma2)
  X <- x - mean(x)
  Y <- y - mean(y)
  cout5test(x, y)
})
mean(tests)
# F test with alpha = 0.055
# normality test
shapiro.test(x)
shapiro.test(y)# if p_value < 0.1, we need generate new sample
alpha <- 0.055
var.test(x, y, ratio = 1, alternative = c("two.sided"), conf.level = 1 - alpha)
#p_value = 0.398

# for moderate, large sample
set.seed(1234)
N1 <- N2 <- 100
c5test <- numeric(2)
  x1 <- rnorm(N1, 0, sd = sigma1)
  y1 <- rnorm(N2, 0, sd = sigma2)
tests <- replicate(m , expr = {
  x1 <- rnorm(N1, 0, sd = sigma1)
  y1 <- rnorm(N2, 0, sd = sigma2)
  X <- x1 - mean(x)
  Y <- y1 - mean(y)
  cout5test(x1, y1)
})
  mean(tests)
# 0.85
# F test with alpha = 0.055
# normality test
shapiro.test(x1)
shapiro.test(y1)# if p_value < 0.01, we need generate new sample
alpha <- 0.055
var.test(x1, y1, ratio = 1, alternative = c("two.sided"), conf.level = 1 - alpha)
#p_value = 0.398

#large sample
set.seed(12345)
N1 <- N2 <- 500
  x2 <- rnorm(N1, 0, sd = sigma1)
  y2 <- rnorm(N2, 0, sd = sigma2)
tests <- replicate(m , expr = {
  x2 <- rnorm(N1, 0, sd = sigma1)
  y2 <- rnorm(N2, 0, sd = sigma2)
  X <- x2 - mean(x)
  Y <- y2 - mean(y)
  cout5test(x2, y2)
})
  mean(tests)
# 0.85
# F test with alpha = 0.055
# normality test
shapiro.test(x2)
shapiro.test(y2)# if p_value < 0.01, we need generate new sample
alpha <- 0.055
var.test(x2, y2, ratio = 1, alternative = c("two.sided"), conf.level = 1 - alpha)

## -----------------------------------------------------------------------------

library(MASS)
set.seed(111111)
n <- 10
alpha <- 0.05
Sigma <- matrix(c(1, 0, 0, 1), 2, 2)
x1 <- mvrnorm(n, rep(0, 2), Sigma)
x2 <- mvrnorm(n, rep(0, 2), Sigma)
msk <- function(x1, x2){
  x1bar <-colMeans(x1)
  x2bar <- colMeans(x2)
  xbar <- (x1bar + x2bar) /2
  t_x1 <- t(x1 - xbar)
  x2_ <-x2 - xbar
  Sigma_hat <- (var(x1) + var(x2)) / 2
  Sigma_hat_inv <- solve(Sigma_hat)
  z <- numeric(length(x1) / 2)
  t <- length(x1) / 2
  for (i in 1:t) {
    z[i] <- (t_x1[,i] %*% Sigma_hat_inv %*% (x2_[i,]))^3 / n^2
  }
  return(sum(z))
}
p.reject <- numeric(length(n))
cv <- qchisq(1 - alpha/2, df = 1)
m <- 1e4
  for (i in 1:n) {
  sktests <- numeric(m) 
  for (j in 1:m) {
    x1 <- mvrnorm(n, rep(0, 2), Sigma)
    x2 <- mvrnorm(n, rep(0, 2), Sigma)
    sktests[j] <- as.integer(abs(msk(x1, x2)) >= cv)
  }
  p.reject[i] <- mean(sktests)

  }

p.reject


## -----------------------------------------------------------------------------
set.seed(111111)
library(bootstrap)
data("law")
n <- nrow(law) #sample size
cor_law <- cor(law$LSAT, law$GPA)
# Set up the jackknife
cor_law_jack <- numeric(n)
for (i in 1:n) {
  cor_law_jack[i] <- cor(law$LSAT[-i], law$GPA[-i])
}
# Estimate the bias
bias_cor_law <- (n - 1) * (mean(cor_law_jack) - cor_law)
cat('Jackknife estimate of the bias of the correlation statistic is:', bias_cor_law,'\n')
# Estimate the standard error
se_cor_law <- sqrt((n - 1) * mean((cor_law_jack - mean(cor_law_jack))^{2}))
cat('Jackknife estimate of the standard error of the correlation statistic is:', se_cor_law, '\n')

## -----------------------------------------------------------------------------
set.seed(111111)
library(boot)
data("aircondit")
B <- 500 # number of replicates
func <- function(x, i){
  y <- x[i,1]
  mean(y)
}
boot_obj <- boot(data = aircondit, statistic = func, R = B)
round(c(original = boot_obj$t0, bias = mean(boot_obj$t) - boot_obj$t0, se = sd(boot_obj$t)), 3)
boot_ci <- boot.ci(boot_obj, conf = 0.95, type = c("norm", "basic", "perc", "bca"))
print(boot_ci)
mean(aircondit[,1])

## -----------------------------------------------------------------------------
set.seed(111111)
library(bootstrap)
data("scor")
lambda_hat <- eigen(cov(scor))$values
theta_hat <- lambda_hat[1] / sum(lambda_hat)
B <- 200
n <- nrow(scor)
# Estimate by jackknife
theta_jack <- numeric(n)
for (i in 1:n) {
  x <- scor [-i,]
  lambda <- eigen(cov(x))$values
  theta_jack[i] <- lambda[1] / sum(lambda)
}
 bias_jack <- (n - 1) * (mean(theta_jack) - theta_hat)
 se_jack <- (n - 1)/sqrt(n) * sd(theta_jack)
print(c(bias_jack, se_jack))

## -----------------------------------------------------------------------------

library(DAAG)
attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <-  lm(magnetic ~ chemical)
L2 <-  lm(magnetic ~ chemical + I(chemical ^ 2))
L3 <-  lm(log(magnetic) ~ chemical)
L4 <-  lm(log(magnetic) ~ log(chemical))
n <- length(ironslag$magnetic) / 2
e1 <- e2 <- e3 <- e4 <- numeric(n * 2)
##leave-two-out
m <- for (k in (1:n)*2) {
  y <- magnetic[-c(k-1,k)] 
  x <- chemical[-c(k-1,k)]
  
  J1 <- lm(y ~ x) 
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k-1]
  e1[k-1] <- magnetic[k-1] - yhat1#error between estimation and observation of model
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1#error between estimation and observation
  
  J2 <- lm(y ~ x + I(x^2)) 
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k-1] + J2$coef[3] * chemical[k-1]^2
  e2[k-1] <- magnetic[k-1] - yhat2#error between estimation and observation
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2#error between estimation and observation

  J3 <- lm(log(y) ~ x) 
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k-1] 
  yhat3 <- exp(logyhat3)
  e3[k-1] <- magnetic[k-1] - yhat3#error between estimation and observation
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k] 
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3#error between estimation and observation
  
  J4 <- lm(log(y) ~ log(x)) 
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k-1]) 
  yhat4 <- exp(logyhat4)
  e4[k-1] <- magnetic[k-1] - yhat4#error between estimation and observation
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k]) 
  yhat4 <- exp(logyhat4)
  e4[k] <- magnetic[k] - yhat4#error between estimation and observation
}
print(c(mean(e1 ^ 2), mean(e2 ^ 2), mean(e3 ^ 2), mean(e4 ^ 2)))

## -----------------------------------------------------------------------------
set.seed(111111)
n1 <- 20
n2 <- 30
sigma1 <- 1
sigma2 <- 1
cout5test <- function(x, y){
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do notreject H0)
  return(as.integer((outx > 4)|| (outy>7)))
}
R <- 1000
N <- n1 + n2
  x <- rnorm(n1, 0, sd = sigma1)
  y <- rnorm(n2, 0, sd = sigma2)
  z <- c(x, y)
K <- 1:N
n_min <- min(n1, n2)
reps <- numeric(R)
for (i in 1:R) {
  k <- sample(K, size = n_min, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k]
  reps[i] <- cout5test(x1, y1)
}
mean(reps)

## -----------------------------------------------------------------------------
library(RANN)
library(energy)
library(boot)
library(Ball)
set.seed(111111)
nn.test <- function(x,y){
z <- c(x, y)
o <- rep(0, length(z))
z <- as.data.frame(cbind(z, o))
Tn3 <- function(z, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  o <- rep(0, nrow(z))
  z <- as.data.frame(cbind(z, o))
  NN <- nn2(z, k=3)
  b1 <- NN$nn.idx[1:n1, ]
  b2 <- NN$nn.idx[(n1+1):n, ]
  i1 <- sum(b1 < n1 + .5)
  i2 <- sum(b2 > n1 + .5)
  return((i1 + i2) / (3 * n))
}
N <- c(length(x), length(y))
boot_obj <- boot(data = z, statistic = Tn3, sim = "permutation", R = 999, sizes = N)
t_b <- c(boot_obj$t, boot_obj$t0)
mean(t_b >= boot_obj$t0)
}
N0 <- length(x)+length(y)
energy.test <- function(x,y,R= N0){
  z <- c(x, y)
  o <- rep(0, length(z))
  z <- as.data.frame(cbind(z, o))
  N <- c(length(x), length(y))
  eqdist.etest(z, sizes = N, R=R)$p
}

#1. Unequal variances and equal expectations
matrix <- matrix(0,10,3)
for(i in 1:10){
  x <- rnorm(100, mean = 0, sd = 1)
  y <- rnorm(100, mean = 0, sd = 1) * (1 + i/10)# yi = (1 + i/10)x
  matrix[i,] <- c(nn.test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
}
plot(matrix[,1], type = "n", ylim = c(0,0.5), main = "unequal variance")
for(i in 1:3)points(matrix[,i], col=i+1)
for(i in 1:3)points(matrix[,i], col=i+1, type='l')

## -----------------------------------------------------------------------------
#2. Unequal variances and unequal expectations
set.seed(111111)
matrix <- matrix(0,10,3)
for(i in 1:10){
  x <- rnorm(100,mean = i/10, sd = 1)
  y <- rnorm(100,mean = i/10, sd = 1) * (1+i/10)# y, x unequal
  matrix[i,] <- c(nn.test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
}
plot(matrix[,1],type='n',ylim=c(0,0.7),main="unequal variances and unequal expectations")
for(i in 1:3)points(matrix[,i],col=i+1)
for(i in 1:3)points(matrix[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
#3_1 Non-normal distributions: t distribution with 1 df (heavy-tailed distribution)
set.seed(111111)
matrix <- matrix(0,10,3)
for(i in 1:10){
  x <- rt(1000,df=1)
  y <- rt(1000,df=1+i/10)
  matrix[i,]=c(nn.test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
}
plot(matrix[,1],type='n',ylim=c(0,0.7),main="heavy-tail")
for(i in 1:3)points(matrix[,i],col=i+1)
for(i in 1:3)points(matrix[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
#3_2 bimodel distribution (mixture of two normal distributions)
set.seed(111111)
matrix <- matrix(0,10,3)
for(i in 1:10){
  x <- rnorm(500)
  y <- ifelse(runif(500)<i/11,rnorm(500,sd=0.3),rnorm(500,sd=1.38))
  matrix[i,]=c(nn.test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
}
plot(matrix[,1],type='n',ylim=c(0,0.5),main="bimodel")
for(i in 1:3)points(matrix[,i],col=i+1)
for(i in 1:3)points(matrix[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
#4 Unbalanced samples (say, 1 case versus 10 controls)
set.seed(111111)
matrix <- matrix(0,10,3)
for(i in 1:10){
  x <- rnorm(100/i)
  y <- rnorm(100*i,sd=1.5)
  matrix[i,]=c(nn.test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
}
plot(matrix[,1],type='n',ylim=c(0,1),main="unbalanced")
for(i in 1:3)points(matrix[,i],col=i+1)
for(i in 1:3)points(matrix[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
set.seed(111111)
dl <- function(x){ #Laplace function
        return(0.5 * exp(-abs(x)))
} 
rw.Metropolis <- function(sigma, x0, N) {
x <- numeric(N)
x[1] <- x0
u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <= dl(y) / dl(x[i - 1]))
x[i] <- y else {
x[i] <- x[i-1]
k <- k + 1
} }
return(list(x=x, k=k))
}
n <- 4 #degrees of freedom for target Student t dist.
N <- 2000
sigma <- c(.05, .5, 2, 8)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
print(c((N - rw1$k) / N, (N - rw2$k) / N, (N - rw3$k) / N, (N - rw4$k) / N))

## -----------------------------------------------------------------------------
set.seed(111111)
sigma <- 2 #parameter of distribution
k <- 4#number of chains to generate
N <- 15000#length of the chain
b <- 500#burn-in

dl<-function(x){#Laplace function
  return(0.5*exp(-abs(x)))
}

Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)#row means
  B <- n * var(psi.means)#between variance est.
  psi.w <- apply(psi, 1, "var")#within variances
  W <- mean(psi.w)#within est.
  v.hat <- W * (n - 1) / n + (B / n)#upper variance est.
  r.hat <- v.hat / W#G-R statistic
  return(r.hat)
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dl(y) / dl(x[i-1]))){
      x[i] <- y
    }
    else {
      x[i] <- x[i-1]
      k <- k + 1
      } 
    }
  return(list(x=x, k=k))
}

#generate the chains
x0 <- c(-10, -5, 5, 10)
X <- matrix(0, nrow=k, ncol=N)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma, x0[i], N)$x

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi)) 
  psi[i, ] <- psi[i, ] / (1:ncol(psi))
print(Gelman.Rubin(psi))
#plot psi for the four chains
par(mfrow=c(1.5, 1.5))
for (i in 1:k)
  plot(psi[i, (b+1):N], type="l", xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1)) #restore default

# plot the sequence of R-hat statistics
rhat <- rep(0, N)
for (j in (b+1):N) 
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2)

## -----------------------------------------------------------------------------
set.seed(111111)
eps <- .Machine$double.eps^0.25#judge whether function is near to 0
k <- c(4:25,100,500,1000)#k mentioned in the question

S <- function(k,a){
  return((1 - pt(sqrt((a^2*k) / (k+1-a^2)), df = k)) - (1 - pt(sqrt((a^2*(k-1)) / (k-a^2)), df = k-1)))
}#function of S_k(a) - S_{k-1}(a)
Root <- function(k1){
a <- seq(0.1, sqrt(k1) - 0.1,length = 3)
y <- c(S(k1,a[1]), S(k1,a[2]), S(k1,a[3]))
while(abs(y[2]) > eps) {
  if (y[1] * y[2] < 0) {
    a[3] <- a[2]
    y[3] <- y[2]
  } else {
    a[1] <- a[2]
    y[1] <- y[2]
  }
  a[2] <- (a[1] + a[3]) / 2
  y[2] <- S(k1,a[2])
}
result <-list(k1,a[2],y[2])
return(result)
}

for(i in k){#print the output of each k
  cat('k:',Root(i)[[1]],'root:',Root(i)[[2]],'value of function:',Root(i)[[3]],'\n')
  
} 

## -----------------------------------------------------------------------------
k <- 4
a <- seq(0.1, sqrt(k)-0.1, 0.01)
y0 <- numeric(length(a))
y <- (1-pt(sqrt((a^2*k) / (k+1-a^2)),df = k)) - (1 - pt(sqrt((a^2*(k-1)) / (k-a^2)), df = k-1))
plot(a,y,'l')
lines(a,y0)
cat('The root of curves when k = ',k,'is',Root(k)[[2]],'\n')
k <- 10
a <- seq(0.1, sqrt(k) - 0.1, 0.01)
y0 <- numeric(length(a))
y <- (1-pt(sqrt((a^2*k)/(k+1-a^2)),df = k))-(1-pt(sqrt((a^2*(k-1))/(k-a^2)),df=k-1))
plot(a,y,'l')
lines(a,y0)
cat('The root of curves when k = ',k,'is',Root(k)[[2]],'\n')

## -----------------------------------------------------------------------------
EM <- function(p_ini,n.obs){
  T <- .Machine$double.eps # converge time
  M <- 1e3 # the maximum ieration number
  n <- sum(n.obs)
  nA. <- n.obs[1]
  nB. <- n.obs[2]
  nOO <- n.obs[3]
  nAB <- n.obs[4]
  # assignment
  p <- q <- r <- numeric(0)
  p[1] <- p_ini[1]
  q[1] <- p_ini[2]
  r[1] <- 1-p[1]-q[1]
  t <- 1
  # calculate
  for(i in 2:M){
    p_pre <- p[i-1]
    q_pre <- q[i-1]
    r_pre <- r[i-1]
    
    nAA_t <- nA.*p_pre^2/(p_pre^2+2*p_pre*r_pre)
    nAO_t <- nA.*2*p_pre*r_pre/(p_pre^2+2*p_pre*r_pre)
    nBB_t <- nB.*q_pre^2/(q_pre^2+2*q_pre*r_pre)
    nBO_t <- nB.*2*q_pre*r_pre/(q_pre^2+2*q_pre*r_pre)
    nOO_t <- nOO
    nAB_t <- nAB
    
    p[i] <- (2*nAA_t+nAO_t+nAB_t)/2/n
    q[i] <- (2*nBB_t+nBO_t+nAB_t)/2/n
    r[i] <- (2*nOO_t+nAO_t+nBO_t)/2/n
    t <- t+1
    
    U <- abs((p[i]-p_pre)/p_pre)<=T
    V <- abs((q[i]-q_pre)/q_pre)<=T
    W <- abs((r[i]-r_pre)/r_pre)<=T
    if(U&&V&&W)
      break
  }
  list(p_mle.em=p[t],q.mle.em=q[t],r.mle.em=r[t],t=t)
}
nObs <- c(444,132,361,63)
p_Initial <- c(1/3,1/3) #initial p,q value
em.result<-EM(p_ini=p_Initial,n.obs=nObs)
print(em.result)

## -----------------------------------------------------------------------------
EM_trend <- function(p_ini, n.obs){
  T <- .Machine$double.eps # converge time
  M <- 1e3 # the maximum ieration number
  n <- sum(n.obs)
  nA. <- n.obs[1]
  nB. <- n.obs[2]
  nOO <- n.obs[3]
  nAB <- n.obs[4]
  # assignment
  p <- q <- r <- numeric(0)
  loglikelihood <- numeric(0)
  p[1] <- p_ini[1]
  q[1] <- p_ini[2]
  r[1] <- 1-p[1]-q[1]
  loglikelihood[1] <- 0
  t <- 1
  
  for(i in 2:M){
    p_pre <- p[i-1]
    q_pre <- q[i-1]
    r_pre <- r[i-1]
    
    nAA_t <- nA.*p_pre^2/(p_pre^2+2*p_pre*r_pre)
    nAO_t <- nA.*2*p_pre*r_pre/(p_pre^2+2*p_pre*r_pre)
    nBB_t <- nB.*q_pre^2/(q_pre^2+2*q_pre*r_pre)
    nBO_t <- nB.*2*q_pre*r_pre/(q_pre^2+2*q_pre*r_pre)
    nOO_t <- nOO
    nAB_t <- nAB
    
    p[i] <- (2*nAA_t+nAO_t+nAB_t)/2/n
    q[i] <- (2*nBB_t+nBO_t+nAB_t)/2/n
    r[i] <- (2*nOO_t+nAO_t+nBO_t)/2/n
    t <- t+1
    
    loglikelihood[i]=nAA_t*2*log(p[i])+nAO_t*log(2*p[i]*r[i])+nBB_t*2*log(q[i])+nBO_t*log(q[i]*r[i])+nOO_t*2*log(r[i])+nAB_t*log(2*p[i]*q[i])
    
    U <- abs((p[i]-p_pre)/p_pre)<=T
    V <- abs((q[i]-q_pre)/q_pre)<=T
    W <- abs((r[i]-r_pre)/r_pre)<=T
    if(U&&V&&W)
      break
  }
  list(p_mle.em=p[t],q.mle.em=q[t],r.mle.em=r[t],t=t,p_mle.all=p,q.mle.all=q,loglikelihoods=loglikelihood)
}
nObs <- c(444,132,361,63)
pInitial <- c(0.4,0.3) #initial p,q value
em.result <- EM_trend(p_ini=pInitial,n.obs=nObs)

par(mfrow=c(1,1.5))
plot(em.result$loglikelihoods[-1],xlab = "t",ylab = "loglikehood")


## -----------------------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

##Use loops to fit 
for (i in 1:length(formulas)) {
  re <- lm(formulas[[i]], mtcars)
  print(re)
}
## lapply() to fit
lapply(formulas, function(x) lm(data=mtcars,x))

## -----------------------------------------------------------------------------
trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)), simplify = FALSE)
set.seed(111111)
##Use sapply
p_value <- sapply(trials, function(x) x$p.value)
p_value


## -----------------------------------------------------------------------------
func <- function(data,funct,output_type){
  new <- Map(funct,data)
  vapply(new, function(x) x ,output_type)
}

##Example
func(women, mean, double(1))

## -----------------------------------------------------------------------------
set.seed(111111)
library(Rcpp)
sourceCpp("../src/MetropolisC.cpp")
N <-  2000
sigma = c(.05, .5, 2, 10)
x0 = 25
rw1new = MetropolisC(sigma[1],x0,N)
rw2new = MetropolisC(sigma[2],x0,N)
rw3new = MetropolisC(sigma[3],x0,N)
rw4new = MetropolisC(sigma[4],x0,N)
# number of candidate points rejected
Reject = cbind(rw1new$k, rw2new$k, rw3new$k, rw4new$k)
## Accept rates
Accept = round((N-Reject)/N, 4)
rownames(Accept) = "Accept rates"
colnames(Accept) = paste("sigma",sigma)
knitr::kable(Accept)
# plot
par(mfrow=c(1.5,1.5))  #display 4 graphs together
    rwnew = cbind(rw1new$x, rw2new$x, rw3new$x,  rw4new$x)
    for (j in 1:4) {
        plot(rwnew[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rwnew[,j]))
    }

## -----------------------------------------------------------------------------
set.seed(111111)
laplace_fun <-  function(x) exp(-abs(x))
rw.Metropolis <- function(sigma, x0, N){
 x = numeric(N)
 x[1] = x0
 u = runif(N)
 k = 0
 for (i in 2:N) {
  y = rnorm(1, x[i-1], sigma)
  if (u[i] <= (laplace_fun(y) / laplace_fun(x[i-1]))) x[i] = y 
  else {
  x[i] = x[i-1]
  k = k+1
  }
 }
 return(list(x = x, k = k))
}
N = 2000
sigma = c(.05, .5, 2, 10)
x0 = 25
rw1 = rw.Metropolis(sigma[1],x0,N)
rw2 = rw.Metropolis(sigma[2],x0,N)
rw3 = rw.Metropolis(sigma[3],x0,N)
rw4 = rw.Metropolis(sigma[4],x0,N)
#number of candidate points rejected
Reject = cbind(rw1$k, rw2$k, rw3$k, rw4$k)
Accept = round((N-Reject)/N,4)
rownames(Accept) = "Accept rates"
colnames(Accept) = paste("sigma",sigma)
knitr::kable(Accept)
#plot
par(mfrow=c(1.5,1.5))  #display 4 graphs together
    rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
    }


## -----------------------------------------------------------------------------
a <- c(0.05, seq(0.1, 1, 0.1), 0.95)
rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
rwnew <- cbind(rw1new$x, rw2new$x, rw3new$x,  rw4new$x)
mc <- rw[501:N,]
mcnew <- rwnew[501:N,]
q_rw <- apply(mc, 2, function(x) quantile(x,a))
q_rwnew <- apply(mcnew, 2, function(x) quantile(x,a))
qtable <- round(cbind(q_rw, q_rwnew), 3)
colnames(qtable) <- c("rw1","rw2","rw3","rw4","rw1new","rw2new","rw3new","rw4new")
qtable

#qqplot
aa <- ppoints(100)
QQ_rw3 <- quantile(rw3$x[501:N], aa)
QQ_rw3new <- quantile(rw3new$x[501:N], aa)
qqplot(QQ_rw3, QQ_rw3new, main="",xlab="rw3 quantiles",ylab="rw3new quantiles")
qqline(QQ_rw3new)

## -----------------------------------------------------------------------------
set.seed(111111)
library(microbenchmark)
ts <- microbenchmark(
  rw.Metropolis(sigma[3],x0,N),
  MetropolisC(sigma[3],x0,N))
summary(ts)[,c(1, 4)]
mean_time <- summary(ts)[,c(1, 4)][,2]
mean_time[1] / mean_time[2]
cat("the Rcpp function is", mean_time[1] / mean_time[2], "times faster than the R function on average.")

