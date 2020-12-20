## -----------------------------------------------------------------------------
boots_var <- function(data, B){
   n <- length(data)      #sample size
  boot <- numeric()
  for(i in 1:B){
    boot[i] <- mean(sample(data, size = n, replace = TRUE))
  }
  return(var(boot))
}

## -----------------------------------------------------------------------------
skewness.test <- function(data, alpha){
  n <- length(data)
  cv <- qnorm(1 - alpha/2, 0, sqrt((6*(n - 2)) / ((n + 1)*(n + 3))))
sk <- function(x) {
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return(m3 / m2^1.5)
}
 m <- 10000
 sktests <- numeric(m)
 for(j in 1:m){
    x <- rnorm(n)
    # test decision is 1 (reject) or 0 (can not reject)
    sktests[j] <- as.integer(abs(sk(x)) >= cv)
 }
 p.reject <- mean(sktests)
 return(p.reject)
}

