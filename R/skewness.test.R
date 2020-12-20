#' @title Skewness test
#' @description Compute the empirical estimation of Type I error in skewness normality test
#' @param data the data given as a random variable from N(2, 1) with sample size n=50
#' @param alpha the significance level
#' @return the empirical estimation of Type I error
#' @examples
#' \dontrun{
#' data <- rnorm(n, 2, 1)
#' skewness.test(data, alpha)
#' }
#' @importFrom stats qnorm rnorm var
#' @export
skewness.test <- function(data, alpha){ #alpha: significance level
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

