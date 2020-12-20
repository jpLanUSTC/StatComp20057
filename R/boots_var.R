#' @title Bootstrap method
#' @description Compute the bootstrap estimate of variance using R
#' @param data the data given as a vector
#' @param B the number of bootstrap samples
#' @return the bootstrap estimate of variance
#' @examples
#' \dontrun{
#' data <- c(1, 2, 0, 4, -3, 7, 2)
#' boots_var(data = data, B)
#' }
#' @importFrom stats qnorm rnorm var
#' @export
boots_var <- function(data, B){
  n <- length(data) #sample size
  boot <- numeric()
  for(i in 1:B){    #B is the number of bootstrap samples
    boot[i] <- mean(sample(data, size = n, replace = TRUE))
  }
  return(var(boot))
}
