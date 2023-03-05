# Question 2b
n <- 5
x <- c(21.72, 14.65, 50.42, 28.78, 11.23)
mle <- n / sum(log(x))
mle

# Question 2c
log_likelihood <- function(par, dat){
  result <- -1 * sum(log(par/dat^(par+1)))
  return (result)
}

MLE <- optim(par = 1, fn = log_likelihood, method="BFGS", hessian = TRUE, dat = x)
MLE

# Question 2d
se <- sqrt(diag(solve(MLE$hessian)))
se

CI_lower <- MLE$par - qnorm(1-(0.05/2)) * se
CI_upper <- MLE$par + qnorm(1-(0.05/2)) * se
CI_lower
CI_upper