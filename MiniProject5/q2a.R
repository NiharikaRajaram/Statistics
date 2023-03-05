zci_check <- function(n, lambda){
  u_dis <- rexp(n, lambda)
  lb <- mean(u_dis) - qnorm(0.975)*sd(u_dis)/sqrt(n)
  ub <- mean(u_dis) + qnorm(0.975)*sd(u_dis)/sqrt(n)
  tm = 1/lambda
  if(ub > tm & lb < tm){
    return(1)
  }
  else{
    return(0)
  }
}

zprop <- function(n, lambda){
  values <- replicate(5000, zci_check(n, lambda))
  ones <- values[which(values==1)]
  return(length(ones)/5000)
}

zprop(5,0.01)

mean.star <- function(n, lambda){
  u.star <- rexp(n, lambda)
  return(mean(u.star))
}

bci_check <- function(n, lambda){
  u_dis <- rexp(n, lambda)
  tm <- 1/lambda
  lambda1 = 1/mean(u_dis)
  v <- replicate(1000, mean.star(n, lambda1))
  bound <- sort(v)[c(25, 975)]
  if(bound[2] > tm & bound[1] < tm) {
    return(1)
  }
  else{
    return(0)
  }
}

bprop <- function(n, lambda){
  values <- replicate(5000, bci_check(n,lambda))
  ones <- values[which(values==1)]
  return(length(ones)/5000)
}

bprop(5,0.01)
