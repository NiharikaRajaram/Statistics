# sample size
size=100000
# defining the probability density function
lt<-function(x){(0.2*exp(-0.1*x)-0.2*exp(-0.2*x))}
# picking 100000 random points
tenk <- replicate(size, max(rexp(n=1, rate=0.1), rexp(n=1, rate = 0.1)))
# plotting a histogram of the randomly generated points
hist(tenk, probability=TRUE)
# adding a curve to the histogram
curve(lt, add=TRUE)
# E(T)
mean(tenk)
# probability of lifetime of sattelite being more than 15 years
sum(tenk > 15)/size