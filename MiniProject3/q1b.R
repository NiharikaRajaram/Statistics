n_s <- function(n, theta){
  data = runif(n,0,theta)
  mom = 2*mean(data)
  mle = max(data)
  return (c(mle, mom))
}

comp <- function(n, theta){
  estimate <- replicate(1000, n_s(n, theta))
  mse <- (estimate - theta) ^2
  mse.mle = mean(mse[c(TRUE, FALSE)])
  mse.mom = mean(mse[c(FALSE, TRUE)])
  return(c(mse.mle, mse.mom))
}

res11 = comp(1,1)
res11
res15 = comp(1,5)
res15
res150 = comp(1,50)
res150
res1100 = comp(1, 100)
res1100
res21 = comp(2,1)
res21
res25 = comp(2,5)
res25
res250 = comp(2,50)
res250
res2100 = comp(2,100)
res2100
res31 = comp(3,1)
res31
res35 = comp(3,5)
res35
res350 = comp(3,50)
res350
res3100 = comp(3,100)
res3100
res51 = comp(5,1)
res51
res55 = comp(5,5)
res55
res550 = comp(5,50)
res550
res5100 = comp(5,100)
res5100
res101 = comp(10,1)
res101
res105 = comp(10,5)
res105
res1050 = comp(10,50)
res1050
res10100 = comp(10,100)
res10100
res301 = comp(30,1)
res301
res305 = comp(30,5)
res305
res3050 = comp(30,50)
res3050
res30100 = comp(30,100)
res30100

par = (mfrow = c(2,2))

n = c(1,2,3,5,10,30)

MLE_1 <- c(res11[1], res21[1], res31[1], res51[1], res101[1], res301[1])
MLE_5 <- c(res15[1], res25[1], res35[1], res55[1], res105[1], res305[1])
MLE_50 <- c(res150[1], res250[1], res350[1], res550[1], res1050[1], res3050[1])
MLE_100 <- c(res1100[1], res2100[1], res3100[1], res5100[1], res10100[1], res30100[1])

MOM_1 <- c(res11[2], res21[2], res31[2], res51[2], res101[2], res301[2])
MOM_5 <- c(res15[2], res25[2], res35[2], res55[2], res105[2], res305[2])
MOM_50 <- c(res150[2], res250[2], res350[2], res550[2], res1050[2], res3050[2])
MOM_100 <- c(res1100[2], res2100[2], res3100[2], res5100[2], res10100[2], res30100[2])

plot(n, MLE_1, xlab="Sample Size", ylab="MSE", type="b", col="red", main="Theta = 1")
lines(n, MOM_1, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')


plot(n, MLE_5, xlab="Sample Size", ylab="MSE", type="b", col="red", main="Theta = 5")
lines(n, MOM_5, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')


plot(n, MLE_50, xlab="Sample Size", ylab="MSE", type="b", col="red", main="Theta = 50")
lines(n, MOM_50, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')


plot(n, MLE_100, xlab="Sample Size", ylab="MSE", type="b", col="red", main="Theta = 100")
lines(n, MOM_100, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

thetas <- c(1,5,50,100)
MLE_t_1 = c(res11[1], res15[1], res150[1], res1100[1])
MLE_t_2 = c(res21[1], res25[1], res250[1], res2100[1])
MLE_t_3 = c(res31[1], res35[1], res350[1], res3100[1])
MLE_t_5 = c(res51[1], res55[1], res550[1], res5100[1])
MLE_t_10 = c(res101[1], res105[1], res1050[1], res10100[1])
MLE_t_30 = c(res301[1], res305[1], res3050[1], res30100[1])

MOM_t_1 = c(res11[2], res15[2], res150[2], res1100[2])
MOM_t_2 = c(res21[2], res25[2], res250[2], res2100[2])
MOM_t_3 = c(res31[2], res35[2], res350[2], res3100[2])
MOM_t_5 = c(res51[2], res55[2], res550[2], res5100[2])
MOM_t_10 = c(res101[2], res105[2], res1050[2], res10100[2])
MOM_t_30 = c(res301[2], res305[2], res3050[2], res30100[2])

plot(thetas, MLE_t_1, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n = 1")
lines(thetas, MOM_t_1, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

plot(thetas, MLE_t_2, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n = 2")
lines(thetas, MOM_t_2, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

plot(thetas, MLE_t_3, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n = 3")
lines(thetas, MOM_t_3, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

plot(thetas, MLE_t_5, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n = 5")
lines(thetas, MOM_t_5, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

plot(thetas, MLE_t_10, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n = 10")
lines(thetas, MOM_t_10, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')

plot(thetas, MLE_t_30, xlab="Sample Size", ylab="MSE", type="b", col="red", main="n =30")
lines(thetas, MOM_t_30, type="b", col="blue")
legend("topright", legend=c("MLE", "MOM"), col=c('red', 'blue'), text.col = c('black', 'black'), lty=1, pch=1, inset=0.01, ncol=1, cex=0.6, bty='n')