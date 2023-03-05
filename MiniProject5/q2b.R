zcimatrix <- matrix(c(zprop(5,0.01), zprop(10,0.01), zprop(30,0.01), zprop(100,0.01),
                        zprop(5,0.1), zprop(10,0.1), zprop(30,0.1), zprop(100,0.1), zprop(5,1), zprop(10,1),
                        zprop(30,1), zprop(100,1), zprop(5,10), zprop(10,10), zprop(30,10), zprop(100, 10)), nrow=4,
                      ncol=4)

bcimatrix <- matrix(c(bprop(5,0.01), bprop(10,0.01), bprop(30,0.01), bprop(100,0.01),
                        bprop(5,0.1), bprop(10,0.1), bprop(30,0.1), bprop(100,0.1), bprop(5,1), bprop(10,1),
                        bprop(30,1), bprop(100,1), bprop(5,10), bprop(10,10), bprop(30,10), bprop(100, 10)),
                      nrow=4, ncol=4)
# Drawing graphs
#1
plot(c(5,10,30,100), zcimatrix[,1], main="L=0.01", xlab='n', ylab='Proportions', col='red',
       type='b', xlim=c(1,100), ylim=c(0,1)) + lines(c(5,10,30,100), bcimatrix[,1], col='blue',type='b')

#2
plot(c(5,10,30,100), zcimatrix[,2], main="L=0.1", xlab='n', ylab='Proportions', col='red',
       type='b', xlim=c(1,100), ylim=c(0,1)) + lines(c(5,10,30,100), bcimatrix[,2], col='blue',type='b')

#3
plot(c(5,10,30,100), zcimatrix[,3], main="L=1", xlab='n', ylab='Proportions', col='red',
       type='b', xlim=c(1,100), ylim=c(0,1)) + lines(c(5,10,30,100), bcimatrix[,3], col='blue',type='b')

#4
plot(c(5,10,30,100), zcimatrix[,4], main="L=10", xlab='n', ylab='Proportions', col='red',
       type='b', xlim=c(1,100), ylim=c(0,1)) + lines(c(5,10,30,100), bcimatrix[,4], col='blue',type='b')

#5
plot(c(0.01,0.1,1,10), zcimatrix[1,], main="N=5", xlab='Lambda', ylab='Proportions',
       col='red', type='b', xlim=c(0.01,10), ylim=c(0,1)) + lines(c(0.01,0.1,1,10), bcimatrix[1,], col='blue',type='b')

#6
plot(c(0.01,0.1,1,10), zcimatrix[2,], main="N=10", xlab='Lambda', ylab='Proportions',
       col='red', type='b', xlim=c(0.01,10), ylim=c(0,1)) + lines(c(0.01,0.1,1,10), bcimatrix[2,], col='blue',type='b')

#7
plot(c(0.01,0.1,1,10), zcimatrix[3,], main="N=30", xlab='Lambda', ylab='Proportions',
       col='red', type='b', xlim=c(0.01,10), ylim=c(0,1)) + lines(c(0.01,0.1,1,10), bcimatrix[3,], col='blue',type='b')

#8
plot(c(0.01,0.1,1,10), zcimatrix[4,], main="N=100", xlab='Lambda', ylab='Proportions',
       col='red', type='b', xlim=c(0.01,10), ylim=c(0,1)) + lines(c(0.01,0.1,1,10), bcimatrix[4,], col='blue',type='b')