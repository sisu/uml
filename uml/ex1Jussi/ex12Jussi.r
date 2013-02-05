## Does PC analysis for the first and the fourth dataset, writes the PCs on a file

data <- read.table("data1")

# Center data:
data <- as.matrix(data)
data <- t(apply(data,1,"-", colMeans(data)))
data <- data.frame(data)

attach(data)
cov1 <- cov(cbind(X1,Y1))
cov4 <- cov(cbind(X4,Y4))
eig1 <- eigen(cov1, symmetric = T)
eig4 <- eigen(cov4, symmetric = T)
detach(data)

PC <- matrix(ncol = 4, nrow = 2)


attach(eig1)
index = which(values == max(values))
PC[,1] = vectors[,index]
index = which(values == max(values[-index]))
PC[,2] = vectors[,index]
detach(eig1)
attach(eig4)
index = which(values == max(values))
PC[,3] = vectors[,index]
index = which(values == max(values[-index]))
PC[,4] = vectors[,index]
detach(eig4)

rm(eig1,eig4,cov1,cov4,index)
colnames(PC) <- c("PC11", "PC12", "PC21", "PC22")

# plotting:
par(mfrow = c(1,2))
par(pty = 's')
par(mar=c(0,0,0,0)+2)
attach(data)

plot(X1,Y1, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA, asp = 1)
x <- c(-PC[1,1]*1000, PC[1,1]*1000)
y <- c(-PC[2,1]*1000, PC[2,1]*1000)
points(x, y , type = 'l', col = 'red')
x <- c(-PC[1,2]*1000, PC[1,2]*1000)
y <- c(-PC[2,2]*1000, PC[2,2]*1000)
points(x, y , type = 'l', col = 'red')

plot(X4,Y4, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA, asp = 1)
x <- c(-PC[1,3]*1000, PC[1,3]*1000)
y <- c(-PC[2,3]*1000, PC[2,3]*1000)
points(x, y , type = 'l', col = 'red')
x <- c(-PC[1,4]*1000, PC[1,4]*1000)
y <- c(-PC[2,4]*1000, PC[2,4]*1000)
points(x, y , type = 'l', col = 'red')

detach(data)
rm(x,y, PC)
