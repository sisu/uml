# Exercise 1

# create data
Sigma <- diag(c(1,1))
n <- mvrnorm(n = 5000, mu = c(0,0), Sigma = Sigma)

# transformation matrices
A1 <- matrix(c(0.4483, 2.1907,-1.6730, -1.4836), c(2,2))
A2 <- matrix(c(0, 1.7321, -1.7321, -2), c(2,2))

# transform data
y1 <- t(A1%*%t(n))
y2 <- t(A2%*%t(n))

# plotting
par(mfrow = c(1,3))
par(pty = 's')
par(mar=c(0,0,0,0)+2)
plot(n, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta')
plot(y1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta')
plot(y2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta')




# Exercise 2
whiten <- function(data){
	PCA <- prcomp(data)
	data <- (PCA$rotation)%*%t(data)
	data <- diag(1/(PCA$sdev))%*%data
	return(data)
}

# whiten data
y1 <- t(whiten(y1))
y2 <- t(whiten(y2))

# plot whitened data
par(mfrow = c(1,2))
par(pty = 's')
par(mar=c(0,0,0,0)+2)
plot(y1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta')
plot(y2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta')



# Exercise 3
alpha <- pi/8
w <- c(cos(alpha), sin(alpha))

# Project onto w
y1 <- w%*%t(y1)
y2 <- w%*%t(y2)

# kurtosis:
