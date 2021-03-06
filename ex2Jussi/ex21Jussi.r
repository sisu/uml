# Exercise 1

# create data
Sigma <- diag(c(1,1))
n <- mvrnorm(n = 5000, mu = c(0,0), Sigma = Sigma)
x <- cbind(runif(5000, min = -sqrt(3), max = sqrt(3)),runif(5000, min = -sqrt(3), max = sqrt(3)))

# transformation matrices
A1 <- matrix(c(0.4483, 2.1907,-1.6730, -1.4836), c(2,2))
A2 <- matrix(c(0, 1.7321, -1.7321, -2), c(2,2))

# transform data
y1 <- t(A1%*%t(n))
y2 <- t(A2%*%t(n))
x1 <- t(A1%*%t(x))
x2 <- t(A2%*%t(x))

# plotting
# par(mfrow = c(3,3))
# par(pty = 's')
# par(mar=c(0,0,0,0)+2)
# plot(n, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'n')
# plot(y1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'y1')
# plot(y2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'y2')
# plot(x, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'x')
# plot(x1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'x1')
# plot(x2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'x2')




# Exercise 2
PCAnalysis <- function(x){
	eig <- eigen(cov(x))	
	rotation <- eig$vectors
	sdev <- sqrt(eig$values)
	sorted <- sort(eig$values, decreasing = T)
	for(iii in 1:length(sorted)){
		rotation[iii,] <- eig$vectors[eig$values == sorted[iii],]
	}
	PCA <- list('rotation' = rotation, 'sdev' = sdev)
	return(PCA)
}

whiten <- function(x){
	PCA <- prcomp(x)
	whitening <- diag(1/(PCA$sdev))%*%t(PCA$rotation)
	y <- whitening%*%t(x)
	return(y)
}

# whiten data
whitenedY1 <- t(whiten(y1))
whitenedY2 <- t(whiten(y2))
whitenedX1 <- t(whiten(x1))
whitenedX2 <- t(whiten(x2))

# plot whitened data
# par(mfrow = c(2,2))
# par(pty = 's')
# par(mar=c(0,0,0,0)+2)
# plot(whitenedY1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'Whitened y1')
# plot(whitenedY2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta' ,main = 'Whitened y2')
# plot(whitenedX1, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'Whitened x1')
# plot(whitenedX2, xlim = c(-10,10), asp = 1, pch = 16, cex = 0.2, col = 'magenta', main = 'Whitened x2')



# Exercise 3
# kurtosis:
kurtosis <- function(x){
	mean(x^4) - 3*mean(x^2)^2
}

# kurtosis as function of alpha
kurtosisAlpha <- function(alpha, data){
	w <- c(cos(alpha), sin(alpha))
	kurt <- kurtosis(w%*%t(data))
	kurt
}

alpha <- seq(from = 0, to = pi, length.out = 1000)
# Kurtosis of y1 as a function of alpha
kurtY1 <- sapply(alpha, kurtosisAlpha, data = whitenedY1)
# angle of maximal kurtosis
maxAlphaY1 <- alpha[abs(kurtY1) == max(abs(kurtY1))]

# Kurtosis of y2 as a function of alpha
kurtY2 <- sapply(alpha, kurtosisAlpha, data = whitenedY2)
# angle of maximal kurtosis
maxAlphaY2 <- alpha[abs(kurtY2) == max(abs(kurtY2))]

# Kurtosis of x1 as a function of alpha
kurtX1 <- sapply(alpha, kurtosisAlpha, data = whitenedX1)
# angle of maximal kurtosis
maxAlphaX1 <- alpha[abs(kurtX1) == max(abs(kurtX1))]

# Kurtosis of x2 as a function of alpha
kurtX2 <- sapply(alpha, kurtosisAlpha, data = whitenedX2)
# angle of maximal kurtosis
maxAlphaX2 <- alpha[abs(kurtX2) == max(abs(kurtX2))]



# plotting
# par(mfrow = c(2,2))
# par(pty = 's')
# par(mar=c(0,0,0,0)+2)
# plot(kurtY1~alpha, type = 'l', xlim = c(0,pi), col = 'magenta', main = 'Kurtosis of y1 as a function of alpha')
# abline(v=maxAlphaY1, col = 'red')
# plot(kurtY2~alpha, type = 'l', xlim = c(0,pi), col = 'magenta', main = 'Kurtosis of y2 as a function of alpha')
# abline(v=maxAlphaY2, col = 'red')
# plot(kurtX1~alpha, type = 'l', xlim = c(0,pi), col = 'magenta', main = 'Kurtosis of x1 as a function of alpha')
# abline(v=maxAlphaX1, col = 'red')
# plot(kurtX2~alpha, type = 'l', xlim = c(0,pi), col = 'magenta', main = 'Kurtosis of x2 as a function of alpha')
# abline(v=maxAlphaX2, col = 'red')






# Exercise 4
maxAlpha <- function(x, n){
	alpha <- seq(from = 0, to = pi, length.out = n)
	kurt <- sapply(alpha, kurtosisAlpha, data = x)
	maxAlpha <- alpha[abs(kurt) == max(abs(kurt))]

}

estA <- function(x, n){
	whitenedX <- t(whiten(x))
	b1 <- c(cos(maxAlpha(whitenedX, n)), sin(maxAlpha(whitenedX, n)))
	R <- matrix(c(0,1,-1,0), c(2,2))
	b2 <- t(R%*%b1)
	invWA <- rbind(b1,b2)
	PCA <- prcomp(x)
	whitening <- diag(1/PCA$sdev)%*%t(PCA$rotation)
	A <- ginv(whitening)%*%ginv(invWA)
	return(A)	
}

# Compare the estimated A and the original:
# estA(x1,1000)
# A1
# estA(x2,1000)
# A2
# estA(y1,1000)
# A1
# estA(y2,1000)
# A2

