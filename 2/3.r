source('1.r')
source('../1/visual.R')
X <- matrix(scan("mixed_images.txt",n=6*90000),6,90000,byrow="TRUE")

whiteT <- function(x) {
	x <- t(x)
	p <- prcomp(x)
	t(p$rotation) / p$sdev
}

mroot <- function(a) {
	eig <- eigen(a)
	eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
}
orthogonalize <- function(U) solve(mroot(U%*%t(U))) %*% U

ica <- function(X) {
	n <- dim(X)[1]
	cnt <- dim(X)[2]
	W <- whiteT(X)
	Z <- W %*% X
	B <- matrix(rnorm(n*n),n,n)
	gamma <- array(0,n)
	ug <- 0.8
	u <- 0.2

	expval <- function(x) rowSums(x) / dim(x)[2]

	objs <- NULL
	prevobj <- 1e100
	for (i in 1:10000) {
		Y <- B %*% Z
		ngamma <- expval(-tanh(Y)*Y + (1-tanh(Y)**2))
		gamma <- (1-ug)*gamma + ug * ngamma

		obj <- -sum(gamma * expval(log(cosh(Y))))
		objs <- c(objs,obj)

		I <- diag(n)
		gY <- t(sapply(1:n, function(i) if (gamma[i]>0) -2*tanh(Y[i,]) else tanh(Y[i,]) - Y[i,]))
		B <- B + u * (I + (gY %*% t(Y))/cnt) %*% B

		B <- orthogonalize(B)

#		print(obj)
#		print(B)
#		if (obj >= prevobj-1e-8) break
		if (abs(obj-prevobj)<1e-8) break
		prevobj <- obj
	}
	res <- NULL
	res$obj <- objs
	res$tr <- B %*% W
	res$gamma <- gamma
	res
}

plotICAres <- function(x, i) {
	y <- i$tr %*% x
	par(mfrow=c(1,2))
	plot(t(x), pch=16, cex=0.5, col='blue', xlab='Data x', ylab='Data y', asp=1)
	plot(t(y), pch=16, cex=0.5, col='blue', xlab='Latent x', ylab='Latent y', asp=1)
}

#B <- ica(X)

mkfigs <- function() {
	w <- 900
	h <- 600
	png('mixed.png', width=w, height=h)
	visual(X, 3)
	dev.off()
	png('white.png', width=w, height=h)
	visual(whiteT(X) %*% X, 3)
	dev.off()
	signs <- c(-1,-1,-1,1,-1,-1)
	png('icares.png', width=w, height=h)
	visual(B$tr %*% X*signs, 3)
	dev.off()

	c1 <- ica(x1)
	pdf('icatest.pdf', width=10, height=5)
	plotICAres(x1,c1)
	dev.off()
	pdf('icaobj.pdf', width=5, height=5)
	plot(c1$obj[5:length(c1$obj)],t='l',xlab='Number of iterations', ylab='Objective value')
	dev.off()
}
