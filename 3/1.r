library('MASS')

n <- 2000
genData <- function(theta, var, center) {
	data <- cbind(rnorm(n),rnorm(n))
	R <- t(matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2)) # Rotation matrix
	D <- matrix(c(var[1],0,0,var[2]),2,2)# Dilation matrix
	A <- D%*%R # First dilate, then rotate
	data = apply(data, 1, "%*%", A)
	data = data.frame(data[1,],data[2,])
	cm <- t(matrix(center, 2, n))
	data + cm
}

dat1 <- genData(1.5, c(.6,.9), c(-2,2))
dat2 <- genData(pi/8, c(1,.1), c(2,2))
prob1 <- 0.5
choises <- rbinom(n, 1, prob1)
chmat <- matrix(choises, n, 2)
dat <- as.matrix(chmat*dat1 + (1-chmat)*dat2)

plotdat <- function() plot(dat, pch=16, cex=1, col='blue', xlim=c(-6,6), ylim=c(0,4))


# X: t*n matrix, t is the number of samples and n the number of dimensions
# m: the desired number of cluster
emCluster <- function(X, m) {
	t <- dim(X)[1]
	n <- dim(X)[2]
	posdef <- function(M) M %*% t(M)
	C <- lapply(1:m, function(c) posdef(matrix(rnorm(n*n),n,n)))
	mu <- matrix(rnorm(n*m), n,m)
	p <- runif(m)
	p <- p / sum(p)
#	X <- t(X)

	calcQ <- function(c,t,invC,constf) exp(-.5 * t(X[t,]-mu[,c]) %*% invC %*% (X[t,]-mu[,c])) * constf

	objs <- NULL
	prevobj <- -1e100
	qn <- NULL
	for(i in 1:10) {
		# maximization
#		invCs <- lapply(1:m, function(c) solve(C[[c]]))
#		constfs <- sapply(1:m, function(c) p[c] / sqrt(det(C[[c]])))
		q <- sapply(1:m, function(c) sapply(1:t, calcQ, c=c, invC=solve(C[[c]]), constf=p[c]/sqrt(det(C[[c]]))))
#		q <- sapply(1:m, function(c) exp(-.5* t(X-mu[,c]) %*% solve(C[[c]]) %*% (X-mu[,c])) * p[c] / sqrt(det(C[[c]])) )
#		print(dim( t(X-mu[,1]) %*% C[[1]] %*% (X-mu[,1]) ))
		qn <- t(apply(q, 1, function(x) x/sum(x)))

		mu <- sapply(1:m, function(c) colSums(qn[,c]*X) / sum(qn[,c]))
		C <- lapply(1:m, function(c) Reduce('+', lapply(1:t, function(t) qn[t,c] * (X[t,]-mu[,c]) %*% t(X[t,]-mu[,c]))) / sum(qn[,c]))
		p <- colSums(qn) / t
#		print(C)
#		print(mu)
#		print(dim(mu))
#		print(length(p))

		# expectation
		calcObj <- function(c,t,invC,constt) (-.5*t(X[t,]-mu[,c])%*%invC%*%(X[t,]-mu[,c]) + constt) * qn[t,c]
		obj <- sum(sapply(1:m, function(c) sapply(1:t, calcObj, c=c, invC=solve(C[[c]]), constt=log(p[c])-.5*log(abs(det(C[[c]])))-log(2*pi)**(n/2))))
#		if (obj < prevobj+1e-5) break
		if (abs(obj - prevobj) < 1e-5) break
#		print(obj)
		objs <- c(objs,obj)
	}
	res <- NULL
	res$cov <- C
	res$mean <- mu
	res$prob <- p
	res$obj <- objs
	res$map <- apply(qn,1,which.max) - 1
	res
}

plotChoises <- function(ch) {
	x1 <- dat[ch==0,]
	x2 <- dat[ch==1,]
	plot(x1, pch=16, cex=1, col='blue', xlim=c(-6,6), ylim=c(0,4))
	points(x2, pch=16, cex=1, col='red')
}
