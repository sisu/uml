X0 <- c(5,3,0,1,-1,-3,5,0,-4,-4,
	    -2,-1,0,0,1,4,-3,1,5,3,
	    0,1,4,-1,0,5,5,-5,-3,-3,
	    0,2,3,0,-1,3,3,-7,-2,0,
	    3,4,-2,1,3,-3,-3,2,0,0)
X <- t(matrix(X0, 5, 10, byrow=T))

px <- prcomp(X)

projplot <- function() {
	c1 <- px$rotation[,1]
	c2 <- px$rotation[,2]

	xs <- px$x[,1]
	ys <- px$x[,2]

	vars <- 10*diag(5)
	xv <- 10*c1
	yv <- 10*c2

#	print(xv)
#	print(yv)

#	plot.new()
	plot(xs,ys, asp=1, col='black', xlab='The first principal component', ylab='The second principal component')
#	segments(x0=0,y0=0, x1=xs, y1=ys)
#	points(xv,yv)
	segments(x0=0,y0=0, x1=xv, y1=yv, col='red', lwd=2)
	abline(h=0,v=0,col='blue',lwd=3)
}

varplot <- function() {
	devs <- px$sdev
	vars <- devs*devs
	nvars <- vars / sum(vars)
	cvars <- cumsum(nvars)
	plot(cvars, xlab='Number of principal components', ylab='Variance explained', t='l')
		 #, names.arg=1:5)
}

gradJ <- function(A,U) 4 * t(A) %*% (A %*% U)**3

mroot <- function(a) {
	eig <- eigen(a)
	eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
}
mroott <- function(mat,maxit=50) {
	stopifnot(nrow(mat) == ncol(mat))
	niter <- 0
	y <- mat
	z <- diag(rep(1,nrow(mat)))
	for (niter in 1:maxit) {
		y.temp <- 0.5*(y+solve(z))
		z <- 0.5*(z+solve(y))
		y <- y.temp
	}
	y
}

quartimax <- function(A) {
	f <- 0.1
	U <- diag(dim(A)[2])
	for (i in 1:100) {
		U <- U + f * gradJ(A,U)
		U <- solve(mroot(U%*%t(U))) %*% U
#		U <- U %*% solve(mroot(t(U)%*%U))
	}
	U
}

A0 <- c(-.9511, -1.6435, 2.3655, -2.9154, -3.7010)
A1 <- c(.9511, -1.6435, 2.3655, -2.9154, 3.7010)
A <- matrix(c(A0,-A1),5,2)
#Anorm <- matrix(c(A0/sqrt(sum(A0*A0)),A1/sqrt(sum(A1*A1))),5,2)

u1proj <- c(-0.7625, 0.5724, -0.0865, -0.0191, -0.2882)
u2proj <- c(-0.0338, 0.0377, -0.6922, -0.5909, 0.4113)

projqmax <- function() {
	u1 <- px$rotation[,1]
	u2 <- px$rotation[,2]
	a <- matrix(c(u1,u2),5)
#	A <- px$rotation
	U <- quartimax(a)
	m <- a %*% U
	d <- X %*% m
	plot(d, xlim=c(-10,10), ylim=c(-10,10), xlab='The first principal component rotated', ylab='The second principal component rotated')
	abline(h=0,v=0,col='blue',lwd=3)
	segments(x0=0,y0=0,x1=10*m[,1],y1=10*m[,2],col='red',lwd=2)
	m
}

mkpdfs <- function() {
	w <- 7
	h <- 7
	pdf('doc/fig42.pdf', width=w, height=h)
	projplot()
	dev.off()

	pdf('doc/varamount.pdf', width=w, height=h)
	varplot()
	dev.off()

	pdf('doc/qmax.pdf', width=w, height=h)
	projqmax()
	dev.off()
}
