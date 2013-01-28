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
	plot(xs,ys, asp=1, col='blue', xlab='First principal component', ylab='Second principal component')
#	segments(x0=0,y0=0, x1=xs, y1=ys)
#	points(xv,yv)
	segments(x0=0,y0=0, x1=xv, y1=yv, col='red')
}

varplot <- function() {
	devs <- px$sdev
	vars <- devs*devs
	nvars <- vars / sum(vars)
	cvars <- cumsum(nvars)
	barplot(cvars)
}
