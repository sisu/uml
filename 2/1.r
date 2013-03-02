library('MASS')
cnt <- 5000
n <- t(data.frame(mvrnorm(cnt, c(0,0), matrix(c(1,0,0,1),2,2))))
s <- matrix(runif(2*cnt, -sqrt(3),sqrt(3)), 2, cnt)

A1 <- matrix(c(0.4483, 2.1907, -1.6730, -1.4836),2,2)
A2 <- matrix(c(0, 1.7321, -1.7321, -2.0),2,2)

x1 <- A1 %*% s
x2 <- A2 %*% s
y1 <- A1 %*% n
y2 <- A2 %*% n

whiteT <- function(x) {
	x <- t(x)
	p <- prcomp(x)
	t(p$rotation) / p$sdev
}

splot <- function() {
	par(mfrow = c(1,3))
	plot(t(n))
	plot(t(A1 %*% n))
	plot(t(A2 %*% n))
}

wplot <- function() {
	par(mfrow = c(4,2))
	for (i in list(x1,x2,y1,y2)) {
		x <- i
		plot(t(x))
		plot(t(whiteT(x) %*% x))
	}
}

kurtAngle <- function(x, a) {
	v <- c(cos(a),sin(a))
	y <- x %*% whiteTT(x) %*% v
	sum(y**4)/length(y) - 3
}

plotkurt <- function() {
	n <- 100
	as <- (0:n)/n*pi
	par(mfrow = c(2,2))
	for (i in list(x1,x2,y1,y2)) {
		x <- t(i)
		ys <- sapply(as, function(a) kurtAngle(x,a))
		plot(as,ys, t='l')
	}
}
