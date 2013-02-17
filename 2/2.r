rlap <- function(n) {
	u <- runif(n,-.5,.5)
	sign(u) * log(1-2*abs(u)) / sqrt(2)
}

n <- 32
cnt <- 10000
s <- matrix(rlap(cnt*n), cnt)

calcys <- function(m) {
	y <- if (m==1) s[,1] else rowSums(s[,1:m])
	y / sqrt(var(y))
}
ys <- sapply(1:n, calcys)

ms <- 2^(0:5)

mcdf <- function(m,x) sum(ys[,m]<x) / cnt
eps <- 0.1
mpdf <- function(m,x) (mcdf(m,x+eps) - mcdf(m,x-eps)) / (2*eps)

plotlogpdf <- function() {
	par(mfrow = c(3,2))
	d <- 3
	k <- 100
	xs <- d * ((1:(2*k))/k-1)
	for(m in ms) {
		ys <- sapply(xs, function(x) mpdf(m,x))
		plot(xs,log(ys), t='l')
	}
}
plotgausslog <- function() {
	d <- 3
	k <- 100
	xs <- d * ((1:(2*k))/k-1)
	ys <- sapply(xs, dnorm)
	plot(xs, log(ys), t='l')
}

kurt <- function(xs) sum(xs**4)/length(xs) - 3

plotkurt <- function() {
	xs <- 1:32
	ys <- sapply(xs, function(m) kurt(ys[,m]))
	plot(xs,ys, t='l')
}
