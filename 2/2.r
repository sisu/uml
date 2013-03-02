source('1.r')

rlap <- function(n) {
	u <- runif(n,-.5,.5)
	sign(u) * log(1-2*abs(u)) / sqrt(2)
}

n <- 32
cnt <- 10000
s <- matrix(rlap(cnt*n), cnt)

#calcys <- function(m) {
#	y <- if (m==1) s[,1] else rowSums(s[,1:m])
#	y / sqrt(var(y))
#}
yhat <- sapply(1:n, function(m) if (m==1) s[,1] else rowSums(s[,1:m]))
ys <- apply(yhat,2, function(y) y/sqrt(var(y)))
#ys <- sapply(1:n, calcys)

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


mroot <- function(a) {
	eig <- eigen(a)
	eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
}
orthogonalize <- function(U) solve(mroot(U%*%t(U))) %*% U


kurtICAStep <- function(W, Z) (t(Z) %*% (Z %*% W)**3)/dim(Z)[1] - 3*W

# assumes that Z is white. The rows of Z are the variables.
kurtICA <- function(Z, m) {
	Z <- t(Z)
	n <- dim(Z)[2]
	W <- matrix(rnorm(n*m),n,m)
	for (i in 1:100) {
		prevW <- W
		W <- kurtICAStep(W, Z)
		W <- orthogonalize(W)
		diff <- sum((prevW-W)**2)
		if (diff<1e-6) break
	}
	W
}

Sigma <- lower.tri(diag(32), diag = T)*1
D <- diag(1/sqrt(apply(yhat,2, var)))
A <- D%*%Sigma

approxA <- function() {
	W <- whiteT(t(ys))
	B <- kurtICA(W %*% t(ys),32)
	solve(B %*% W)
}

absdiff <- function(x,y) min(sum((x-y)**2),sum(x+y)**2)

recError <- function(Ahat) {
	n <- dim(Ahat)[1]
	ns <- 1:n
	ms <- 1:n
	bestErr <- function(x) {
		err <- apply(as.matrix(A[,ms]),2,absdiff,x=x)
		min(err)
#		which(err == min(err))
	}
	res <- 0
	for (i in 1:n) {
		es <- apply(as.matrix(Ahat[,ns]),2,bestErr)
		j <- which(es==min(es))
		errs <- apply(as.matrix(A[,ms]),2,absdiff,x=Ahat[,ns[j]])
		k <- which(errs==min(errs))
		ns <- ns[ns!=ns[j]]
		ms <- ms[ms!=ms[k]]

		res  <- res + errs[k]
#		print(errs[k])
	}
	res / 32^2
#	aorder <- apply(Ahat,2,bestCol)
#	aorder
}
