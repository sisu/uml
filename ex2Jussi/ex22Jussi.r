# Exercise 1
rlap <- function(n) {
	u <- runif(n,-.5,.5)
	sign(u) * log(1-2*abs(u)) / sqrt(2)
}

nPoints <- 10000
s <- matrix(nrow=nPoints,ncol=32)
for(iii in 1:32){
	s[,iii] <- rlap(nPoints)
}

yHat = matrix(nrow=nPoints,ncol=32)
yHat[,1] <- s[,1]
for(iii in 2:32){
	yHat[,iii] <- rowSums(s[,1:iii])	
}

y <- matrix(nrow=nPoints,ncol=32)
for(iii in 1:32){
	y[,iii] <- yHat[,iii]/sqrt(var(yHat[,iii]))
}

# density estimate using kernel estimation
dens <- apply(y,2,density, bw = 0.25, n = 512, from = -4, to = 4)
# take logarithm of density function
for(iii in 1:32){
	dens[[iii]]$y <- log(dens[[iii]]$y)
}

# plotting
# par(mar=c(0,0,0,0)+2)
# par(mfrow = c(2,3))
# for(iii in c(1,2,4,8,16,32)){
	# plot(dens[[iii]], xlim = c(-4,4), ylim = c(-8,0), main = '', col = 'lightslateblue')
	# points(dens[[iii]]$x, log(dnorm(dens[[iii]]$x)), type = 'l', col = 'magenta')
# }




# Exercise 2
kurtosis <- function(x){
	mean(x^4) - 3
}
kurt <- apply(y,2,kurtosis)

# plotting
# plot(kurt, type = 'l', col = 'forestgreen', xlab = 'm', ylab='Kurtosis')



# Exercise 3
# Data from 1. exercise:
whiten <- function(x){
	PCA <- prcomp(x)
	whitening <- diag(1/(PCA$sdev))%*%t(PCA$rotation)
	y <- whitening%*%t(x)
	return(y)
}

x <- cbind(runif(5000, min = -sqrt(3), max = sqrt(3)),runif(5000, min = -sqrt(3), max = sqrt(3)))
A1 <- matrix(c(0.4483, 2.1907,-1.6730, -1.4836), c(2,2))
x1 <- t(A1%*%t(x))

whitenedX1 <- t(whiten(x1))

# Actual exercise begins here:
normVector <- function(v) sqrt(sum(v^2))

invRoot <- function(x){
	eig <- eigen(x)
	y <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
	return(y)
}

symOrtogProj <- function(x){
	invRoot(x%*%t(x))%*%x
}

kurtGrad <- function(x,w){
	colMeans(apply(x, 2, '*', (w%*%t(x))^3)) -3*w
}

ICAOnePointIteration <- function(x,w){
	w <- kurtGrad(x,w)
	# w <- w/normVector(w)
	return(w)
}

ICASymmetric <- function(x, n){
	W <- diag(1,n,dim(x)[2])
	for(iii in 1:100){
		W <- apply(W,1, ICAOnePointIteration, x = x)
		W <- symOrtogProj(W)
	}
	return(W)
}

# Exercise 5
Sigma <- lower.tri(diag(32), diag = T)*1
D <- diag(1/sqrt(apply(yHat,2, var)))
A <- D%*%Sigma
#
wY <- t(whiten(y))
PCA <- prcomp(y)
whitening <- diag(1/(PCA$sdev))%*%t(PCA$rotation)
#Ahat <- ICASymmetric(wY, 32)
#Ahat <- solve(whitening)%*% Ahat

# Average squared error:
# mean(1/32^2 * (Ahat-A)^2)
