source('visual.R')
D <- as.matrix(read.table('data_proj.txt'))

e <- eigen(D %*% t(D) / 1000)

toNorm <- function(v) (v-min(v))/(max(v)-min(v))

plotcol <- function() {
	c1 <- c(t(e$vectors[,1]) %*% D)
	c2 <- c(t(e$vectors[,2]) %*% D)
	par(mfrow = c(1,2))
	plot(t(D), col=hsv(toNorm(c1)))
	plot(t(D), col=hsv(toNorm(c2)))
}

# X is n*N matrix, n is the dimension and N number of samples
# k is the number of model vectors to produce
som <- function(X, k) {
	n <- dim(X)[1]
	ws <- matrix(rnorm(n*k), n,k)
	sdist <- function(x,y) sum((x-y)**2)
	getClosest <- function(x) which.min(apply(ws,2,sdist,x=x))
	getIsNeighbor <- function(a) closest==a | closest==a%%k+1 | closest==(a+k-2)%%k+1
#	getIsNeighbor <- function(a) closest==a
	calcCenter <- function(isN) if (sum(isN)==0) rnorm(n) else rowSums(X[,isN]) / sum(isN)

	for(i in 1:20) {
		print(i)
		closest <- apply(X,2,getClosest)
#		print(closest)
		pw <- ws
		ws <- sapply(1:k, function(a) calcCenter(getIsNeighbor(a)))
		ch <- sum((pw-ws)**2)
#		print(ch)
		if (ch<1e-3) break
	}
	ws
}

plotSOM <- function(X, ws) {
	sdist <- function(x,y) sum((x-y)**2)
	getClosest <- function(x) which.min(apply(ws,2,sdist,x=x))
	closest <- apply(X,2,getClosest)
	rb <- rainbow(dim(ws)[2])
	plot(t(X), col=rb[closest])
	lines(t(ws))
#	points(t(ws),col=rb, pch=4, cex=2)
	points(t(ws),pch=4, cex=2)
}

#I <- lapply(1:6, function(i) as.matrix(read.table(paste('I',i,'.txt',sep=''))))
genPatches <- function(img) {
	r <- floor(dim(img)[1]/10)
	c <- floor(dim(img)[2]/10)
	data.frame(lapply(0:(r-1), function(y) sapply(0:(c-1), function(x) img[10*y+(1:10),10*x+(1:10)])))
}
patches <- data.frame(lapply(I, genPatches))

ppPatch <- function(x) {
	x <- x-mean(x)
	s <- sqrt(mean(x**2))
	if (s==0) x else x/s
}
pp <- apply(patches,2,ppPatch)
