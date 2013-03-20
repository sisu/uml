source('visual.R')
D <- as.matrix(read.table('data_proj.txt'))

e <- eigen(D %*% t(D) / 1000)

toNorm <- function(v) (v-min(v))/(max(v)-min(v))

plotcol <- function() {
	c1 <- c(t(e$vectors[,1]) %*% D)
	c2 <- c(t(e$vectors[,2]) %*% D)
#	par(mfrow = c(1,2))
	plot(t(D), col=hsv(.5+.5*toNorm(c1)), pch=16, cex=0.5, xlab=NA, ylab=NA)
#	plot(t(D), col=hsv(toNorm(c2)), xlab=NA, ylab=NA)
}
plotpca <- function() {
	plotcol()
	c1 <- 3*e$vectors[,1]
	c2 <- 3*e$vectors[,2]
	arrows(x0=0,y0=0,x1=c(c1[1],c2[1]), y1=c(c1[2],c2[2]))
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

	for(i in 1:30) {
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
	rb <- rainbow(dim(ws)[2], v=0.9)
	plot(t(X), col=rb[closest], pch=16, cex=0.5, xlab=NA, ylab=NA)
	lines(t(ws))
#	points(t(ws),col=rb, pch=4, cex=2)
	points(t(ws),pch=4, cex=2)
}
somPlots <- function(X) {
	sz <- c(4,7,12,20)
	par(mfrow=c(2,2))
	for(i in sz) {
		w <- som(X, i)
		plotSOM(X,w)
	}
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

mkfigs <- function() {
	w<-5
	h<-5
	pdf('mds.pdf',width=w,height=h)
	plotcol()
	dev.off()
	pdf('mdspca.pdf',width=w,height=h)
	plotpca()
	dev.off()
	pdf('som.pdf',width=7,height=7)
	somPlots(D)
	dev.off()
}
