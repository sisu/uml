source('visual.R')

digits <- read.table('digits.txt')

preprocess <- function(d) {
	v <- dim(d)[1]
	n <- dim(d)[2]

	avgs <- colSums(d) / v
	center <- d - matrix(rep(avgs, v), v, byrow=T)

	norms <- sapply(1:n, function(i) norm(matrix(center[,i]), "F"))
	normalized <- sapply(1:n, function(i) center[,i]/norms[i])
	vmeans <- rowSums(normalized) / n
	data <- normalized - matrix(rep(vmeans,n), v)

	res <- NULL
	res$avgs <- avgs
	res$norms <- norms
	res$vmeans <- vmeans
	res$x <- data
	res
}
postprocess <- function(d) {
	v <- dim(d$x)[1]
	n <- dim(d$x)[2]
	x <- d$x + matrix(rep(d$vmeans,n), v)
	y <- sapply(1:n, function(i) x[,i]*d$norms[i])
	y + matrix(rep(d$avgs, v), v, byrow=T)
}
take <- function(d, ns) {
	r <- NULL
	r$avgs <- d$avgs[ns]
	r$norms <- d$norms[ns]
	r$vmeans <- d$vmeans
	r$x <- d$x[,ns]
	r
}

data <- preprocess(digits)

td <- t(data$x)
pc <- prcomp(td)

plotpcadev <- function() {
	ndev <- pc$sdev / sum(pc$sdev)
	plot(cumsum(ndev), t='l', xlab='Number of principal components', ylab='Proportion of variance explained')
}
plotpcas <- function(n) visual(t(pc$rotation[,1:n]))


counts <- c(2^(0:7), dim(td)[2])

tdata <- take(data, 1:10)

approxby <- function(d, n) {
	cs <- pc$rotation[,1:n]
	reduced <- t(d$x) %*% cs
	d$x <- t(reduced %*% t(cs))
	postprocess(d)
}

plotapprox <- function() {
	resmat <- data.frame(lapply(counts, function(n) approxby(tdata, n)))
	dc <- dim(tdata$x)[2]
	cc <- length(counts)
	order <- c(sapply(1:dc, function(n) n+dc*(0:(cc-1))))
#	print(order)
	ordered <- resmat[,order]
	visual(t(ordered), cc)
}
apprerror <- function(d, n) {
	cs <- pc$rotation[,1:n]
	reduced <- t(d$x) %*% cs
	m <- t(reduced %*% t(cs))
	sum((m-d$x)**2) / dim(d$x)[2]
}
ploterror <- function() {
	cs <- 2^(0:7)
	errs <- sapply(cs, function(i) apprerror(tdata, i))
	plot(cs, errs, xlab="Number of principal components", ylab="Average reconstruction error", t='l')
}

noisyt <- read.table('noisyDigits.txt')
noisy <- preprocess(noisyt)
#noisy <- t(noisyt)
plotdenoise <- function(n) {
	dn <- approxby(noisy, n)
	visual(t(dn))
}

mkpdfs <- function() {
	w <- 500
	h <- 500
	n <- 20
	png('doc/digits.png', width=w, height=h)
	visual(t(digits)[1:n,])
	dev.off()

	png('doc/digitspre.png', width=w, height=h)
	visual(t(data$x)[1:n,])
	dev.off()

	pdf('doc/pcavar.pdf', width=5, height=5)
	plotpcadev()
	dev.off()

	png('doc/pcanums.png', width=w, height=h)
	plotpcas(20)
	dev.off()

	png('doc/digitreduce.png', width=w, height=h)
	plotapprox()
	dev.off()

	pdf('doc/error.pdf', width=5, height=5)
	ploterror()
	dev.off()

	png('doc/noisy.png', width=w, height=h)
	visual(t(noisy$x))
	dev.off()

	dns <- 2^(3:6)
	for (i in dns) {
		png(paste('doc/denoise',i,'.png',sep=''), width=w, height=h)
		plotdenoise(i)
		dev.off()
	}
}
