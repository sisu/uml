source('visual.R')

digits <- read.table('digits.txt')
vars <- dim(digits)[1]
nums <- dim(digits)[2]

avgs <- colSums(digits) / vars
digits.center <- digits - matrix(rep(avgs, vars), vars, byrow=T)

normalize <- function(v) v / norm(matrix(v),"F")
digits.norm <- sapply(1:nums, function(i) normalize(digits.center[,i]))

vmeans <- rowSums(digits.norm) / nums
data <- digits.norm - matrix(rep(vmeans,nums), vars)

preprocess <- function(d) {
	vars <- dim(d)[1]
	nums <- dim(d)[2]
	d$avgs <- colSums(d) / vars
	d$center <- d - matrix(rep(avgs, vars), vars, byrow=T)
	d$norm <- sapply(1:nums, function(i) normalize(d$center[,i]))
	d$vmeans <- rowSums(d$norm) / nums
	d$x <- d$norm - matrix(rep(d$vmeans,nums), vars)
}

td <- t(data)
pc <- prcomp(td)

plotpcadev <- function() {
	ndev <- pc$sdev / sum(pc$sdev)
	plot(cumsum(ndev), t='l', xlab='Number of principal components', ylab='Proportion of variance explained')
}
plotpcas <- function(n) visual(t(pc$rotation[,1:n]))


counts <- c(2^(0:7), dim(td)[2])

tdigits <- td[1:10,]

approxby <- function(d, n) {
	cs <- pc$rotation[,1:n]
	reduced <- d %*% cs
	reduced %*% t(cs)
}

plotapprox <- function() {
	resmat <- data.frame(lapply(counts, function(n) t(approxby(tdigits, n))))
	dc <- dim(tdigits)[1]
	cc <- length(counts)
	order <- c(sapply(1:dc, function(n) n+dc*(0:(cc-1))))
#	print(order)
	ordered <- resmat[,order]
	visual(t(ordered), cc)
}

noisyt <- read.table('noisyDigits.txt')
noisy <- t(noisyt)
plotdenoise <- function(n) {
	dn <- approxby(noisy, n)
	visual(dn)
}

mkpdfs <- function() {
	w <- 7
	h <- 7
	n <- 20
	pdf('doc/digits.pdf', width=w, height=h)
	visual(t(digits)[1:n,])
	dev.off()

	pdf('doc/digitspre.pdf', width=w, height=h)
	visual(t(data)[1:n,])
	dev.off()

	pdf('doc/pcavar.pdf', width=w, height=h)
	plotpcadev()
	dev.off()

	pdf('doc/pcanums.pdf', width=w, height=h)
	plotpcas(20)
	dev.off()

	pdf('doc/digitreduce.pdf', width=w, height=h)
	plotapprox()
	dev.off()

	pdf('doc/noisy.pdf', width=w, height=h)
	visual(noisy)
	dev.off()

	dns <- 2^(3:6)
	for (i in dns) {
		pdf(paste('doc/denoise',i,'.pdf',sep=''), width=w, height=h)
		plotdenoise(i)
		dev.off()
	}
}
