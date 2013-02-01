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

td <- t(data)
pc <- prcomp(td)

plotpcadev <- function() {
	ndev <- pc$sdev / sum(pc$sdev)
	plot(cumsum(ndev), t='l', xlab='Number of principal components', ylab='Proportion of variance explained')
}
plotpcas <- function(n) visual(pc$rotation[,1:n])


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
	visual(ordered, cc)
}

noisyt <- read.table('noisyDigits.txt')
noisy <- t(noisyt)
plotdenoise <- function(n) {
	dn <- approxby(noisy, n)
	visual(t(dn))
}
