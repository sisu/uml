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

plotpca <- function() {
	p <- prcomp(data)
	ndev <- p$sdev / sum(p$sdev)
	plot(cumsum(ndev), t='l', xlab='Number of principal components', ylab='Proportion of variance explained')
}
