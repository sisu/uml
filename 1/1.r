library('MASS')

#gendata <- function(n, xs) data.frame(mvrnorm(n, c(0,0), matrix(xs,2,2)))

#covs <- matrix(c(c(3,-1.5,4,2), c(1,0,0,1), c(1,.5,.5,.6), c(.8,.6,.6,.8)),4,4)

drawpoints <- function(p) plot(p, pch = 16, cex=0.5, col = 'blue', asp = 1, xlim=c(-4,4), ylim=c(-4,4), ann=FALSE)

createArtificialData <- function(theta, var){
	data <- cbind(rnorm(1000),rnorm(1000))
	R <- t(matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2)) # Rotation matrix
	D <- matrix(c(var[1],0,0,var[2]),2,2)# Dilation matrix
	A <- D%*%R # First dilate, then rotate
	data = apply(data, 1, "%*%", A)
	data = data.frame(data[1,],data[2,])
}
dvars <- matrix(c(1,2, 1,1, 1,.5, 1,.25),2)

drawprins <- function(ps) {
	drawpoints(ps)
	c <- prcomp(ps)
	r <- c$rotation
	slope <- r[2,1] / r[1,1]
	abline(a=0, b=slope, col='red', lwd=4)
	slope2 <- r[2,2] / r[1,2]
	abline(a=0, b=slope2, col='red', lwd=2)
}

#datas <- lapply(1:4, function(i) gendata(1000, covs[,i]))
datas <- data.frame(lapply(1:4, function(i) createArtificialData(1000, c(dvars[,i]))))

getdata <- function(i) cbind(datas[2*i-1],datas[2*i])

mkpcaplot <- function(num) drawprins(getdata(num))

mkhist <- function(d, comp) {
	c <- prcomp(d)

	cc <- c$rotation[,comp]
	projected <- c(rbind(t(cc)) %*% t(d))
	hist(projected, breaks=20, xlim=c(-6,6))
}

#matpcas <- matrix(c(1,1,-1,1)/sqrt(2),2,2)
#eigvals <- matrix(c(1,3),2,1)
#varmat <- matpcas * diag(eigvals) * solve(matpcas)

v1 <- c(1,1)/sqrt(2)*1
v2 <- c(-1,1)/sqrt(2)*3

theta = pi/4
V = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2)  #
#vvec <- data.frame(t(sapply(rnorm(1000), function(x) x*v1) + sapply(rnorm(1000), function(x) x*v2)))
Lambda = diag(c(1,3),2,2) # The eigenvalues are the variances of the PC's
Sigma = V%*%Lambda%*%t(V)
mean <- c(0,0)
vvec <- data.frame(mvrnorm(1000, mean, Sigma))

#vvcov <- cov(vvec)
#vveig <- eigen(vvcov)

projvv <- function() {
	c <- prcomp(vvec)
	c0 <- c$rotation[,1]
	p <- c(rbind(t(c0)) %*% t(vvec))

	filt <- vvec[1:50,]
	plot(filt, ann=F, col='blue', asp=1, cex=0.5)
	pf <- p[1:50]
	pps <- data.frame(pf %*% rbind(c0))
	points(pps, col='red', cex=0.5)
	segments(x0=pps[,1], y0=pps[,2], x1=filt[,1], y1=filt[,2])
}

mkpdfs <- function() {
	for (i in 1:4) {
		pdf(paste('doc/scatter', i, '.pdf', sep=''), width=5, height=5)
		d <- getdata(i)
		drawpoints(d)
		dev.off()
	}
	for (i in 1:4) {
		pdf(paste('doc/pcadir', i, '.pdf', sep=''), width=5, height=5)
		mkpcaplot(i)
		dev.off()
	}
	d <- getdata(1)
	for (j in 1:2) {
		pdf(paste('doc/histo1-', j, '.pdf', sep=''), width=5, height=5)
		mkhist(d, j)
		dev.off()
	}

	pdf('doc/vscatter.pdf', width=5, height=5)
	drawpoints(vvec)
	dev.off()

	pdf('doc/proj.pdf', width=5, height=5)
	projvv()
	dev.off()
}
