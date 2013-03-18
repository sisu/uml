## setwd('/Users/juhaanttiisojarvi/Documents/jussi/opiskelu/unsupervised machine learning/git/uml/ex3Jussi')



## Exercise set 1



## Exercise 1
rGauss <- function(mean, var, theta, n){
	data <- cbind(rnorm(n),rnorm(n))
	R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2) # Rotation matrix
	D <- matrix(c(var[1],0,0,var[2]),2,2)# Dilation matrix
	A <- D%*%R # First dilate, then rotate
	data <- apply(data, 1, "%*%", A)
	data <- data + mean
	data <- data.frame(data[1,],data[2,])
	data
}

# draw 2000 samples from a 2-dim. mixture of gaussians:
n <- 2000
mean1 <- c(1,2)
var1 <- c(0.5,0.5)
mean2 <-  c(-4,0)
var2 <- c(0.5,0.5)

prior <- c(0.7, 0.3)
n1 <- sum(rbinom(n, size = 1, prob = prior[1]))
n2 <- (n-n1)

data1 <- rGauss(mean1, var1, 0, n1)
data2 <- rGauss(mean2, var2, 0, n2)
data <- rbind(data1, data2)

xlim <- c(min(data[1]), max(data[1]))
ylim <- c(min(data[2]), max(data[2]))

plot(data1, cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data2, cex = 0.3, pch = 16, col = 'darkblue')




# Exercise 2
invRoot <- function(x){
	eig <- eigen(x)
	y <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
	return(y)
}

symOrtogProj <- function(x){
	invRoot(x%*%t(x))%*%x
}

# generates a random covariance matrix
createRCov <- function(ndim = 2){
	lambda <- diag(rexp(ndim,0.5))
	C <- matrix(rnorm(ndim^2, 0, 1), c(ndim,ndim))
	C <- symOrtogProj(C)
	C %*% lambda %*% t(C)
}

# generates a list of random covariance matrices
createRListCov <- function(nclust = 2, ndim = 2){
	C <- vector('list', nclust)
	for(iii in 1 : nclust){
		C[[iii]] <- createRCov(ndim)
	}
	C
}

# generates a list of random mean vectors
createRListMean <- function(nclust = 2, ndim = 2){
	mu <- vector('list', nclust)
	for(iii in 1:nclust){
		mu[iii] <- list(runif(ndim, -10,10))
	}
	mu
}

# generates a list of prior probabilities for the clusters
createRListPie <- function(nclust){
	pie <- c(sort(runif(nclust - 1)), 1)
	apu <- pie
	for(iii in 2:nclust){
		pie[iii] <- apu[iii] - apu[iii - 1]
	}
	pie
}

# the posterior probabilities of the cluster indexes of the points
pr <- function(x,C,mu,pie){
	abs(det(C))^(-1/2) * exp(-1/2 * t(as.matrix(x-mu)) %*% solve(C) %*% (as.matrix(x-mu))) * pie
}

# This one is for the apply in function qt
multWT <- function(x){
	as.matrix(x) %*% t(as.matrix(x))
}

# computes a list containing q_t_c for all t for each c
qt <- function(data,C,mu,pie){
	qt <- vector('list', nclust)
	for(iii in 1:nclust){
		qt[[iii]] <- apply(data, 1, pr, C = C[[iii]], mu = mu[[iii]], pie = pie[[iii]])
	}
	qt
}

# normalizes the qt:
qtstar <- function(data,C,mu,pie){
	qtstar <- vector('list', nclust)
	p <- qt(data,C,mu,pie)
	for(iii in 1:nclust){
		qtstar[[iii]] <- p[[iii]] * 1/Reduce('+', p)
	}
	qtstar
}

# The EM algorithm starts here
EM <- function(data, nclust, C, mu, pie){
	ndim <- length(data)
	
	qts <- NA
	
	condition <- T
	while(condition){
		# parameters at beginnning of iteration (for stop criterion of loop):
		muBfr <- mu
		CBfr <- C
		pieBfr <- pie
		
		qts <- qtstar(data,C,mu,pie)
		
		# mu:
		for(j in 1:nclust){
			mu[[j]] <- 1/sum(qts[[j]]) * colSums(qts[[j]] * data)
		}
		
		# C:
		for(j in 1:nclust){
			C[[j]] <- 1/sum(qts[[j]]) * matrix(colSums(qts[[j]] * t(apply(t(apply(data,1,'-', mu[[j]])), 1, multWT))), c(2,2))
		}
		
		# pie:
		for(j in 1:nclust){
			pie[[j]] <- 1/n * sum(qts[[j]])
		}
	    
	    # stop condition change of estimated parameters sufficiently small
	    condition <- sqrt(sum(c(unlist(CBfr), unlist(muBfr), unlist(pieBfr)) - c(unlist(C), unlist(mu), unlist(pie)))^2) > 0.1
	}
	
	# return
	ret <- list("qts" = qts, 'mu' = mu, 'C' = C, "pie" = pie)
	ret
}




# Exercise 3
nclust = 2
C <- list(diag(rep(1,length(data))))
C <- rep(C,nclust)
mu <- createRListMean(nclust, length(data))
pie <- rep(1/nclust, nclust)

# do EM and plot clusters:
em <- EM(data, nclust, C, mu , pie)
cindex <- apply(Reduce(cbind, em$qts), 1 , which.max)
plot(data[cindex == 1,], cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data[cindex == 2,], cex = 0.3, pch = 16, col = 'darkblue')


# Exercise 4

# draw 100 samples from gaussian mixture with 4 clusters:
n <- 2000

mean1 <- c(1,2)
var1 <- c(1,1)
mean2 <-  c(-4,0)
var2 <- c(0.5,0.5)
mean3 <- c(-5,10)
var3 <- c(1,1)
mean4 <- c(-10,-5)
var4 <- c(1,1)

prior <- c(0.25, 0.5, 0.1, 0.4)

ni <- rbinom(n, size = 3, prob = prior[1:nclust-1])
n1 <- sum(ni == 0)
n2 <- sum(ni == 1)
n3 <- sum(ni == 2)
n4 <- sum(ni == 3)

data1 <- rGauss(mean1, var1, 0, n1)
data2 <- rGauss(mean2, var2, 0, n2)
data3 <- rGauss(mean3, var3, 0, n3)
data4 <- rGauss(mean4, var4, 0, n4)
data <- rbind(data1, data2, data3, data4)

# plot result:
xlim <- c(min(data[1]), max(data[1]))
ylim <- c(min(data[2]), max(data[2]))
plot(data1, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data2, pch = 16, col = 'darkblue')
points(data3, pch = 16, col = 'purple')
points(data4, pch = 16)

# Do EM with 4 clusters and plot clusters with 5 different initial points:

nclust = 4
C <- list(diag(rep(1,length(data))))
C <- rep(C,nclust)
mu <- createRListMean(nclust, length(data))
pie <- rep(1/nclust, nclust)
em <- EM(data,nclust,C,mu,pie)
em <- vector('list', 5)
cindex <- vector('list', 5)
for(k in 1:5){
	C <- createRListCov(nclust, length(data))
	mu <- createRListMean(nclust, length(data))
	pie <- createRListPie(nclust)
	em[[k]] <- EM(data, nclust, C, mu, pie)
	cindex[[k]] <- apply(Reduce(cbind, em[[k]]$qts), 1 , which.max)
}

plot(data[cindex[[2]] == 1,], cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data[cindex[[2]] == 2,], cex = 0.3, pch = 16, col = 'darkblue')
points(data[cindex[[2]] == 3,], cex = 0.3, pch = 16, col = 'purple')
points(data[cindex[[2]] == 4,], cex = 0.3, pch = 16)

