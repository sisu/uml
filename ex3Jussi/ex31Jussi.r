t## setwd('/Users/juhaanttiisojarvi/Documents/jussi/opiskelu/unsupervised machine learning/git/uml/ex3Jussi')


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
var1 <- c(1,3)
mean2 <-  c(-4,0)
var2 <- c(0.5,4)

prior <- c(0.7, 0.3)
n1 <- sum(rbinom(n, size = 1, prob = prior[1]))
n2 <- (n-n1)

data1 <- rGauss(mean1, var1, pi/6, n1)
data2 <- rGauss(mean2, var2, (pi-pi/3), n2)
data <- rbind(data1, data2)

# the covariance matrices of the underlying distributions:
# theta <- pi/6
# R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2) # Rotation matrix
# D <- matrix(c(var1[1],0,0,var1[2]),2,2)# Dilation matrix
# A <- D%*%R # First dilate, then rotate
# cov1 <- t(A)%*%A
# theta <- pi - pi/3
# R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2) # Rotation matrix
# D <- matrix(c(var2[1],0,0,var2[2]),2,2)# Dilation matrix
# A <- D%*%R # First dilate, then rotate
# cov2 <- t(A)%*%A


xlim <- c(min(data[1]), max(data[1]))
ylim <- c(min(data[2]), max(data[2]))

plot(data1, cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data2, cex = 0.3, pch = 16, col = 'darkblue')




# Exercise 2
pr <- function(x,C,mu,pie){
	abs(det(C))^(-1/2) * exp(-1/2 * t(as.matrix(x-mu)) %*% solve(C) %*% (as.matrix(x-mu))) * pie
}

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
EM <- function(data, nclust){
	ndim <- length(data)
	
	# initializes starting point of maximization algorithm:
	C <- vector('list', nclust)
	for(iii in 1:nclust){
		C[iii] <- list(diag(c(1,1)))
	}
	mu <- vector('list', nclust)
	for(iii in 1:nclust){
		mu[iii] <- list(runif(ndim, -10,10))
	}
	pie <- c(sort(runif(nclust - 1)), 1)
	apu <- pie
	for(iii in 2:nclust){
		pie[iii] <- apu[iii] - apu[iii - 1]
	}
	
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
			C[[j]] <- 1/sum(qts[[j]]) * matrix(colSums(qts[[j]] * t(apply((data-mu[[j]]), 1, multWT))), c(2,2))
		}
		
		# pie:
		for(j in 1:nclust){
			pie[[j]] <- 1/n * sum(qts[[j]])
		}
	    
	    # parameters after iteration:
	    muAft <- mu
	    CAft <- C
	    pieAft <- pie
	    
	    condition <- sqrt(sum(c(unlist(CBfr), unlist(muBfr), unlist(pieBfr)) - c(unlist(CAft), unlist(muAft), unlist(pieAft)))^2) > 0.1
	}
	ret <- list("qts" = qts, 'mu' = mu, 'C' = C, "pie" = pie)
	ret
}




# Exercise 3
em <- EM(data, nclust = 2)
cindex <- apply(Reduce(cbind, em$qts), 1 , which.max)
plot(data[cindex == 1,], cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data[cindex == 2,], cex = 0.3, pch = 16, col = 'darkblue')



# Exercise 4
n <- 100
mean1 <- c(1,2)
var1 <- c(1,3)

mean2 <-  c(-4,0)
var2 <- c(0.5,4)

mean3 <- c(-5,10)
var3 <- c(1,1)

mean4 <- c(-10,-5)
var4 <- c(1,5)

prior <- c(0.25, 0.5, 0.1, 0.4)

ni <- rbinom(n, size = 3, prob = prior[1:nclust-1])
n1 <- sum(ni == 0)
n2 <- sum(ni == 1)
n3 <- sum(ni == 2)
n4 <- sum(ni == 3)

data1 <- rGauss(mean1, var1, pi/6, n1)
data2 <- rGauss(mean2, var2, (pi-pi/3), n2)
data3 <- rGauss(mean3, var3, (pi/2), n3)
data4 <- rGauss(mean4, var4, (pi/2 - pi/6), n4)
data <- rbind(data1, data2, data3, data4)

# the covariance matrices of the underlying distributions:
# theta <- pi/6
# R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2) # Rotation matrix
# D <- matrix(c(var1[1],0,0,var1[2]),2,2)# Dilation matrix
# A <- D%*%R # First dilate, then rotate
# cov1 <- t(A)%*%A
# theta <- pi - pi/3
# R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),2,2) # Rotation matrix
# D <- matrix(c(var2[1],0,0,var2[2]),2,2)# Dilation matrix
# A <- D%*%R # First dilate, then rotate
# cov2 <- t(A)%*%A


xlim <- c(min(data[1]), max(data[1]))
ylim <- c(min(data[2]), max(data[2]))

plot(data1, cex = 0.3, pch = 16, col = 'forestgreen', xlim = xlim, ylim = ylim, xlab = 'x', ylab = 'y')
points(data2, cex = 0.3, pch = 16, col = 'darkblue')
points(data3, cex = 0.3, pch = 16, col = 'purple')
points(data4, cex = 0.3, pch = 16)



