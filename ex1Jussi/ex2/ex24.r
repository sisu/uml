quarti <- function(U, A) sum(sum((A%*%U)^4))

mroot <- function(W){
	eig <- eigen(W)
	return(eig$vectors %*% diag(eig$values^(1/2)) %*% t(eig$vectors))
}

gradJ <- function(A,U) 4 * t(A) %*% (A %*% U)**3

quartimax <- function(U,A,mu){
	for(i in 1:1000){
		U <- U + mu * gradJ(A,U)
		U <- solve(mroot(U%*%t(U))) %*% U
	}
	return(U)
}

# Load data:
A <- read.table("PC")
A <- data.frame(A)

U <- diag(dim(A)[2])

# Rotate
mu = 0.1
A <- as.matrix(A)%*%quartimax(U,as.matrix(A),mu)


# Center data:
data <- read.table("data.txt")
data <- t(as.matrix(data))
data <- t(apply(data,1,"-", colMeans(data)))

# Project data
projData <- data
projData$X1 <- apply(data, 1, "%*%", A[,1])
projData$X2 <- apply(data, 1, "%*%", A[,2])

# represent the coordinate axes as a linear combination of the PCs
axis1 <- 10*c(c(1,0,0,0,0)%*%A[,1], c(1,0,0,0,0)%*%A[,2])
axis2 <- 10*c(c(0,1,0,0,0)%*%A[,1], c(0,1,0,0,0)%*%A[,2])
axis3 <- 10*c(c(0,0,1,0,0)%*%A[,1], c(0,0,1,0,0)%*%A[,2])
axis4 <- 10*c(c(0,0,0,1,0)%*%A[,1], c(0,0,0,1,0)%*%A[,2])
axis5 <- 10*c(c(0,0,0,0,1)%*%A[,1], c(0,0,0,0,1)%*%A[,2])


plot(projData$X1, projData$X2, asp = 1, col = 'blue', pch=16, xlim = c(-8,8), ylim = c(-8,8))
abline(v =0)
abline(h = 0)
points(x = c(0, axis1[2]), y = c(0, axis1[1]), type = 'l', col = 'red')
points(x = c(0, axis2[2]), y = c(0, axis2[1]), type = 'l', col = 'red')
points(x = c(0, axis3[2]), y = c(0, axis3[1]), type = 'l', col = 'red')
points(x = c(0, axis4[2]), y = c(0, axis4[1]), type = 'l', col = 'red')
points(x = c(0, axis5[2]), y = c(0, axis5[1]), type = 'l', col = 'red')

