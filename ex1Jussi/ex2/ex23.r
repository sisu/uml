quarti <- function(U, A) sum(sum((A%*%U)^4))

mroot <- function(W){
	eig <- eigen(W)
	return(eig$vectors %*% diag(eig$values^(1/2)) %*% t(eig$vectors))
}

gradJ <- function(A,U) 4 * t(A) %*% (A %*% U)**3

A <- cbind(c(-.9511, -1.6435, 2.3655, -2.9154, -3.7010), c(.9511, -1.6435, 2.3655, -2.9154, 3.7010))

# Initial rotation
theta = 0
U <- t(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),2,2))

quartimax <- function(U,A,mu){
	for(i in 1:1000){
		U <- U + mu * gradJ(A,U)
		U <- solve(mroot(U%*%t(U))) %*% U
	}
	return(U)
}
