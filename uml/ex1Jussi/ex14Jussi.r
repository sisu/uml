## Creates two dimensional multivariate normally distributed artificial data using given PC directions and respective variances

# creates data:
theta = pi/4
V = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),2,2)  #
Lambda = diag(c(1,3),2,2) # The eigenvalues are the variances of the PC's
Sigma = V%*%Lambda%*%t(V)
mean <- c(0,0)
data <- data.frame(mvrnorm(1000, mean, Sigma))
colnames(data) <- c("X", "Y")
write.table(data, "data4")

# plot
plot(data, pch = 16, cex = 0.5, col = 'blue', asp = 1);

# center data:
# Center data:
data <- as.matrix(data)
data <- t(apply(data,1,"-", colMeans(data)))
data <- data.frame(data)

cov <- cov(data)
eig <- eigen(cov) # The eigenvectors are approximately the directions of the principal components  and the eigenvalues are the respective variances

rm(data, theta, V, Lambda, Sigma, mean, cov)
