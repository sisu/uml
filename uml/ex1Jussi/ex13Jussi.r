## Projects the fourth data set on its PC's, computes variance of the projected data, and eigenvalues of the covariance matrix of the data.

# load data:
data <- read.table("data1")

# Center data:
data <- as.matrix(data)
data <- t(apply(data,1,"-", colMeans(data)))
data <- data.frame(data)

# Principal component analysis for the fourth data set:
attach(data)
cov <- cov(cbind(X4,Y4))
eig <- eigen(cov, symmetric = T)
detach(data)

rm(cov)

PC <- matrix(ncol = 2, nrow = 2)

attach(eig)
index = which(values == max(values))
PC[,1] = vectors[,index]
index = which(values == max(values[-index]))
PC[,2] = vectors[,index]
detach(eig)

rm(index)

attach(data)
projDataPC1 <- apply(cbind(X4,Y4), 1,"%*%", PC[,1])
projDataPC2 <- apply(cbind(X4,Y4), 1,"%*%", PC[,2])
detach(data)

rm(PC)

#plotting for Exercise 3
par(mfrow = c(2,1))
hist(projDataPC1, breaks = seq(min(projDataPC1), max(projDataPC1), length =20), main = 'Histogram of the data projected on the first PC', xlab = "Projected data")
hist(projDataPC2, breaks = seq(min(projDataPC2), max(projDataPC2), length =20), main = 'Histogram of the data projected on the second PC', xlab = "Projected data")

# The variance of the projected data should equal the eigen values of the covariance matrix of the data:
var = c(NA,NA)
var[1] <- var(projDataPC1)
var[2] <- var(projDataPC2)
rm(projDataPC1, projDataPC2)

# for comparison, run:
# var
# eig