data <- read.table("data4")

# Center data:
data <- as.matrix(data)
data <- t(apply(data,1,"-", colMeans(data)))
data <- data.frame(data)

# Principal component analysis
cov <- cov(data)
eig <- eigen(cov, symmetric = T) 
attach(eig)
index = which(values == max(values))
PC1 <- vectors[,index] # Only the first principal component is needed
detach(eig)

# Reduce dimension by minimizing reconstruction error
dimRedData <- apply(data, 1,"%*%", PC1)
projection <- sapply(dimRedData,"*", PC1)

#plotting
plot(data[1:50,], pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA)
points(projection[1,1:50],projection[2,1:50], pch = 16, cex = 0.5, col = 'red')
x <- rep(NA, times = 3*50)
x[seq(1,148, by = 3)] <- data[1:50,1]
x[seq(1,148, by = 3)+1] <- projection[1,1:50]
y <- rep(NA, times = 3*50)
y[seq(1,148, by = 3)] <- data[1:50,2]
y[seq(1,148, by = 3)+1] <- projection[2,1:50]
lines(x,y)


# The Average reconstruction error:
normVec <- function(x) sqrt(sum(x^2))
recError <- apply(data - data.frame(t(projection)), 1, normVec)
avRecError <- mean(recError)
# Proportion of variance
propVar <- sapply(eig$values, "/", sum(eig$values))
# The average reconstruction error, and the variance explained are roughly the same

rm(cov, data, dimRedData, eig, index, normVec, PC1, x, y, projection, recError)
