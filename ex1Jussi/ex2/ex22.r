# Load data:
data <- t(read.table("data.txt"))
row.names(data) <- c(1:10)
data <- data.frame(data)



# Center data:
data <- as.matrix(data)
data <- t(apply(data,1,"-", colMeans(data)))
data <- data.frame(data)



# Principal component analysis
cov <- cov(data)
eig <- eigen(cov)

sorted <- sort(eig$values, decreasing = T)




propVar <- sapply(sorted, "/", sum(sorted))
propVar <- c(sum(propVar[1:1]), sum(propVar[1:2]), sum(propVar[1:3]), sum(propVar[1:4]), sum(propVar[1:5]))
plot(propVar, ylim = c(0,1), pch = 15, xlab = "number of PCs", ylab = "proportion of variance explained")

rm(cov, eig, sorted)