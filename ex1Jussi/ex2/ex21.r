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

attach(eig)
sorted <- sort(values, decreasing = T)
PC1 <- vectors[, values == sorted[1]]
PC2 <- vectors[, values == sorted[2]]
PC3 <- vectors[, values == sorted[3]]
PC4 <- vectors[, values == sorted[4]]
PC5 <- vectors[, values == sorted[5]]
detach(eig)

write.table(cbind(PC1, PC2), "PC")



# represent data as a linear combination of the principal components:
projData <- data
projData$X1 <- apply(as.matrix(data), 1, "%*%", PC1)
projData$X2 <- apply(as.matrix(data), 1, "%*%", PC2)
projData$X3 <- apply(as.matrix(data), 1, "%*%", PC3)
projData$X4 <- apply(as.matrix(data), 1, "%*%", PC4)
projData$X5 <- apply(as.matrix(data), 1, "%*%", PC5)

# represent the coordinate axes as a linear combination of the PCs
axis1 <- 10*c(c(1,0,0,0,0)%*%PC1, c(1,0,0,0,0)%*%PC2)
axis2 <- 10*c(c(0,1,0,0,0)%*%PC1, c(0,1,0,0,0)%*%PC2)
axis3 <- 10*c(c(0,0,1,0,0)%*%PC1, c(0,0,1,0,0)%*%PC2)
axis4 <- 10*c(c(0,0,0,1,0)%*%PC1, c(0,0,0,1,0)%*%PC2)
axis5 <- 10*c(c(0,0,0,0,1)%*%PC1, c(0,0,0,0,1)%*%PC2)

# plot:
plot(projData$X1, projData$X2, asp = 1, col = 'blue', pch=16, xlim = c(-8,8), ylim = c(-8,8))
abline(v =0)
abline(h = 0)
points(x = c(0, axis1[1]), y = c(0, axis1[2]), type = 'l', col = 'red')
points(x = c(0, axis2[1]), y = c(0, axis2[2]), type = 'l', col = 'red')
points(x = c(0, axis3[1]), y = c(0, axis3[2]), type = 'l', col = 'red')
points(x = c(0, axis4[1]), y = c(0, axis4[2]), type = 'l', col = 'red')
points(x = c(0, axis5[1]), y = c(0, axis5[2]), type = 'l', col = 'red')

rm(axis1, axis2, axis3, axis4, axis5,projData,  cov, eig, sorted)