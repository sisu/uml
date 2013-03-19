## Creates four sets of two domensional artificial data, and saves them in one data frame called "data", plots the four sets of data.

# normally distributed twodimensional artificial data:
createArtificialData <- function(theta, var){
	data <- cbind(rnorm(1000),rnorm(1000))
	R <- t(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),2,2)) # Rotation matrix
	D <- matrix(c(var[1],0,0,var[2]),2,2)# Dilation matrix
	A <- D%*%R # First dilate, then rotate
	data = apply(data, 1, "%*%", A)
	data = data.frame(data[1,],data[2,])
}

data1 <- createArtificialData(theta = pi/8, var = c(1,2))
data2 <- createArtificialData(theta = pi/8, var = c(1,1))
data3 <- createArtificialData(theta = pi/8, var = c(1,1/2))
data4 <- createArtificialData(theta = pi/8, var = c(1,1/4))

data <- data.frame(data1, data2,data3,data4)
colnames(data) = c("X1", "Y1", "X2", "Y2","X3", "Y3","X4", "Y4")
rm(data1, data2, data3,data4)

write.table(data, "data1")



# plotting:
par(mfrow = c(2,2))
par(mar=c(0,0,0,0)+2)
attach(data)
plot(X1,Y1, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA)
plot(X2,Y2, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA)
plot(X3,Y3, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA)
plot(X4,Y4, pch = 16, cex = 0.5, col = 'blue', xlim = c(-4,4), ylim = c(-4,4), xlab = NA, ylab = NA)
detach(data)

rm(createArtificialData)