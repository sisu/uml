## Exercise set 3


# Exercise 1
plot_colour <- function(x1,x2,U1) {

  # x1, x2 data vecots
  # U1 contains the values of the projection (which we use for the colouring of
  # the data in x1 and x2)

  ncol = 50 # number of colours used
  myCol = rainbow(ncol, start = 0, end = 5/6)

  ra <-range(U1)
  d <-(ra[2]-ra[1])/ncol # space the range of U1 into equally wide intervals

  # make a vector containing the color corresponding to the value in U1
  U1col <- round((U1-ra[1])/d+1)
  U1col[U1col==(ncol+1)] <- ncol
  U1col <- myCol[U1col]

  # plot
  plot(x1,x2,col=U1col, pch = 16)

}


data <- as.matrix(read.table('data_proj.txt'))
N <- length(data[1,])
ndim <- length(data[,1])

# linear MDS
D <- (t(data) %*% data)/N
eig <- eigen(D)
index <- eig$values == sort(eig$values, decreasing = T)[1]
MDS <- (eig$vectors)[,index]

# plotting
plot_colour(data[1,], data[2,], abs(MDS))



## Exercise 2
PCA <- prcomp(t(data))
projection <- apply(data,2, '%*%', PCA$rotation[,1])
plot_colour(data[1,], data[2,], abs(projection))



## Exercise 3
# the Euclidian distance
eDist <- function(x,y) sqrt(colSums((x-y)^2))

# find indices for k smallest in a vector
which.kmin <- function(x, k){
	match(sort(x)[1:k], x)
}

SOM <- function(data, nmodel, niter){
	ndim <- length(data[,1])
	model <- vector('list', nmodel)
	for(iii in 1:nmodel) model[[iii]] <- runif(2, min(data), max(data))
	for(j in 1 : niter){
		# distances of points to model vectors
		d <- lapply(model, eDist, y = data)
		#the closest model vector for each point:
		closest.model <- max.col(-t(Reduce(rbind, d)))
		# the neighbours of each model vector:
		neighbours = vector('list', nmodel)
		index <- c(nmodel, 1: nmodel, 1)
		for(iii in 1:nmodel){
			neighbours[[iii]] <- index[iii:(iii+2)]
		}
		# update the model vectors by mean of the data closest to them and their neighbours:
		for(iii in 1:nmodel){
			model[[iii]] <- rowMeans(data[, sapply(closest.model, '%in%', neighbours[[iii]])])
		}
	}
	list('model' = model, 'closest.model' = closest.model, 'neighbours' = neighbours)
}

nmodel = 20
som <- SOM(data,nmodel,100)


# preliminary plotting:
plot(t(data), xlim = c(-10,10), asp = 1, pch = 16, cex = 0.5)
for(j in 1:nmodel){
	points(x = som$model[[j]][1], y = som$model[[j]][2], col = 'red', pch = 16)
}
which.cluster <- 1
for(j in som$neighbours[[which.cluster]]){
	points(x = som$model[[j]][1], y = som$model[[j]][2], col = 'green', pch = 16)
}
points(x = som$model[[which.cluster]][1], y = som$model[[which.cluster]][2], col = 'yellow', pch = 16)

plot(t(data[, sapply(som$closest.model, '%in%', som$neighbours[[which.cluster]])]), xlim = c(-10,10), asp = 1, pch = 16, cex = 0.5)
points(t(data[, -sapply(som$closest.model, '%in%', som$neighbours[[which.cluster]])]), cex = 0.5)
for(j in 1:nmodel){
	points(x = som$model[[j]][1], y = som$model[[j]][2], col = 'red', pch = 16)
}
for(j in som$neighbours[[which.cluster]]){
	points(x = som$model[[j]][1], y = som$model[[j]][2], col = 'green', pch = 16)
}
points(x = som$model[[which.cluster]][1], y = som$model[[which.cluster]][2], col = 'yellow', pch = 16)



# Exercise 4
I <- vector('list', 6)
for(iii in 1:6){
	file <- paste(paste('I', iii, sep = ''), '.txt', sep = '')
	I[[iii]] <- as.matrix(read.table(file))
}
genPatches <- function(img) {
	r <- floor(dim(img)[1]/10)
	c <- floor(dim(img)[2]/10)
	data.frame(lapply(0:(r-1), function(y) sapply(0:(c-1), function(x) img[10*y+(1:10),10*x+(1:10)])))
}
patches <- data.frame(lapply(I, genPatches))



# Exercise 5

