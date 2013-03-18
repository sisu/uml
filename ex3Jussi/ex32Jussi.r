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
plot_colour(data[1,], data[2,], MDS)



## Exercise 2
PCA <- prcomp(t(data))
projection <- apply(data,2, '%*%', PCA$rotation[,1])
plot_colour(data[1,], data[2,], -projection)



