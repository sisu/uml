# Exercise 1

s <- matrix(nrow=10000,ncol=32)
for(iii in 1:32){
	s[,iii] <- rlaplace(100, location = 0, scale = (1/sqrt(2)))
}

yHat = matrix(nrow=10000,ncol=32)
yHat[,1] <- s[,1]
for(iii in 2:32){
	yHat[,iii] <- rowSums(s[,1:iii])	
}

y <- matrix(nrow=10000,ncol=32)
for(iii in 1:32){
	y[,iii] <- yHat[,iii]/sqrt(var(yHat[,iii]))
}

# density estimate using kernel estimation
dens <- apply(y,2,density, bw = 0.25, n = 512, from = -4, to = 4)
# take logarithm of density function
for(iii in 1:32){
	dens[[iii]]$y <- log(dens[[iii]]$y)
}

# plotting
# par(mar=c(0,0,0,0)+2)
# par(mfrow = c(2,3))
# for(iii in c(1,2,4,8,16,32)){
	# plot(dens[[iii]], xlim = c(-4,4), ylim = c(-8,0), main = '', col = 'lightslateblue')
	# points(dens[[iii]]$x, log(dnorm(dens[[iii]]$x)), type = 'l', col = 'magenta')
# }
