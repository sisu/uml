## Exercise 1
digits <- as.matrix(read.table('digits.txt'))

visual(t(digits[,1:64]))

# Remove average from images:
digits <- apply(digits, 1, '-', colMeans(digits))

visual(digits[1:64,])

# normalize to unit norm:
digits <- apply(digits, 2, function(x) x/sqrt(sum(x^2)))

# center data:
digits <- apply(digits, 2, '-', rowMeans(digits))

visual(digits[1:64,])



#Exercise 2
PCA <- prcomp(digits)

# proportion of variance explained plot:
propvar <- PCA$sdev^2
propvar <- propvar/sum(propvar)
A <- lower.tri(PCA$rotation, diag = T)
plot(1:20, (A %*% propvar)[1:20])

# plot PCAs:
visual(t(PCA$rotation[,1:20]))



# Exercise 3



