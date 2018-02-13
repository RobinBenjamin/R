library(MASS)

#Multivariante Normalverteilung
mu <- c(5, 5)
sigma <- matrix(c(1, 0.25, 0.25, 1), 2, 2)
df <- (mvrnorm(10000, mu = mu, Sigma = sigma))
df1 <- as.data.frame(df) #weil seq data.frame will
colnames(df1) <- c("x", "y") 

#Pixel und in buckets abfüllen
pixel <- 20;
breaks_x <- seq(floor(min(df1$x)), ceiling(max(df1$x)), length.out = pixel)
breaks_y <- seq(floor(min(df1$y)), ceiling(max(df1$y)), length.out = pixel)

# Frequency table
freq <-  as.data.frame(table(as.numeric(cut(df1$x, breaks = breaks_x)), as.numeric(cut(df1$y, breaks = breaks_y))))

# Plot matrix
matrix <- diag(pixel) * 0
matrix[cbind(freq[, 1], freq[, 2])] <- freq[, 3]
p1 <- image(breaks_x, breaks_y, matrix, xlab = '', ylab = '')
text(breaks_x[freq$Var1], breaks_y[freq$Var2], freq$Freq, cex = 0.8)

# Poissonverteilung
cf <- pnorm(df)
df2 <- apply(cf, 2, qpois, lambda=1)
df2 <- as.data.frame(df2)
colnames(df2) <- c("x","y")

#inf Einträge von df2 durch 0 ersetzten
is.na(df2)<-sapply(df2, is.infinite)
df2[is.na(df2)]<-0

breaks_x2 <- seq(floor(min(df2$x)), ceiling(max(df2$x)), length.out = pixel)
breaks_y2 <- seq(floor(min(df2$y)), ceiling(max(df2$y)), length.out = pixel)

freq2 <-  as.data.frame(table(as.numeric(cut(df2$x, breaks = breaks_y2)), as.numeric(cut(df2$y, breaks = breaks_y2))))

matrix1 <- diag(pixel) * 0
matrix1[cbind(freq2[, 1], freq2[, 2])] <- freq2[, 3]
p2 <- image(breaks_y, breaks_y2, matrix, xlab = '', ylab = '')
text(breaks_y[freq2$Var1], breaks_y2[freq2$Var2], freq2$Freq, cex = 0.8)

#beide heatmaps in einen output
par(mfrow=c(1,2))

