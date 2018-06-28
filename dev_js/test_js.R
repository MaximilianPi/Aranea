# Create Radar Plot from polar coordinates.
nr <- c(1:20)
x <- sample(1:360, 20)
y <- sample(1:15, 20, replace = T)
d <- sample(40:500, 20)

dat <- do.call(cbind.data.frame, list(nr, x, y, d))

colnames(dat) <- c('nr','azi', 'dist', 'd1')

dat$x <- cos((dat$azi+90)*(pi/180))*dat$dist
dat$y <- sin((dat$azi+90)*(pi/180 )) * dat$dist

dat$x <- dat$x*-1

plot(dat$x
     , dat$y
     , xlim = c(-15, 15)
     , ylim = c(-15, 15)
     , axes = F
     , cex = dat$d1/200
     , xlab = NA
     , ylab = NA)
points(0, 0
     , col = 'red'
     , pch = 16)

text(dat$x
     , dat$y
     , labels = dat$nr
     , adj = c(1, 1)
     , cex = 0.7)

