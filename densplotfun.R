densplotfun <- function(F,M,xmax,ymax,mains) {
  par(mar=c(2,1,1,1))
  par(mfrow = c(3,5))
  ffpi <- posterior_interval(F)
  mmpi <- posterior_interval(M)
  for (i in seq(1,15)) {
    f <- density(F[,i])
    m <- density(M[,i])
    
    fpi <- ffpi[i,]
    fidx <- f$x > fpi[1] & f$x < fpi[2]
    fy <- f$y[fidx]
    fy[1] <- 0
    fy[length(fy)] <- 0
    
    mpi <- mmpi[i,]
    midx <- m$x > mpi[1] & m$x < mpi[2]
    my <- m$y[midx]
    my[1] <- 0
    my[length(my)] <- 0
    
    xgrid <- seq(-xmax,xmax, by = 0.05)
    plot(f, main = mains[i], axes = FALSE, xlim = c(-xmax,xmax), ylim = c(0, ymax))
    ifelse(i > 10, axis(1, tck = -0.04, at = xgrid, labels = xgrid, las = 1), axis(1, tck = -0.04, at = xgrid, labels = rep("",length(xgrid)), las = 1))
    polygon(f$x[fidx],fy, col=rgb(1, 0, 0, 0.3), border = NA)
    lines(m)
    polygon(m$x[midx],my, col=rgb(0, 0, 1, 0.3), border = NA)
    abline(v=0, col="black", lty = 2)
  }
}