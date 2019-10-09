plot_posterior_intervals <- function(X) {
  
  XI95 <- posterior_interval(X,0.95)
  XI80 <- posterior_interval(X,0.80)
  XM <- colMeans(X)
  ymin <- floor(min(XI95)/0.05)*0.05
  ymax <- ceiling(max(XI95)/0.05)*0.05
  
  plot(c(1,ncol(X)),c(0,0),"l",col="gray",bty = "n", ylim=c(ymin, ymax), xlim = c(1,ncol(X)), las = 2, ylab = "", xlab = "", axes = FALSE)
  axis(1, tck = -0.05, at=seq(1,ncol(X)), labels = colnames(X), las = 2, cex.axis = 0.7)
  axis(2, tck = -0.05, at=seq(ymin,ymax,0.05), las = 2, cex.axis = 0.7)
  
  xX <- seq(1,ncol(X))
  for (i in seq(1,ncol(X))) {
    lines(c(i,i),XI95[i,],lwd=3, col = rgb(0,0.6,0.6,0.8))
    lines(c(i,i),XI80[i,],lwd=7, col = rgb(0,0.6,0.6,0.8))
  }
  points(xX,XM,"p", pch = "-", cex = 2)
}