plot_posterior_intervals2 <- function(X,Y) {

  XI95 <- posterior_interval(X,0.95)
  XI80 <- posterior_interval(X,0.80)
  XM <- colMeans(X)
  YI95 <- posterior_interval(Y,0.95)
  YI80 <- posterior_interval(Y,0.80)
  ymin <- floor(min(c(YI95,XI95))/0.05)*0.05
  ymax <- ceiling(max(c(YI95,XI95))/0.05)*0.05
  YM <- colMeans(Y)
  
  d <- 0.1
  
  plot(c(1-2*d,ncol(X)+2*d),c(0,0),"l",col="gray",bty = "n", ylim=c(ymin, ymax), xlim = c(1,ncol(X)), las = 2, ylab = "", xlab = "", axes = FALSE)
  axis(1, tck = -0.05, at=seq(1,ncol(X)), labels = colnames(X), las = 2, cex.axis = 0.7)
  axis(2, tck = -0.05, at=seq(ymin,ymax,0.05), las = 2, cex.axis = 0.7)
  
  xX <- seq(1,ncol(X)) - d
  xY <- seq(1,ncol(X)) + d
  for (i in seq(1,ncol(X))) {
    lines(c(i-d,i-d),XI95[i,],lwd=3, col = rgb(0,0,1,0.5))
    lines(c(i-d,i-d),XI80[i,],lwd=7, col = rgb(0,0,1,0.5))
    lines(c(i+d,i+d),YI95[i,],lwd=3, col = rgb(1,0,0,0.5))
    lines(c(i+d,i+d),YI80[i,],lwd=7, col = rgb(1,0,0,0.5))
  }
  points(xX,XM,"p", pch = "-", cex = 2)
  points(xY,YM,"p", pch = "-", cex = 2)
}