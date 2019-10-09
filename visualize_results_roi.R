linfun <- function(a,b,val) {
  mu <- a + b*val
  return(mu)
}

# Age
plot_posterior_intervals2(BF_age,BM_age)

age_grid <- seq(range(d$age_sd)[1], range(d$age_sd)[2], length.out = 100)
age_grid_lab <- age_grid*sd(d$age) + mean(d$age)
age_ticks <- c(20,30,40,50,60)
age_sd_ticks <- (age_ticks-mean(d$age))/sd(d$age)

par(mar=c(2,4,1,1))
par(mfrow = c(3,5))
for (i in seq(1,num_rois)) {
  mu_f <- sapply(age_grid, linfun, a = AF[,i], b = BF_age[,i])
  mu_f_mean <- apply(mu_f,2,mean)
  mu_f_pi <- apply(mu_f,2,PI,prob = 0.95)
  
  mu_m <- sapply(age_grid, linfun, a = AM[,i], b = BM_age[,i])
  mu_m_mean <- apply(mu_m,2,mean)
  mu_m_pi <- apply(mu_m,2,PI,prob = 0.95)
  yy <- c(mu_f_pi, mu_m_pi)
  ymin <- min(c(min(mu_f_pi),min(mu_m_pi)))
  ymax <- max(c(max(mu_f_pi),max(mu_m_pi)))
  yat <- c(ymin,0.5*(ymin+ymax),ymax)
  ygrid <- round(yat,1)
  
  plot(age_grid,mu_f_mean,"l",col="red",xlim=range(age_grid),ylim=range(yy),bty="n",axes=FALSE,lwd=1.5,ylab="")
  ifelse(i > 10, axis(1, tck = -0.03, at=age_sd_ticks,labels=age_ticks), axis(1, tck = -0.03, at=age_sd_ticks,labels=rep("",5)))
  axis(2, tck = -0.1, at = yat, labels = ygrid, las = 2,ylab="BP")
  shade(mu_f_pi,age_grid,col=rgb(1,0,0,alpha=0.1))
  lines(age_grid,mu_m_mean,col="blue",lwd=1.5)
  shade(mu_m_pi,age_grid,col=rgb(0,0,1,alpha=0.1))
  title(roi_labels[i],ylab="log BP")
}

# Smoking
plot_posterior_intervals(BS)
