### VIZ OF CARFE BP DATA ###

library(rstan)
library(rethinking)
library(brms)

draws <- as.matrix(fit1)

num_rois <- 320
clusters <- d$region[1:num_rois]

source("densplotfun.R")

## ROI-specific intercepts

# Females
AF <- matrix(ncol = num_rois, nrow = nrow(draws))
for (i in seq(1,num_rois)) {
  roi_intercept_idx <- colnames(draws) == paste("r_roi[",as.character(i),",Intercept]",sep="")
  AF[,i] <- draws[,1] + draws[,roi_intercept_idx]
}
colnames(AF) <- clusters

# Males
AM <- matrix(ncol = num_rois, nrow = nrow(draws))
male_idx <- colnames(draws) == "b_male"
for (i in seq(1,num_rois)) {
  roi_male_idx <- colnames(draws) == paste("r_roi[",as.character(i),",male]",sep="")
  AM[,i] <- AF[,i] + draws[,male_idx] + draws[,roi_male_idx]
}
colnames(AM) <- clusters

## AGE

age_grid <- seq(range(d$age_sd)[1], range(d$age_sd)[2], length.out = 100)
age_grid_lab <- age_grid*sd(d$age) + mean(d$age)
linfun <- function(a,b,val) {
  mu <- a + b*val
  return(mu)
}

age_ticks <- c(20,30,40,50,60)
age_sd_ticks <- (age_ticks-mean(d$age))/sd(d$age)

# Females
BF_age <- matrix(ncol = num_rois, nrow = nrow(draws))
age_idx <- colnames(draws) == "b_age_sd"
for (i in seq(1,num_rois)) {
  roi_age_idx <- colnames(draws) == paste("r_roi[",as.character(i),",age_sd]",sep="")
  BF_age[,i] <- draws[,age_idx] + draws[,roi_age_idx]
}
colnames(BF_age) <- clusters

# Males
BM_age <- matrix(ncol = num_rois, nrow = nrow(draws))
age_male_idx <- colnames(draws) == "b_male:age_sd"
for (i in seq(1,num_rois)) {
  roi_age_male_idx <- colnames(draws) == paste("r_roi[",as.character(i),",male:age_sd]",sep="")
  BM_age[,i] <- BF_age[,i] + draws[,age_male_idx] + draws[,roi_age_male_idx]
}
colnames(BM_age) <- seq(1,320)

## SMOKING

BS <- matrix(ncol = num_rois, nrow = nrow(draws))
smoker_idx <- colnames(draws) == "b_smoker"
for (i in seq(1,num_rois)) {
  roi_smoker_idx <- colnames(draws) == paste("r_roi[",as.character(i),",smoker]",sep="")
  BS[,i] <- draws[,smoker_idx] + draws[,roi_smoker_idx]
}

densplotfun(BS,BS,0.4,10,roi_full_labels)
