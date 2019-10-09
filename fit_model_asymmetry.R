## LOAD REQUIRED PACKAGES

library("rstan")
library("brms")
rstan_options(auto_write = TRUE)
options(max.print=10000)

## PREPARE THE DATA FRAME

d = read.csv("hemi_diff_dataframe.csv")
d$bp_diff <- d$bp_rh - d$bp_lh

## SAMPLING SPECIFICATIONS

ITER <- 12000
WARMUP <- 2000
CORES <- 3
CHAINS <- 3
AD <- 0.95

roi_labels <- unique(d$roi)
num_rois <- length(roi_labels)

## FIT THE MODEL TO THE DATA AND DRAW POSTERIOR SAMPLES

# full model
fit1 <- brm(bp_diff ~ 1 + (1 | roi:sex), data = d, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))

draws <- as.matrix(fit1)

FI <- matrix(ncol = num_rois, nrow = nrow(draws))
intercept_idx <- colnames(draws) == "b_Intercept"
for (i in seq(1,num_rois)) {
  roi_intercept_idx <- colnames(draws) == paste("r_roi:sex[",roi_labels[i],"_f,Intercept]",sep="")
  FI[,i] <- draws[,intercept_idx] + draws[,roi_intercept_idx]
}
colnames(FI) <- roi_labels

MI <- matrix(ncol = num_rois, nrow = nrow(draws))
for (i in seq(1,num_rois)) {
  roi_intercept_idx <- colnames(draws) == paste("r_roi:sex[",roi_labels[i],"_m,Intercept]",sep="")
  MI[,i] <- draws[,intercept_idx] + draws[,roi_intercept_idx]
}
colnames(MI) <- roi_labels
