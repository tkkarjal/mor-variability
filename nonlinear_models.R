## LOAD REQUIRED PACKAGES

library("rstan")
library("brms")

## PREPARE THE DATA FRAME

d = read.csv("variability_dataset_roi.csv")

d$scanner <- tolower(d$scanner)
d$age_sd <- (d$age - mean(d$age))/sd(d$age)
d$log_bp <- log(d$bp)

## SAMPLING SPECIFICATIONS (STAN)

rstan_options(auto_write = TRUE)
ITER <- 10000
WARMUP <- 4000
CORES <- 3
CHAINS <- 6
AD <- 0.999

## FIT THE MODELS

dat <- d[d$roi=="tha",] # change the ROI label here

form1 = bf(log_bp ~ (1 | scanner) + s(age), sigma ~ (1 | scanner))
fit1 <- brm(formula = form1, data = dat, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 20))

form2 = bf(log_bp ~ (1 | scanner) + s(age, by = gender), sigma ~ (1 | scanner))
fit2 <- brm(formula = form2, data = dat, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 20))
