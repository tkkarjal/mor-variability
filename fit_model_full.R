## LOAD REQUIRED PACKAGES

library("rstan")
library("brms")
rstan_options(auto_write = TRUE)
options(max.print=10000)

## PREPARE THE DATA FRAME

d = read.csv("full_volume_dataframe.csv")

d$scanner <- tolower(d$scanner)
d$hrrt <- ifelse(d$scanner=="hrrt",1,0)
d$pet_mri <- ifelse(d$scanner=="pet-mri",1,0)
d$ge <- ifelse(d$scanner=="ge_advance",1,0)
d$sampo <- ifelse(d$scanner=="sampo",1,0)
d$male <- ifelse(d$sex=="m",1,0)
d$age_sd <- (d$age - mean(d$age))/sd(d$age)
d$log_bp <- log(d$bp)
d$smoker <- ifelse(d$smoker==1,1,0)
d$roi <- as.factor(d$roi)

## SAMPLING SPECIFICATIONS

ITER <- 12000
WARMUP <- 2000
CORES <- 3
CHAINS <- 3
AD <- 0.95

## PRIOR

custom_prior <- set_prior("normal(0,1)", class = "b")

## FIT THE MODEL TO THE DATA AND DRAW POSTERIOR SAMPLES

# full model
fit1 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + age_sd:male + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + age_sd:male + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
