## LOAD REQUIRED PACKAGES

library("rstan")
library("brms")
library("loo")

## PREPARE THE DATA FRAME

d = read.csv("variability_dataset_roi.csv")

d$scanner <- tolower(d$scanner)
d$bmi <- d$weight/(d$height/100)^2
d$hrrt <- ifelse(d$scanner=="hrrt",1,0)
d$sampo <- ifelse(d$scanner=="sampo",1,0)
d$pet_mri <- ifelse(d$scanner=="pet-mri",1,0)
d$ge <- ifelse(d$scanner=="ge_advance",1,0)
d$male <- ifelse(d$gender=="m",1,0)
d$age_sd <- (d$age - mean(d$age))/sd(d$age)
d$bmi_sd <- (d$bmi - mean(d$bmi))/sd(d$bmi)
d$log_bp <- log(d$bp)
d$smoker[is.na(d$smoker)] <- 0

## SAMPLING SPECIFICATIONS (STAN)

rstan_options(auto_write = TRUE)
ITER <- 12000
WARMUP <- 2000
CORES <- 3
CHAINS <- 3
AD <- 0.95

## PRIOR

custom_prior <- set_prior("normal(0,1)", class = "b")

## FIT THE MODELS

fit1 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + age_sd:male + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + age_sd:male + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit2 <- brm(log_bp ~ (1 | subject) + (1 + age_sd + bmi_sd + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + age_sd + bmi_sd + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit3 <- brm(log_bp ~ (1 | subject) + (1 + male + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit4 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + age_sd:male + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + age_sd:male + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit5 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + age_sd:male + bmi_sd + bmi_sd:male + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + age_sd:male + bmi_sd + bmi_sd:male + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit6 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + bmi_sd + bmi_sd:male + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))
fit7 <- brm(log_bp ~ (1 | subject) + (1 + male + age_sd + age_sd:male + bmi_sd + smoker + hrrt + pet_mri + ge + sampo | roi) + 1 + male + age_sd + age_sd:male + bmi_sd + smoker + hrrt + pet_mri + ge + sampo, data = d, prior = custom_prior, cores = CORES, chains = CHAINS, iter = ITER, warmup = WARMUP, control = list(adapt_delta = AD, max_treedepth = 15))

## K-FOLD CROSS-VALIDATION

K <- 10
ids <- kfold_split_stratified(K = K, x = as.numeric(d$subject))
loo1 <- kfold(fit1, K = K, folds = ids, cores = CORES)
loo2 <- kfold(fit2, K = K, folds = ids, cores = CORES)
loo3 <- kfold(fit3, K = K, folds = ids, cores = CORES)
loo4 <- kfold(fit4, K = K, folds = ids, cores = CORES);
loo5 <- kfold(fit5, K = K, folds = ids, cores = CORES);
loo6 <- kfold(fit6, K = K, folds = ids, cores = CORES);
loo7 <- kfold(fit7, K = K, folds = ids, cores = CORES);

compare(loo1,loo2,loo3,loo4,loo5,loo6,loo7)
