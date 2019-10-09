draws <- as.matrix(fit4)
num_rois <- 15

## Intercepts

# Females
AF <- matrix(ncol = num_rois, nrow = nrow(draws))
for (i in seq(1,num_rois)) {
  roi_intercept_idx <- colnames(draws) == paste("r_roi[",as.character(i),",Intercept]",sep="")
  AF[,i] <- draws[,1] + draws[,roi_intercept_idx]
}
colnames(AF) <- roi_labels

# Males
AM <- matrix(ncol = num_rois, nrow = nrow(draws))
male_idx <- colnames(draws) == "b_male"
for (i in seq(1,num_rois)) {
  roi_male_idx <- colnames(draws) == paste("r_roi[",as.character(i),",male]",sep="")
  AM[,i] <- AF[,i] + draws[,male_idx] + draws[,roi_male_idx]
}
colnames(AM) <- roi_labels

### SLOPES

## AGE

# Females
BF_age <- matrix(ncol = num_rois, nrow = nrow(draws))
age_idx <- colnames(draws) == "b_age_sd"
for (i in seq(1,num_rois)) {
  roi_age_idx <- colnames(draws) == paste("r_roi[",as.character(i),",age_sd]",sep="")
  BF_age[,i] <- draws[,age_idx] + draws[,roi_age_idx]
}
colnames(BF_age) <- roi_labels

# Males
BM_age <- matrix(ncol = num_rois, nrow = nrow(draws))
age_male_idx <- colnames(draws) == "b_male:age_sd"
for (i in seq(1,num_rois)) {
  roi_age_male_idx <- colnames(draws) == paste("r_roi[",as.character(i),",male:age_sd]",sep="")
  BM_age[,i] <- BF_age[,i] + draws[,age_male_idx] + draws[,roi_age_male_idx]
}
colnames(BM_age) <- roi_labels

## SMOKING

BS <- matrix(ncol = num_rois, nrow = nrow(draws))
smoker_idx <- colnames(draws) == "b_smoker"
for (i in seq(1,num_rois)) {
  roi_smoker_idx <- colnames(draws) == paste("r_roi[",as.character(i),",smoker]",sep="")
  BS[,i] <- draws[,smoker_idx] + draws[,roi_smoker_idx]
}
colnames(BS) <- roi_labels