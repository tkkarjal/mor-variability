![age_effects](http://emotion.utu.fi/wp-content/uploads/2019/04/g4927.png)
# mor-variability

This repository contains analysis scripts related to the manuscript **Interindividual variability and lateralization of μ-opioid receptors in the human brain** by *Tatu Kantonen, Tomi Karjalainen, Janne Isojärvi, Pirjo Nuutila, Jouni Tuisku, Juha Rinne, Jarmo Hietala, Valtteri Kaasinen, Kari Kalliokoski, Harry Scheinin, Jussi Hirvonen, Aki Vehtari, and Lauri Nummenmaa*.

## List of the scripts

### Variability: ROI-level analysis

- fit_and_compare_models_roi.R
    - Fits seven different models to the ROI-level data and compares their predictive accuracy using Bayesian 10-fold cross-validation. Model fitting is done using brms (https://paul-buerkner.github.io/brms/), and the cross-validation is done using the R package LOO (https://CRAN.R-project.org/package=loo)
- build_roi_specific_posteriors.R
    - Draws samples from the posterior distribution of Model 4 and sums them up to get ROI-specific posteriors for age, sex, and smoking
- visualize_results_roi.R
    - Visualizes the results from the ROI-level analysis
    
### Variability: Full-volume analysis

- create_clusters.m
    - Takes in i) high-resolution mean BP-image over subjects, and ii) an anatomical ROI atlas of same resolution to produce functionally (BP-wise) homogenous clusters within clearly defined anatomical boundaries.
- fit_model_full.R
    - Fits the Model 4 to the full-brain data (again, with brms)
- build_cluster_specific_posteriors.R
    - Draws samples from the posterior distribution of Model 4 and sums them up to get cluster-specific posteriors for age, sex, and smoking
- visualize_cluster_posteriors.R
    - Visualizes the cluster-specific posterior distributions
    
### Hemispheric asymmetry

- fit_model_asymmetry.R
    - Fits the model and builds the posterior distributions of interest
