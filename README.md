This repository contains code from the paper "Estimating Average Causal Effects with Incomplete Exposure and Confounders", co-authored by Wen and McGee. 
Each folders contain codes to generate data for the MAR, MNAR1, MNAR2 and MNAR3 missingness mechanisms as described in the main manuscript. 
In each of these folders, the files to reproduce the results found in the main paper (and in the supplementary materials) include:

datagen.R: contains the function to generate data sets
deterministic_datagen_wide_true.R: code to produce the true parameter estimates
tmle.R: codes to produce the parameter estimates and standard errors from the TMLE estimators described in the main manuscript for each scenario
ICE.R: codes to produce the parameter estimates and standard errors from an ICE estimator
IPW.R: codes to produce the parameter estimates and standard errors from an IPW estimator

Additionally, 
- Folder "cc_tmle" contains simulation study codes for complete case analysis followed by TMLE
- Folder "MI_tmle" contains simulation study codes for MICE (multiple imputation using chained equations) followed by TMLE
- Folder "Misspecify" contains simulation study codes for model misspecification scenarios described in Section 6 of the main manuscript
