# Machine learning methods for accurately predicting survival and guiding treatment in stage I and II hepatocellular carcinoma

Data preprocessing is handled by R code: [preprocessing.R](preprocessing.R)

Model construction, hyperparameters tuning, treatment recommendation and evaluation are handled with [pysurvival](https://github.com/square/pysurvival), [scikit-learn](https://github.com/scikit-learn/scikit-learn) and [lifelines](https://github.com/CamDavidsonPilon/lifelines) packages: [model_development.py](model_development.py)

Plotting is mainly using R language:[Brier_score_plot.R](Brier_score_plot.R) and [recommend_treatment.R](recommend_treatment.R)

The original data read in R code is not provided in this repository and needs to be extracted in the [SEER](https://seer.cancer.gov/) database according to inclusion criteria.

The [data](data_surv.csv) after data preprocessing is provided. 

[Paper link](https://pubmed.ncbi.nlm.nih.gov/)(To be updated)
