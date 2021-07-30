# Statistical-Learning Group Project

*Rijin Baby & Angelina Khatiwada,\
MSc Data Science and Economics, UNIMI*

June, 2021

#### Dataset: [In-vehicle coupon recommendation](https://archive.ics.uci.edu/ml/datasets/in-vehicle+coupon+recommendation)

Data was collected via a survey on Amazon Mechanical Turk. The survey describes different driving scenarios including the destination, current time, weather, passenger, etc., and then ask the person whether he will accept the coupon if he is the driver (Y is a binary response variable).

#### Project Description:
*Project Kaggle Link: https://www.kaggle.com/rijinbaby/analysis-in-vehicle-coupon-recommendation*

##### Exloratory Analysis and Data Preparation:
- Basic exploratory analysis
- Missing & unique values check
- Dropping irrelevant variables
- Creating new variables
- Missing imputation using KNN approach
- Plotting variables

##### Modelling - Part 1: Classification (Supervised Learning)
- Logistic Regression with all the parameters
- Multicollinearity check
- Stepwise selection models (both directions, forward, backward)
- Train-test split and Cross-Validation
- Confusion matrix and Sensitivity/Specificity Trade-off
- Penalised models (Lasso and Elastic Net)
- Linear Discriminant Analysis
- Random Forest (parameters tuning, model evaluation)
- Boosting 

##### Modelling - Part 2: Clustering (Unsupervised Learning)
- Gower distance for mixed type data
- K-medoids Clustering 
- Agglomerative clustering (different linkages)
- Clustering Evaluation with Rand Index
- PCA

##### Modelling - Part 3: ANN (Supervised Learning)
- Train-test split
- Neural Network using Keras for Binary Classification
- Network parameters: Sigmoid/Hard_sigmoid activation function, Binary Cross-Entropy loss function, etc.
- Cross-Validation on train set
- Accuracy on test set

#### R Packages used
skimr, readr, plyr, dplyr, purrr, VIM, ggplot2, plotly, caret, grid, gridExtra, pROC, MASS, class, gmodels, randomForest, car, ClusterR, cluster, gbm, Rtsne, glmnet, dendextend, fossil, leaps

#### Python Libraries used
tensorflow, keras, scikit-learn, numpy, pandas, matplotlib


*To display html reports in a browser, please use the following links:*
 - *https://rawcdn.githack.com/rijinbaby/Statistical-Learning/main/Group_16_ANN_final.html*
 - *https://rawcdn.githack.com/rijinbaby/Statistical-Learning/main/Group_16_Supervised_Unsupervised_final.html*
