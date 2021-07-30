
{
  library(readr)
  library(dplyr)
  library(randomForest)
  library(caret)
  library(pROC)
  library(gbm)
  
  cleaned_data_raw_columns <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/cleaned_data_raw_columns.csv")
  cleaned_data_raw_columns$age_weightage <- NULL; cleaned_data_raw_columns$income_weightage <- NULL;
  cleaned_data_raw_columns$occupation<- NULL; cleaned_data_raw_columns$expiration <- NULL
  cleaned_data_raw_columns[] <- lapply(cleaned_data_raw_columns, as.factor)
  cleaned_data_raw_columns$expiration_weightage <- as.numeric(as.character(cleaned_data_raw_columns$expiration_weightage))
}

str(cleaned_data_raw_columns)
# random forest -----------------------------------------------------------

{
  dataset_rf <- cleaned_data_raw_columns
  dataset_rf$direction_opp <- NULL
  dataset_rf$time <- NULL
  
  set.seed(123)
  
  # subseting dataset
  split_train_test  <- createDataPartition(dataset_rf$Y, p = .67,
                                           list = FALSE,
                                           times = 1)
  train <- dataset_rf[ split_train_test,]
  test  <- dataset_rf[-split_train_test,]
  y_col <- which(colnames(test)=="Y")
}
{
  # rF on complete dataset
  full_rF <- randomForest(Y~.,data=train)
  full_rF #TRAIN ERROR?
  
  plot(full_rF) # color for classes and black for OBB
  # plot(full_rF$err.rate)
  
  # Perform training with parameters
  rf_classifier = randomForest(Y ~., data = train, ntree=200, mtry=5, importance=TRUE)
  rf_classifier
  plot(rf_classifier)
  varImpPlot(rf_classifier)
  
  # Most important variables - Coupon, coffee house, age, income, occupation, bar, restaurantlessthan20, expiration
  
  # -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when that 
  # particular variable is omitted from the training set. Caveat: if two variables are somewhat 
  # redundant, then omitting one of them may not lead to massive gains in prediction performance, 
  # but would make the second variable more important.
  # 
  # -MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this, if you use this 
  # feature to split the data, how pure will the nodes be? Highest purity means that each node 
  # contains only elements of a single class. Assessing the decrease in GINI when that feature 
  # is omitted leads to an understanding of how important that feature is to split the data correctly.
  
  # Validation set assessment #1: looking at confusion matrix
  prediction_for_table <- predict(rf_classifier,test[,-y_col])
  # table(actual=test[,y_col],predicted=prediction_for_table)
  confusionMatrix(
    as.factor(prediction_for_table),
    as.factor(test$Y),
    positive = "1" 
  )
  
  # classification perfomance
  auc(test$Y,as.numeric(as.character(prediction_for_table)))
  test_roc = roc(test$Y , as.numeric(as.character(prediction_for_table)), plot = TRUE, print.auc = TRUE)
}


# boosting ----------------------------------------------------------------

{
  train_boost <- train; train_boost$Y <- as.numeric(as.character(train_boost$Y))
  test_boost <- test; test_boost$Y <- as.numeric(as.character(test_boost$Y))
  
  # gaussian - regression, bernoulli - classification
  rf_boost <- gbm(Y~.,data=train_boost,distribution="bernoulli",n.trees=5000,shrinkage=0.01,interaction.depth=1, cv.folds=3) #, verbose=F
  
  best.iter = gbm.perf(rf_boost, method="cv") #Check the best iteration number
  
  # plot - Summary of the model results, with the importance plot of predictors
  par(mar = c(5, 8, 1, 1))
  summary(rf_boost, cBars = 10,las = 2)
  
  pred_boost = predict(rf_boost, test_boost[,-y_col], type = 'response')
  pred_boost_nw <- ifelse(pred_boost > 0.50,1,0)
  
  confusionMatrix(
    as.factor(pred_boost_nw),
    as.factor(test_boost$Y),
    positive = "1" 
  )
}

{
  # Using the caret package the get the model preformance in the best iteration
  set.seed(123)
  fitControl = trainControl(method="cv", number=3, returnResamp = "all")
  
  train_boost$Y <- as.factor(train_boost$Y)
  test_boost$Y <- as.factor(test_boost$Y)
  
  model2 = train(Y~., data=train_boost, method="gbm",distribution="bernoulli", trControl=fitControl,
                 verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))  #
  
  model2
  confusionMatrix(model2)
  
  mPred = predict(model2, test_boost[,-y_col]) #, na.action = na.pass
  postResample(mPred, test_boost$Y)
  
  confusionMatrix(mPred, test_boost$Y)
  
  
  confusionMatrix(
    as.factor(mPred),
    as.factor(test_boost$Y),
    positive = "1"
  )
}
