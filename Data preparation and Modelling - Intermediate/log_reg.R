#' @title  Data preparation, Supervised Learning, Logistic Regression
#' @author Angelina Khatiwada
#' @date 03/06/2021


library(dplyr)
library(skimr)
library(ggplot2) 
library(tidyr)
library(fastDummies)
library(caret)
library(grid)
library(gridExtra)
library(caret)
library("epitools")

#-------------DATA PREPARATION-------------------

data  <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/in-vehicle-coupon-recommendation.csv", 
                 header=T, na.strings=c("","NA"))
head(data)
dim(data)
str(data) #data types

#glimpse(data)

#Data summarization

summary(data)
coupon_data %>% map(table)

skim(data) #checking for NA values, unique value in the column


#-------------PLOTTING-----------------------

coupon_data  <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/cleaned_data_raw_columns.csv")

#coupon_data$age_weightage <- scale(as.numeric(coupon_data$age_weightage), center = FALSE)

#coupon_data$income_weightage <- scale(as.numeric(coupon_data$income_weightage), center = FALSE)

#coupon_data$expiration_weightage <- scale(as.numeric(coupon_data$expiration_weightage), center = FALSE)

skim(coupon_data)

coupon_data$Y <- as.factor(coupon_data$Y)

#Destination
p1 <- ggplot(coupon_data, aes(x=destination, fill=Y)) +
    geom_bar(stat="count")

#passanger 
p2 <- ggplot(coupon_data, aes(x=passanger, fill=Y)) +
  geom_bar(stat="count")

#weather
p3 <- ggplot(coupon_data, aes(x=weather, fill=Y)) +
  geom_bar(stat="count")

#time
p4 <- ggplot(coupon_data, aes(x=time, fill=Y)) +
  geom_bar(stat="count")

#gender
p5 <- ggplot(coupon_data, aes(x=gender, fill=Y)) +
  geom_bar(stat="count")

#maritalStatus   
p6 <- ggplot(coupon_data, aes(x=maritalStatus, fill=Y)) +
  geom_bar(stat="count")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)

#education                       
p7 <- ggplot(coupon_data, aes(x=education, fill=Y)) +
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p7

#occupation                                          
p8 <- ggplot(coupon_data, aes(x=occupation_class, fill=Y)) +
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p8

#Bar                                                 
p9 <- ggplot(coupon_data, aes(x=Bar, fill=Y)) +
  geom_bar(stat="count")

#CoffeeHouse                                       
p10 <- ggplot(coupon_data, aes(x=CoffeeHouse, fill=Y)) +
  geom_bar(stat="count")

#CarryAway                       
p11 <- ggplot(coupon_data, aes(x=CarryAway, fill=Y)) +
  geom_bar(stat="count")

#RestaurantLessThan20                                
p12 <- ggplot(coupon_data, aes(x=RestaurantLessThan20, fill=Y)) +
  geom_bar(stat="count")

#direction_same                                               
p13 <- ggplot(coupon_data, aes(x=direction_same, fill=Y)) +
  geom_bar(stat="count")

#has_children                                                
p14 <- ggplot(coupon_data, aes(x=has_children, fill=Y)) +
  geom_bar(stat="count")

grid.arrange(p9, p10, p11, p12, p13, p14, ncol=2)


#For all columns
for (i in colnames(cleaned_data))
{p <- ggplot(cleaned_data, aes(x=cleaned_data[[i]], fill=cleaned_data$Y)) +
  geom_bar(stat="count")
print(p)
}

#for numeric variables

#Age histogram
p15 <- ggplot(data = coupon_data, aes(age_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

#Income histogram
p16 <- ggplot(data = coupon_data, aes(income_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

#Expiration histogram
p17 <- ggplot(data = coupon_data, aes(expiration_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

grid.arrange(p15, p16, p17, ncol=2)

#-----------------MODELING --------------------------------


#remove_first_dummy is TRUE, removes the first dummy variable created from each column. 
#This is done to avoid multicollinearity in a multiple regression model caused by included all dummy variables. 

#drops= c("direction_opp_1") #drop additional correlated variables

#coupon_data_encoded <- coupon_data_encoded[ , !(colnames(coupon_data_encoded) %in% drops)]

#str(coupon_data_encoded)

#creating factors 

coupon_data$temperature <-as.factor(coupon_data$temperature)
coupon_data$has_children <-as.factor(coupon_data$has_children)
coupon_data$toCoupon_GEQ15min <-as.factor(coupon_data$toCoupon_GEQ15min)
coupon_data$toCoupon_GEQ25min <-as.factor(coupon_data$toCoupon_GEQ25min)
coupon_data$direction_same <-as.factor(coupon_data$direction_same)
coupon_data$Y <-as.factor(coupon_data$Y)

drops= c("direction_opp", 'age', 'income', 'expiration_weightage', 'occupation') #drop additional correlated variables
coupon_data <- coupon_data[ , !(colnames(coupon_data) %in% drops)]

str(coupon_data)
coupon_data_encoded = coupon_data

#train/test split

set.seed(123)
split_train_test  <- createDataPartition(coupon_data$Y, p = .67,
                                  list = FALSE,
                                  times = 1)
 
#-----------------LOGISTIC REGRESSION----------------------

#mod_fit <- glm(Y ~., data = train, family=binomial(link='logit'))
#summary(mod_fit)

# CV  with 10 folds
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
log_reg <- train(Y ~.,
               data = train,
               trControl = train_control,
               method = "glm",
               family=binomial(link='logit'))

log_reg
summary(log_reg)

# Predictions

#probabilities <- log_reg %>% predict(test, type = 'prob')
#predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
#predicted.classes

# Model accuracy and Confusion matrix

log_reg_prob1 <- predict(log_reg, test, type = 'prob')
log_reg_pred1 <- ifelse(log_reg_prob1[2] > 0.5, 1, 0)
mean(log_reg_pred1 == test$Y)

table(Predicted = log_reg_pred1, Actual = test$Y)


confusionMatrix(
  as.factor(log_reg_pred1),
  as.factor(test$Y),
  positive = "1" 
)

log_reg_pred1 <- ifelse(log_reg_prob1[2] > 0.55, 1, 0)
mean(log_reg_pred1 == test$Y)

confusionMatrix(
  as.factor(log_reg_pred1),
  as.factor(test$Y),
  positive = "1" 
)

#High sensitivity : few FN
#Low specificity: Many FP - a problem?
#with threshold 0.5, changing the threshold
#Sensitivity : 0.7669          
#Specificity : 0.5788

 
library(pROC)

test_roc = roc(test$Y ~ log_reg_prob1$"1", plot = TRUE, print.auc = TRUE)

#Check multicollinarity

#--------------------KNN

#-----------------Linear Discriminant Analysis---------------------

library(MASS)
lda.fit=lda(Y~.,data = train)
lda.fit

plot(lda.fit)

lda.pred=predict(lda.fit,test)$class
table(lda.pred,test$Y)


mean(lda.pred==test$Y)


#---------------ASSOCIATION RULES --------------------

assoc <- xtabs(~Y+destination_No.Urgent.Place, data=coupon_data_encoded)
assoc


plot(assoc, col=c("green","blue"))

Test <- chisq.test(assoc, correct=FALSE)
Test

#Chi2 is 216, relation between the variables is significant ( p-value)
#Reject H0 about independence between variables
#Not a perfect association

riskratio.wald(table(coupon_data_encoded$destination_No.Urgent.Place,coupon_data_encoded$Y))
oddsratio.wald(table(coupon_data_encoded$destination_No.Urgent.Place,coupon_data_encoded$Y))

#Confidence interval does not include 1 - reject H0 about independence
#Odds: odds under no urgent place/ odds under other cases = 1.70

lr_fit <- glm(Y ~ destination_No.Urgent.Place, data = coupon_data_encoded,
              family=binomial(link='logit'))
summary(lr_fit)


exp(cbind(OR = coef(lr_fit), confint(lr_fit)))

#Odds ratio of 1.70  means that the odds that coupon accepted with destination_No Urgent Place was 1.70 times higher than the odds among controls.
#`destination_No Urgent Place` might be a factor.


#----------------KNN

coupon_data_encoded  <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/cleaned_data_encoded.csv")

str(coupon_data_encoded)

train <- coupon_data_encoded[ split_train_test,]
test  <- coupon_data_encoded[-split_train_test,]

train_label <- train$Y
test_label <- test$Y

##load the package class
library(class)
##run knn function
model_knn <- knn(train = train, test = test ,cl=train_label,k=100)


##create confusion matrix
tab <- table(model_knn,test_label)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
library(gmodels)

CrossTable(x=model_knn, y=test_label, prop.chisq=FALSE) 

#loop for different k
k <- 1
accuracy_scores <- c()
k_values <- c()
while (k < 10) {
  k_values <- c(k_values, k)
  model_knn <- knn(train = train, test = test ,cl=train_label,k=k)
  tab <- table(model_knn,test_label)
  accuracy_scores <- c(accuracy_scores, accuracy(tab))
  k = k+1
}
acc_test = cbind(data.frame(k_values), data.frame(accuracy_scores))
acc_test

plot(acc_test$k_values, acc_test$accuracy_scores, type="l", col="green", lwd=4, pch=15)

