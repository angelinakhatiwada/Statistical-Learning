#' @title  Data preparation, Supervised Learning, Logistic Regression
#' @author Angelina Khatiwada
#' @date 03/06/2021


library(dplyr)
library(skimr)
library(ggplot2) 
library(tidyr)
library(fastDummies)
library(caret)

#-------------DATA PREPARATION-------------------

coupon_data  <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/in-vehicle-coupon-recommendation.csv", 
                 header=T, na.strings=c("","NA"))
head(coupon_data)
dim(coupon_data)
str(coupon_data) #data types

#glimpse(data)

#Data summarization

summary(data)
coupon_data %>% map(table)

skim(data) #checking for NA values, unique value in the column

#dropping column car, toCoupon_GEQ5min

drops <- c("car", 'toCoupon_GEQ5min')
coupon_data <- coupon_data[ , !(colnames(coupon_data) %in% drops)]
head(coupon_data)

#drop_na in entire table

#coupon_data <- data_ncars[complete.cases(data_ncars), ]
#dim(coupon_data)

library(purrr)
# View(coupon_data %>% map(table))

coupon_data[] <- lapply(coupon_data, as.character)
coupon_data$Y <- as.numeric(coupon_data$Y)

# age column - Creating a new column to give numerical weightage
table(coupon_data$age)
coupon_data$age_weightage <- NA
coupon_data$age_weightage[which(coupon_data$age=="below21")] <- 1
coupon_data$age_weightage[which(coupon_data$age=="21")] <- 2
coupon_data$age_weightage[which(coupon_data$age=="26")] <- 3
coupon_data$age_weightage[which(coupon_data$age=="31")] <- 4
coupon_data$age_weightage[which(coupon_data$age=="36")] <- 5
coupon_data$age_weightage[which(coupon_data$age=="41")] <- 6
coupon_data$age_weightage[which(coupon_data$age=="46")] <- 7
coupon_data$age_weightage[which(coupon_data$age=="50plus")] <- 8


#coupon_data$age_weightage <- scale(as.numeric(coupon_data$age_weightage), center = FALSE)

table(coupon_data$age_weightage)


# temp & weather
# View(table(coupon_data$weather,coupon_data$temperature))

# Income - Creating a new column to give numerical weightage
table(coupon_data$income)
coupon_data$income_weightage <- NA
coupon_data$income_weightage[which(coupon_data$income=="Less than $12500")] <- 1
coupon_data$income_weightage[which(coupon_data$income=="$12500 - $24999")] <- 2
coupon_data$income_weightage[which(coupon_data$income=="$25000 - $37499")] <- 3
coupon_data$income_weightage[which(coupon_data$income=="$37500 - $49999")] <- 4
coupon_data$income_weightage[which(coupon_data$income=="$50000 - $62499")] <- 5
coupon_data$income_weightage[which(coupon_data$income=="$62500 - $74999")] <- 6
coupon_data$income_weightage[which(coupon_data$income=="$75000 - $87499")] <- 7
coupon_data$income_weightage[which(coupon_data$income=="$87500 - $99999")] <- 8
coupon_data$income_weightage[which(coupon_data$income=="$100000 or More")] <- 9


#coupon_data$income_weightage <- scale(as.numeric(coupon_data$income_weightage), center = FALSE)

# Income - Creating a new column to re-classify reference - https://en.wikipedia.org/wiki/International_Standard_Classification_of_Occupations
(table(coupon_data$occupation))
coupon_data$occupation_class <- NA
coupon_data$occupation_class[which(coupon_data$occupation %in% 
                                     c("Architecture & Engineering","Arts Design Entertainment Sports & Media"
                                       ,"Business & Financial","Computer & Mathematical","Education&Training&Library"
                                       ,"Healthcare Practitioners & Technical","Legal","Management"))] <- "Professionals"
coupon_data$occupation_class[which(coupon_data$occupation %in% 
                                     c("Building & Grounds Cleaning & Maintenance","Food Preparation & Serving Related"
                                       ,"Installation Maintenance & Repair","Transportation & Material Moving"))]  <- "Craft and related trades workers"
coupon_data$occupation_class[which(coupon_data$occupation %in% 
                                     c("Community & Social Services","Construction & Extraction","Healthcare Support"
                                       ,"Life Physical Social Science"))] <- "Technicians and associate professionals"
coupon_data$occupation_class[which(coupon_data$occupation %in% 
                                     c("Personal Care & Service","Protective Service","Sales & Related"))] <- "Service and sales workers"
coupon_data$occupation_class[which(coupon_data$occupation %in% 
                                     c("Farming Fishing & Forestry","Office & Administrative Support"
                                       ,"Production Occupations"))] <- "Others"  #own classification
coupon_data$occupation_class[which(coupon_data$occupation=="Retired")] <- 'Retired' 
coupon_data$occupation_class[which(coupon_data$occupation=="Student")] <- "Student"
coupon_data$occupation_class[which(coupon_data$occupation=="Unemployed")] <- "Unemployed"

# TIME VARIABLE
table(coupon_data$expiration)
coupon_data$expiration_weightage <- NA
coupon_data$expiration_weightage[which(coupon_data$expiration=="2h")] <- 2
coupon_data$expiration_weightage[which(coupon_data$expiration=="1d")] <- 24

#coupon_data$expiration_weightage <- scale(as.numeric(coupon_data$expiration_weightage), center = FALSE)

# passenger
coupon_data$passanger[which(coupon_data$passanger=="Friend(s)")] <- "Friends"
coupon_data$passanger[which(coupon_data$passanger=="Kid(s)")] <- "Kids"


coupon_data %>% map(table)

# missing imputation knn approach ------------------------------------------

{
  library(VIM)
  colMeans(is.na(coupon_data))*100
  which(colMeans(is.na(coupon_data))>0)
  cleaned_data <- kNN(coupon_data
                      , variable = c("Bar","CoffeeHouse","CarryAway","RestaurantLessThan20","Restaurant20To50")
                      , k = 5)
  cleaned_data <- cleaned_data[,1:ncol(coupon_data)]
  # coupon_data_final %>% map(table)
  colMeans(is.na(cleaned_data))*100
  
}

dim(cleaned_data)


# Plots -------------------------------------------------------------------
library(grid)
library(gridExtra)

cleaned_data$Y <- as.character(cleaned_data$Y)

#Destination
p1 <- ggplot(cleaned_data, aes(x=destination, fill=Y)) +
    geom_bar(stat="count")

#For all columns
for (i in colnames(cleaned_data))
{p <- ggplot(cleaned_data, aes(x=cleaned_data[[i]], fill=cleaned_data$Y)) +
  geom_bar(stat="count")
print(p)
}

#for numeric variables

#Age histogram
p2 <- ggplot(data = cleaned_data, aes(age_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

#Income histogram
p3 <- ggplot(data = cleaned_data, aes(income_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

#Expiration histogram
p4 <- ggplot(data = cleaned_data, aes(expiration_weightage, color = Y))+
  geom_freqpoly(binwidth = 5, size = 1)

grid.arrange(p1, p2, p3, p4, ncol=2)

cleaned_data$Y <- as.numeric(cleaned_data$Y)

# One-hot encoding --------------------------------------------------------

{
  cleaned_data$age <- NULL; cleaned_data$income <- NULL; cleaned_data$occupation<- NULL; cleaned_data$expiration <- NULL
  
  # library(caret)
  # dummy <- dummyVars(" ~ .", data=cleaned_data)
  # coupon_data_encoded <- data.frame(predict(dummy, newdata = cleaned_data)) 
  
  encoded <- fastDummies::dummy_cols(cleaned_data, remove_first_dummy = TRUE)
  
  coupon_data_encoded <- encoded[ , ((!(colnames(encoded) %in% colnames(cleaned_data))) 
                                     | (colnames(encoded) %in% c("Y","age_weightage","income_weightage","expiration_weightage")))]
}

head(coupon_data_encoded)

#remove_first_dummy is TRUE, removes the first dummy variable created from each column. 
#This is done to avoid multicollinearity in a multiple regression model caused by included all dummy variables. 


drops= c("direction_opp_1") #drop additional correlated variables

coupon_data_encoded <- coupon_data_encoded[ , !(colnames(coupon_data_encoded) %in% drops)]

#-----------------MODELING --------------------------------

#-----------------LOGISTIC REGRESSION----------------------

#train/test split

set.seed(123)
split_train_test  <- createDataPartition(coupon_data_encoded$Y, p = .67,
                                  list = FALSE,
                                  times = 1)
 
train <- coupon_data_encoded[ split_train_test,]
test  <- coupon_data_encoded[-split_train_test,]

#Logistic Regression

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

probabilities <- log_reg %>% predict(test)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
#predicted.classes


# Model accuracy and Confusion matrix

mean(predicted.classes == test$Y)
log_reg_prob1 <- predict(log_reg, test)
log_reg_pred1 <- ifelse(log_reg_prob1 > 0.55,"1","0")
table(Predicted = log_reg_pred1, Actual = test$Y)

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
test_roc = roc(test$Y ~ log_reg_prob1, plot = TRUE, print.auc = TRUE)


#Check multicollinarity



#-----------------Linear Discriminant Analysis---------------------

library(MASS)
lda.fit=lda(Y~.,data = train)
lda.fit

plot(lda.fit)

lda.pred=predict(lda.fit,test)$class
table(lda.pred,test$Y)


mean(lda.pred==test$Y)


#---------------ASSOCIATION RULES --------------------

assoc <- xtabs(~Y+`destination_No Urgent Place`, data=coupon_data_encoded)
assoc


plot(assoc, col=c("green","blue"))

Test <- chisq.test(assoc, correct=FALSE)
Test

#Chi2 is 216, relation between the variables is significant ( p-value)
#Reject H0 about independence between variables
#Not a perfect association

library("epitools")
riskratio.wald(table(coupon_data_encoded$`destination_No Urgent Place`,coupon_data_encoded$Y))
oddsratio.wald(table(coupon_data_encoded$`destination_No Urgent Place`,coupon_data_encoded$Y))

#Confidence interval does not include 1 - reject H0 about independence
#Odds: odds under no urgent place/ odds under other cases = 1.70

lr_fit <- glm(Y ~ `destination_No Urgent Place`, data = coupon_data_encoded,
              family=binomial(link='logit'))
summary(lr_fit)


exp(cbind(OR = coef(lr_fit), confint(lr_fit)))

#Odds ratio of 1.70  means that the odds that coupon accepted with destination_No Urgent Place was 1.70 times higher than the odds among controls.
#`destination_No Urgent Place` might be a factor.

