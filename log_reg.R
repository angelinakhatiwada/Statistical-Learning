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

data <- read.csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/in-vehicle-coupon-recommendation.csv", 
                 header=T, na.strings=c("","NA"))
head(data)
dim(data)
str(data) #data types

#glimpse(data)

#Data summarization

summary(data)

skim(data) #also checking for NA values

#sapply(data, function(x) sum(is.na(x))) #checking for NA values

for (i in colnames(data))
  {print(table(data[[i]]))
}

ggplot(data, aes(x = `occupation`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dropping column car

drops <- c("car")
data_ncars <- data[ , !(colnames(data) %in% drops)]
head(data_ncars)

#drop_na in entire table

data_ncars <- data_ncars[complete.cases(data_ncars), ]
dim(data_ncars)

#creating numeric value for expiration column and scaling

#range1 <- function(x){(x-min(x))/(max(x)-min(x))}

data_ncars$expiration[data_ncars$expiration == "1d"] <- 24
data_ncars$expiration[data_ncars$expiration =="2h"] <- 2

data_ncars$expiration <- scale(as.numeric(data_ncars$expiration), center = FALSE)

table(data_ncars$expiration)


#creating dummy variables and dropping unnecessary variables for final table

dum_cols = c("destination", "passanger", "age", "time",
             "weather","temperature", "coupon", "gender", "maritalStatus","education","occupation",
             "Bar", "income", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", "Restaurant20To50")

results <- fastDummies::dummy_cols(data_ncars, remove_first_dummy = TRUE, select_columns = dum_cols)


#remove_first_dummy is TRUE, removes the first dummy variable created from each column. 
#This is done to avoid multicollinearity in a multiple regression model caused by included all dummy variables. 

drop_cols = c(dum_cols, c("toCoupon_GEQ25min", "direction_opp")) #drop additional correlated variables

drop_cols

df <- results[ , !(colnames(results) %in% drop_cols)]
str(df)

sapply(df, function(x) sum(is.na(x)))


#-------------MODELING -------------------

#train/test split

set.seed(123)
trainIndex <- createDataPartition(df$Y, p = .67,
                                  list = FALSE,
                                  times = 1)
 
Train <- df[ trainIndex,]
Test  <- df[-trainIndex,]

#Logistic Regression

mod_fit <- train(Y ~  has_children + .,  data=Train, method="glm", family="binomial")
summary(mod_fit)


# Predictions

probabilities <- mod_fit %>% predict(Test)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")

# Model accuracy

predicted.classes

mean(predicted.classes == Test$Y)

