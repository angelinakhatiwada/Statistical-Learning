#'
#' R script to for the in_vehicle_coupon_recommendation data preparation
#' Basic details, checks, plots, pca
#' Author: Rijin Baby
#' Date: 02-June-2021
#' 


# Package requirements ----------------------------------------------------

{
  #Install packages
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load(skim,readr, dplyr, purrr, VIM, ggplot2, plotly,caret)
  
  #Load Packages
  {
    library(skimr)
    library(readr)
    library(dplyr)
    library(purrr)
    library(VIM)
    library(ggplot2)
    library(plotly)
  }
}

# data upload -------------------------------------------------------------------

{
  in_vehicle_coupon_recommendation <- read_csv("https://raw.githubusercontent.com/rijinbaby/Statistical-Learning/main/in-vehicle-coupon-recommendation.csv")
  coupon_data <- in_vehicle_coupon_recommendation
  # class(coupon_data)
  coupon_data <- as.data.frame(coupon_data)
  
  # dataset information
  
  # spec(coupon_data)
  # skim(coupon_data)
}


# missing column & unique value check -------------------------------------


{
  (colMeans(is.na(coupon_data))*100)
  coupon_data$car <- NULL # no data at all - 4 other with <2% missing
  
  # unique values of columns
  # View(coupon_data %>% summarise_all(funs(n_distinct(.))))
  which(apply(coupon_data, 2, function(x) length(unique(x)))==1)
  coupon_data$toCoupon_GEQ5min <- NULL # removing column with single value
}


# Basic Cleaning ----------------------------------------------------------

{
  library(purrr)
  # View(coupon_data %>% map(table))
  coupon_data %>% map(table)
  summary(coupon_data)
  
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
  table(coupon_data$income_weightage)
  
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
  
  # passenger
  coupon_data$passanger[which(coupon_data$passanger=="Friend(s)")] <- "Friends"
  coupon_data$passanger[which(coupon_data$passanger=="Kid(s)")] <- "Kids"
  
}

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


# Plots -------------------------------------------------------------------

{
  library(ggplot2)
  
  # stacked bar chart
  ggplot(cleaned_data, 
         aes(x = occupation, 
             fill = Y)) + 
    geom_bar(position = "stack")
  
  
  # for (colname in names(cleaned_data)[-24]) {
  #   plt <- ggplot(data = cleaned_data, aes_string(colname, y = "Y")) +
  #     geom_bar(stat = "identity")
  #   print(plt)
  # }
  
  
  plot_ly(data=cleaned_data, x = ~destination, y = ~Y==1, type = 'bar', name = '1') %>%
    add_trace(y = ~Y==0, name = '0') %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack')
}

# dplyr - plyr
# library(plyr)
# View(table(count(cleaned_data,c(1,2))))

cleaned_data$age <- NULL; cleaned_data$income <- NULL; cleaned_data$occupation<- NULL; cleaned_data$expiration <- NUll


# One-hot encoding --------------------------------------------------------

{
  library(caret)
  
  dummy <- dummyVars(" ~ .", data=cleaned_data)
  newdata <- data.frame(predict(dummy, newdata = coupon_data_final)) 
}


# newdata.pca <- prcomp(newdata[,-(which(colnames(newdata)=="Y"))], center = TRUE,scale. = TRUE)
