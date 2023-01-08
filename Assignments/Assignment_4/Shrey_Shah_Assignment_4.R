# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_4")

# Import the dataset using read_csv()
raw_dataset <- read_csv("Starbucks_data-3.csv")
dataset <- raw_dataset

# Shape of dataset
dim(dataset)

# Count of missing values
sum(is.na(dataset))

# Delete rows with missing values
dataset <- na.omit(dataset)

# Shape of new dataset after listwise deletion
dim(dataset)

library(gmodels)

# Dropping the timestamp columns
new_dataset = subset(dataset, select = -c(Timestamp))

names(new_dataset)[1] <- "Gender"
names(new_dataset)[2] <- "Age"
names(new_dataset)[3] <- "PersonType"
names(new_dataset)[4] <- "AnnualIncome"
names(new_dataset)[5] <- "FrequencyOfVisit"
names(new_dataset)[6] <- "EnjoyType"
names(new_dataset)[7] <- "TimeSpent"
names(new_dataset)[8] <- "NearestOutlet"
names(new_dataset)[9] <- "MembershipCard"
names(new_dataset)[10] <- "FrequentItemPurchase"
names(new_dataset)[11] <- "AverageSpend"
names(new_dataset)[12] <- "StarbucksQuality"
names(new_dataset)[13] <- "PriceRangeRating"
names(new_dataset)[14] <- "SalesPromoPurchaseDec"
names(new_dataset)[15] <- "AmbienceRating"
names(new_dataset)[16] <- "WifiQualityRating"
names(new_dataset)[17] <- "ServiceRating"
names(new_dataset)[18] <- "ChooseStarbucks"
names(new_dataset)[19] <- "HearPromotions"
names(new_dataset)[20] <- "ContinueToBuy"
names(new_dataset)[21] <- "BeforePromoSatRate"
names(new_dataset)[22] <- "AfterPromoSatRate"

new_dataset

number_of_variables <- floor(sqrt(nrow(dataset)))
number_of_variables

str(new_dataset)

# Converting categorical Gender to numeric
table(new_dataset$Gender)
new_dataset$Gender_num <- revalue(new_dataset$Gender, c("Male"="1", "Female"="0"))
new_dataset$Gender_num <- as.numeric(new_dataset$Gender_num)
table(new_dataset$Gender_num)

# Converting categorical Age to numeric
table(new_dataset$Age)
new_dataset$Age_num <- revalue(new_dataset$Age, c("Below 20"="1", "From 20 to 29"="2", 
                                                  "From 30 to 39"="3", "40 and above"="4"))
new_dataset$Age_num <- as.numeric(new_dataset$Age_num)
table(new_dataset$Age_num)

# Converting categorical PersonType to numeric
table(new_dataset$PersonType)
new_dataset$PersonType_num <- revalue(new_dataset$PersonType, c("Student"="1", "Housewife"="2", "Self-employed"="3", 
                                                                "Employed"="4"))
new_dataset$PersonType_num <- as.numeric(new_dataset$PersonType_num)
table(new_dataset$PersonType_num)

# Converting categorical AnnualIncome to numeric
table(new_dataset$AnnualIncome)
new_dataset$AnnualIncome_num <- revalue(new_dataset$AnnualIncome, c("Less than RM25,000"="1", "RM25,000 - RM50,000"="2",
                                                                    "RM50,000 - RM100,000"="3", "RM100,000 - RM150,000"="4",
                                                                    "More than RM150,000"="5"))
new_dataset$AnnualIncome_num <- as.numeric(new_dataset$AnnualIncome_num)
table(new_dataset$AnnualIncome_num)

# Converting categorical FrequencyOfVisit to numeric
table(new_dataset$FrequencyOfVisit)
new_dataset$FrequencyOfVisit_num <- revalue(new_dataset$FrequencyOfVisit, c("Never"="1", "Rarely"="2", "Monthly"="3",
                                                                            "Weekly"="4", "Daily"="5"))
new_dataset$FrequencyOfVisit_num <- as.numeric(new_dataset$FrequencyOfVisit_num)
table(new_dataset$FrequencyOfVisit_num)

# Converting categorical EnjoyType to numeric
table(new_dataset$EnjoyType)
#Here we observe that there are rows which have values as 'I dont like coffee', 'never', 'Never buy'
#which I have considered as Never
new_dataset$EnjoyType_num <- revalue(new_dataset$EnjoyType, c("Never"="1", "never"="1", "Never buy"="1", 
                                                              "I dont like coffee"="1", "Dine in"="2", "Drive-thru"="3",
                                                              "Take away"="4"))
new_dataset$EnjoyType_num <- as.numeric(new_dataset$EnjoyType_num)
table(new_dataset$EnjoyType_num)

# Converting categorical TimeSpent to numeric
table(new_dataset$TimeSpent)
new_dataset$TimeSpent_num <- revalue(new_dataset$TimeSpent, c("Below 30 minutes"="1", "Between 30 minutes to 1 hour"="2", 
                                                              "Between 1 hour to 2 hours"="3","Between 2 hours to 3 hours"="4",
                                                              "More than 3 hours"="5"))
new_dataset$TimeSpent_num <- as.numeric(new_dataset$TimeSpent_num)
table(new_dataset$TimeSpent_num)

# Converting categorical NearestOutlet to numeric
table(new_dataset$NearestOutlet)
new_dataset$NearestOutlet_num <- revalue(new_dataset$NearestOutlet, c("within 1km"="1", "1km - 3km"="2", "more than 3km"="3"))
new_dataset$NearestOutlet_num <- as.numeric(new_dataset$NearestOutlet_num)
table(new_dataset$NearestOutlet_num)

# Converting categorical MembershipCard to numeric
table(new_dataset$MembershipCard)
new_dataset$MembershipCard_num <- revalue(new_dataset$MembershipCard, c("Yes"="1", "No"="0"))
new_dataset$MembershipCard_num <- as.numeric(new_dataset$MembershipCard_num)
table(new_dataset$MembershipCard_num)

# Converting categorical FrequenctItemPurchase to numeric
table(new_dataset$FrequentItemPurchase)
new_dataset$FrequentItemPurchase_num <- revalue(new_dataset$FrequentItemPurchase, c("cake"="1", "Coffee"="2",
                                                                                    "Coffee;Cold drinks"="3",
                                                                                    "Coffee;Cold drinks;Pastries;Sandwiches"="4",
                                                                                    "Coffee;Juices;Pastries;Sandwiches"="5",
                                                                                    "Coffee;Pastries"="6", "Coffee;Pastries;Sandwiches"="7",
                                                                                    "Coffee;Sandwiches"="8","Cold drinks"="9",
                                                                                    "Cold drinks;Juices;Pastries"="10",
                                                                                    "Cold drinks;Never"="11", "Cold drinks;Pastries"="12",
                                                                                    "Cold drinks;Pastries;Sandwiches"="13", "Jaws chip"="14",
                                                                                    "Pastries"="15", "Never"="16", "Nothing"="16", "never"="16"))
new_dataset$FrequentItemPurchase_num <- as.numeric(new_dataset$FrequentItemPurchase_num)
table(new_dataset$FrequentItemPurchase_num)

# Converting categorical HearPromotions to numeric
table(new_dataset$HearPromotions)
new_dataset$HearPromotions_num <- revalue(new_dataset$HearPromotions, c("Application offer"="1", "Billboards"="2",
                                                                        "Emails"="3", "In Store displays"="4", "In Store displays;Billboards"="5", "Social Media"="6", 
                                                                        "Social Media;Deal sites (fave, iprice, etc...)"="7",
                                                                        "Social Media;Deal sites (fave, iprice, etc...);Through friends and word of mouth"="8",
                                                                        "Social Media;Emails"="9",
                                                                        "Social Media;Emails;Deal sites (fave, iprice, etc...);Through friends and word of mouth;In Store displays;Billboards"="10",
                                                                        "Social Media;Emails;Through friends and word of mouth"="11", "Social Media;In Store displays"="12",
                                                                        "Social Media;In Store displays;Billboards"="13", "Social Media;Through friends and word of mouth"="14",
                                                                        "Social Media;Through friends and word of mouth;Billboards"="15", 
                                                                        "Social Media;Through friends and word of mouth;In Store displays"="16",
                                                                        "Social Media;Through friends and word of mouth;In Store displays;Billboards"="17",
                                                                        "Starbucks Website/Apps"="18", "Starbucks Website/Apps;Deal sites (fave, iprice, etc...)"="19",
                                                                        "Starbucks Website/Apps;Emails"="20", "Starbucks Website/Apps;Social Media"="21",
                                                                        "Starbucks Website/Apps;Social Media;Deal sites (fave, iprice, etc...);Through friends and word of mouth"="22",
                                                                        "Starbucks Website/Apps;Social Media;Emails;Billboards"="23",
                                                                        "Starbucks Website/Apps;Social Media;Emails;Deal sites (fave, iprice, etc...)"="24",
                                                                        "Starbucks Website/Apps;Social Media;Emails;Through friends and word of mouth"="25",
                                                                        "Starbucks Website/Apps;Social Media;Through friends and word of mouth"="26",
                                                                        "Starbucks Website/Apps;Social Media;Through friends and word of mouth;In Store displays"="27",
                                                                        "Through friends and word of mouth"="28", "Through friends and word of mouth;In Store displays"="29"))
new_dataset$HearPromotions_num <- as.numeric(new_dataset$HearPromotions_num)
table(new_dataset$HearPromotions_num)

# Converting categorical AverageSpend to numeric
table(new_dataset$AverageSpend)
new_dataset$AverageSpend_num <- revalue(new_dataset$AverageSpend, c("Zero"="1", "Less than RM20"="2", 
                                                                    "Around RM20 - RM40"="3","More than RM40"="4"))
new_dataset$AverageSpend_num <- as.numeric(new_dataset$AverageSpend_num)
table(new_dataset$AverageSpend_num)

# Converting categorical ContinueToBuy to numeric
table(new_dataset$ContinueToBuy)
new_dataset$ContinueToBuy_num <- revalue(new_dataset$ContinueToBuy, c("No"="0", "Yes"="1"))
new_dataset$ContinueToBuy_num <- as.numeric(new_dataset$ContinueToBuy_num)
table(new_dataset$ContinueToBuy_num)

#==============================================================================================================================

# QUESTION 1a

str(new_dataset)

all_variable_dataset <- subset(new_dataset, select = -c(Gender, Age, PersonType, AnnualIncome, FrequencyOfVisit, EnjoyType,
                                                 TimeSpent, NearestOutlet, MembershipCard, AverageSpend, ContinueToBuy,
                                                 HearPromotions, FrequentItemPurchase))

# Dataset for Regressions
final_dataset <- all_variable_dataset %>% select(AnnualIncome_num, FrequencyOfVisit_num, MembershipCard_num, AverageSpend_num,
                                          ContinueToBuy_num, PriceRangeRating, AmbienceRating, WifiQualityRating, 
                                          ServiceRating, ChooseStarbucks, StarbucksQuality)


# X -> Independent variables
manual_reg_x <- final_dataset %>% select(AnnualIncome_num, FrequencyOfVisit_num, MembershipCard_num, AverageSpend_num,
                                         ContinueToBuy_num, PriceRangeRating, AmbienceRating, WifiQualityRating, 
                                         ServiceRating, ChooseStarbucks)

# Y -> Dependent variables
manual_reg_y <- final_dataset$StarbucksQuality

# Manual Multiple Linear Regression
model_manual <- lm(manual_reg_y ~ AnnualIncome_num + FrequencyOfVisit_num + MembershipCard_num + 
                       AverageSpend_num + ContinueToBuy_num + PriceRangeRating + AmbienceRating +
                       WifiQualityRating + ServiceRating + ChooseStarbucks, data = final_dataset)
summary(model_manual)



# QUESTION 1b

residuals <- resid(model_manual)
residuals

# Check linearity
plot(fitted(model_manual), residuals)
abline(0,0)

# Check normality by Shapiro-Wilk test
shapiro.test(residuals)

# Checking homoscedasticity by Breusch-Pagan test
library(lmtest)
lmtest::bptest(model_manual)

# Check for multi-collinearity
library(corrplot)
corrplot(cor(manual_reg_x, method="spearman"), method = "number", tl.col = "black")



# QUESTION 1c

# Manual Multiple Linear Regression
model_manual <- lm(manual_reg_y ~ ., data = manual_reg_x)
summary(model_manual)

# Calculate the Variance Inflation Factor (VIF) to check for multicollinearity
library(DescTools)         
VIF(model_manual)


# QUESTION 1d

# Plot the model diagnostic plots
library(ggfortify)
autoplot(model_manual)



# QUESTION 1e

# Creating the null and full model for stepwise regression

null <- lm(StarbucksQuality ~ 1, data = final_dataset)
null

full <- lm(StarbucksQuality ~ .-StarbucksQuality, data = final_dataset)
full

# Stepwise Linear Regression

model_stepwise <- step(null, scope = list(upper = full), direction = "both")
summary(model_stepwise)


#==============================================================================================================================

# QUESTION 2b

library(glmnet)

# Separating the x and y for lasso

lasso_reg_x <- data.matrix(final_dataset[, c(1:10)])

lasso_reg_y <- final_dataset$StarbucksQuality

# Lasso regression with alpha = 1
cv.lasso <- cv.glmnet(lasso_reg_x, lasso_reg_y, typemeasure="mse", alpha=1)
cv.lasso

plot(cv.lasso)

lambda_min <- cv.lasso$lambda.min
lambda_min

# List of variables which have non-zero coefficients
predict(cv.lasso, s = lambda_min, type = "coefficients")

lasso_using_manual <- lm(StarbucksQuality ~ AnnualIncome_num + FrequencyOfVisit_num + MembershipCard_num + AverageSpend_num +
               PriceRangeRating + AmbienceRating + ServiceRating + ChooseStarbucks, data=final_dataset)
summary(lasso_using_manual)

