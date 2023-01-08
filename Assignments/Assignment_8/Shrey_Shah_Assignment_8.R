# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_8")

# Import the dataset using read_csv()
alzheimer <- read.csv("alzheimer-1.csv", header=TRUE)
alzheimer <- alzheimer

# Shape of dataset
dim(alzheimer)

# Count of missing values
sum(is.na(alzheimer))

# Delete rows with missing values
new_alzheimer <- na.omit(alzheimer)

# Shape of new dataset after listwise deletion
dim(new_alzheimer)

# Renaming columns
names(new_alzheimer)[4] <- "Years_of_Education"
names(new_alzheimer)[5] <- "Socioeconomic_Status"
names(new_alzheimer)[6] <- "Mini_Mental_State_Examination_Score"
names(new_alzheimer)[7] <- "Clinical_Dementia_Rating"
names(new_alzheimer)[8] <- "Estimated_Total_Intracranial_Volume"
names(new_alzheimer)[9] <- "Normalize_Whole_Brain_Volume"
names(new_alzheimer)[10] <- "Atlas_Scaling_Factor"

str(new_alzheimer)

# Converting the Dementia variable to Factor
new_alzheimer$Dementia <- as.factor(new_alzheimer$Dementia)
table(new_alzheimer$Dementia)

new_alzheimer$Age_Group[(new_alzheimer$Age >= 60) & (new_alzheimer$Age <= 72)] <- "Age Between 60 and 72"
new_alzheimer$Age_Group[(new_alzheimer$Age > 72) & (new_alzheimer$Age <= 85)] <- "Age Between 73 and 85"
new_alzheimer$Age_Group[(new_alzheimer$Age > 85) & (new_alzheimer$Age <= 98)] <- "Age Between 86 and 98"
table(new_alzheimer$Age_Group)

new_alzheimer$Gender_Category[(new_alzheimer$Gender == 1)] <- "Male"
new_alzheimer$Gender_Category[(new_alzheimer$Gender == 0)] <- "Female"
table(new_alzheimer$Gender_Category)

str(new_alzheimer)

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance

# Create training (80%) and test (20%)

set.seed(23)  # set seed to ensure you always have same random numbers generated

split <- initial_split(new_alzheimer, prop = .8, strata = "Dementia")
train <- training(split)
test  <- testing(split)

# Dropping the CDR and nWBV columns as the odds-ratio is very absurd for them
log_reg <- glm(
  Dementia ~ Gender_Category + Age_Group + Years_of_Education + 
             Socioeconomic_Status + Mini_Mental_State_Examination_Score + 
             Estimated_Total_Intracranial_Volume + Atlas_Scaling_Factor,
  family = "binomial",
  data = new_alzheimer
)

summary(log_reg)
library(broom)

tidy(log_reg)

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE)

# Creating a model on training dataset
train$Dementia<- as.factor(train$Dementia)

log_reg_train = train(
  form = Dementia ~ Gender_Category + Age_Group + Years_of_Education + 
                    Socioeconomic_Status + Mini_Mental_State_Examination_Score + 
                    Estimated_Total_Intracranial_Volume + Normalize_Whole_Brain_Volume + 
                    Atlas_Scaling_Factor,
  data = train,
  method = "glm",
  family = "binomial"
)

# Running the model on the test dataset and getting the Confusion Matrix
confusionMatrix(predict(log_reg_train, test), as.factor(test$Dementia))

# Variables of Importance
vip(log_reg_train, num_features = 10)

library(ROCR)

log_reg_train = glm(
  form = Dementia ~ Gender_Category + Age_Group + Years_of_Education + 
    Socioeconomic_Status + Mini_Mental_State_Examination_Score + 
    Estimated_Total_Intracranial_Volume + Normalize_Whole_Brain_Volume + 
    Atlas_Scaling_Factor,
  data = train,
  family = "binomial"
)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

prediction <- prediction(as.numeric(log_reg_test_prob), test$Dementia)

performance <- performance(prediction,"tpr","fpr")
plot(performance)

library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$Dementia)
autoplot(precrec_obj)

# ROC_it library gives us the optimal Youden index and the c-statistic
library(ROCit)
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$Dementia)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$Dementia,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure
