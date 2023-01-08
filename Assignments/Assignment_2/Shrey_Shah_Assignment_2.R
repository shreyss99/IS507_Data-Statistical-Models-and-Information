# Load library readr to read the dataset
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_2")

# Import the dataset using read_csv()
raw_dataset <- read_csv("Starbucks_data-4.csv")
dataset <- raw_dataset

# Shape of dataset
dim(dataset)

# Count of missing values
sum(is.na(dataset))

# Delete rows with missing values
dataset <- na.omit(dataset)

# Shape of new dataset after listwise deletion
dim(dataset)
# There are 2 missing values but both are in same row

library(gmodels)

# QUESTION 1
# Considering Frequency of visits, Membership and Continue to Buy as indicators for Loyalty

# Recoding the Membership variable: Yes to 1 and No to 0
dataset$"Starbucks_Membership" <- as.numeric(str_detect(dataset$"9. Do you have Starbucks membership card?", "Yes"))

# Recoding the Frequency variable: Daily, Weekly to 1 and Monthly, Rarely, Never to 0
dataset$"Visit_Frequency" <- as.numeric(str_detect(dataset$"5. How often do you visit Starbucks?", "Weekly|Daily"))

# Recoding the Continue to Buy variable: Yes to 1 and No to 0
dataset$"Continue_To_Buy" <- as.numeric(str_detect(dataset$"20. Will you continue buying at Starbucks?", "Yes"))

# New column Loyalty is 1 if all above 3 variables have value 1 else 0
dataset$"Loyalty" <- ifelse(dataset$"Visit_Frequency" & dataset$"Starbucks_Membership" & dataset$"Continue_To_Buy", 1, 0)


# Case 1: PriceRate vs Starbucks_Membership
CrossTable(dataset$Starbucks_Membership, dataset$"13. How would you rate the price range at Starbucks?",
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')

# Case 2: PriceRate vs Visit_Frequency
CrossTable(dataset$Visit_Frequency, dataset$"13. How would you rate the price range at Starbucks?",
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')

# Case 3: PriceRate vs Continue_To_Buy
CrossTable(dataset$Continue_To_Buy, dataset$"13. How would you rate the price range at Starbucks?",
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')

# Case 4: PriceRate vs Loyalty
CrossTable(dataset$Loyalty, dataset$"13. How would you rate the price range at Starbucks?",
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')

#===========================================================================================================

# QUESTION 2

table(dataset$Starbucks_Membership, dataset$"16. You rate the WiFi quality at Starbucks as..")

CrossTable(dataset$Starbucks_Membership, dataset$"16. You rate the WiFi quality at Starbucks as..",
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')

#===========================================================================================================

# QUESTION 3

# Calculating if the person is a student or not and storing 1 for Student and 0 for All remaining
dataset$"Is_Student" <- as.numeric(str_detect(dataset$"3. Are you currently....?", "Student"))

total_students = sum(dataset$"Is_Student")
total_students

# Calculating if the person has ordered Pastries
dataset$"Has_Purchased_Pastries" <- as.numeric(str_detect(dataset$"10. What do you most frequently purchase at Starbucks?", 
                                                          "Pastries|Pastry"))

# Calculating the students who have bought pastries
dataset$"Student_Pastries" <- as.numeric(dataset$"Is_Student" & dataset$"Has_Purchased_Pastries")

student_pastries = sum(dataset$"Student_Pastries")
student_pastries

prop.test(student_pastries, total_students)


p0 = 0.5
standard_deviation = sqrt(p0 * (1-p0) / total_students)
standard_deviation

z_score = (0.119 - p0) / standard_deviation
z_score

pnorm(z_score)

#===========================================================================================================

# QUESTION 4

# Calculating if the person is a male or female and storing 1 for male and 0 for female
dataset$"Male_Female" <- as.numeric(str_detect(dataset$"1. Your Gender", "Male"))

# Calculating if the person has ordered Sandwiches
dataset$"Has_Purchased_Sandwiches" <- as.numeric(str_detect(dataset$"10. What do you most frequently purchase at Starbucks?", 
                                                            "Sandwiches"))

# Calculating the males who have purchased sandwiches
dataset$"Males_purchased_Sandwiches" <- as.numeric(dataset$"Male_Female" & dataset$"Has_Purchased_Sandwiches")

males_sandwiches = sum(dataset$"Males_purchased_Sandwiches")
males_sandwiches

total_sandwiches = sum(dataset$"Has_Purchased_Sandwiches")
total_sandwiches

females_sandwiches = total_sandwiches - sum(dataset$"Males_purchased_Sandwiches")
females_sandwiches

prop.test(males_sandwiches, total_sandwiches, correct = TRUE)

prop.test(females_sandwiches, total_sandwiches, correct = TRUE)

#===========================================================================================================

# QUESTION 5

shapiro.test(dataset$BeforePromoSatRate)
shapiro.test(dataset$AfterPromoSatRate)

wilcox.test(dataset$AfterPromoSatRate, dataset$BeforePromoSatRate)

#===========================================================================================================

# QUESTION 6

dataset$'BeforePromoSatRate_2class' <- as.numeric(dataset$BeforePromoSatRate >= 4)

dataset$'AfterPromoSatRate_2class' <- as.numeric(dataset$AfterPromoSatRate >= 4)

dataset$"Satisfaction" <- as.numeric(dataset$BeforePromoSatRate_2class 
                                           & dataset$BeforePromoSatRate_2class)

shapiro.test(dataset$Satisfaction)

CrossTable(dataset$"Satisfaction", dataset$"17. How would you rate the service at Starbucks? (Promptness, friendliness, etc..)", 
           digits=3, expected=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=FALSE, 
           chisq = TRUE, fisher=FALSE, format='SPSS')




dataset$"BeforePromoSatRate_cutoff" <- cut(dataset$BeforePromoSatRate, breaks=4, include.lowest = TRUE,
                                           labels=c("Poor", "Low", "Medium", "High"))
dataset$"BeforePromoSatRate_cutoff"

dataset$"AfterPromoSatRate_cutoff" <- cut(dataset$AfterPromoSatRate, breaks=4, include.lowest = TRUE,
                                          labels=c("Poor", "Low", "Medium", "High"))
dataset$"AfterPromoSatRate_cutoff"










