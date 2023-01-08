# Load library readr to read the dataset
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_3")

# Import the dataset using read_csv()
raw_dataset <- read_csv("Starbucks_data-2.csv")
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

#==============================================================================================================================

# QUESTION 1

# Let's find whether the continuous variable of Ambience rating is normal or not using Shapiro's test.

library(RVAideMemoire)

names(dataset)[4] <- "PersonType"
names(dataset)[16] <- "AmbienceRating"

byf.shapiro(as.matrix(dataset$AmbienceRating)~dataset$PersonType, data=dataset)

# Since the Housewife category in the variable PersonType has only 2 samples and Shapiro requires atleast 3 samples. 
# So we will discard the rows with Housewife in the PersonRating column

new_dataset <- dataset[!(dataset$PersonType == 'Housewife'), ]
dim(new_dataset)

byf.shapiro(as.matrix(AmbienceRating)~PersonType, data=new_dataset)

# Using Kruskal-Wallis non-parametric test

kruskal.test(AmbienceRating~PersonType, data=new_dataset)

kruskal.test(PersonType~AmbienceRating, data=new_dataset)

#==============================================================================================================================

# QUESTION 2

names(new_dataset)[14] <- "PriceRating"
names(new_dataset)[18] <- "ServiceRating"

# Let's find whether the continuous variable Price and Service Rating is normal or not using Shapiro's test.

shapiro.test(new_dataset$PriceRating)
shapiro.test(new_dataset$AmbienceRating)

# Performing the Spearman correlation test because both the ratings are not normal
cor.test(new_dataset$ServiceRating, new_dataset$PriceRating, method = "spearman")

#==============================================================================================================================

# QUESTION 3

names(new_dataset)[13] <- "QualityStarbucks"
names(new_dataset)[14] <- "PriceRating"
names(new_dataset)[15] <- "SalesPurchDecision"
names(new_dataset)[16] <- "AmbienceRating"
names(new_dataset)[17] <- "WifiQualityRating"

# Subset the dataset for getting the 5 variables

correlation_dataset <- select(new_dataset, "QualityStarbucks", "PriceRating", 
                              "SalesPurchDecision","AmbienceRating", 
                              "WifiQualityRating")

shapiro.test(new_dataset$PriceRating)
shapiro.test(new_dataset$AmbienceRating)
shapiro.test(new_dataset$QualityStarbucks)
shapiro.test(new_dataset$SalesPurchDecision)
shapiro.test(new_dataset$WifiQualityRating)

install.packages("corrplot")
library(corrplot)

# Plot the correlation
corrplot(cor(correlation_dataset, method="spearman"), tl.col = "black", type = "full", method = "number")

#==============================================================================================================================

# QUESTION 4

library(ggplot2)

mosaicplot(`2. Your Age` ~ `11. On average, how much would you spend at Starbucks per visit?`,
           data = new_dataset, main = "Averge spend at Starbucks vs Age groups",
           xlab = "Age Groups", ylab = "Average spend based on RM", color = "skyblue2", border = "black",
           cex=0.7)

