# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_7")

# Import the dataset using read_csv()
raw_dataset <- read.csv("alzheimer.csv", header=TRUE)
dataset <- raw_dataset

# Shape of dataset
dim(dataset)

# Count of missing values
sum(is.na(dataset))

# Delete rows with missing values
new_dataset <- na.omit(dataset)

# Shape of new dataset after listwise deletion
dim(new_dataset)

library(MASS)

shapiro.test(new_dataset$Gender)
shapiro.test(new_dataset$Age)
shapiro.test(new_dataset$EDUC)
shapiro.test(new_dataset$SES)
shapiro.test(new_dataset$MMSE)
shapiro.test(new_dataset$CDR)
shapiro.test(new_dataset$eTIV)
shapiro.test(new_dataset$nWBV)
shapiro.test(new_dataset$ASF)


new_dataset[2:10] <- scale(new_dataset[2:10])

# 1.a) LDA using Cross Validation

alzheimerLDA_CV <- lda(Dementia ~ ., data=new_dataset, CV=TRUE)
alzheimerLDA_CV

# Plot LDA with CV
alzheimerLDA_CV <- lda(Dementia ~ ., data=new_dataset)
alzheimerLDA_CV
plot(alzheimerLDA_CV, xlab = "LD1", ylab = "LD2")

# Prediction
pred_CV <- predict(alzheimerLDA_CV, newdata=new_dataset[,2:10])$class
pred_CV

# Results of the prediction (Confusion Matrix) using Cross Validation
table_CV<-table(pred_CV, new_dataset$Dementia)
table_CV

# Accuracy of LDA with CV
accuracy_CV <- sum(diag(table_CV))/sum(table_CV)
accuracy_CV


# 1.b) LDA using Training and Testing

#Creating Training and Testing Samples
require(caTools)
library(caTools)

set.seed(23)

# split the data in the ratio mentioned in SplitRatio
sample = sample.split(new_dataset, SplitRatio = 0.80)

# Training data is subset of sample with value as TRUE
train = subset(new_dataset, sample == TRUE)
# Test data is subset of sample with value as FALSE
test = subset(new_dataset, sample == FALSE)

alzheimerLDA_TT = lda(train$Dementia ~ ., data=train)
alzheimerLDA_TT

plot(alzheimerLDA_TT)

pred_TT<-predict(alzheimerLDA_TT)$class
pred_TT

table_TT <- table(pred_TT, train$Dementia)
table_TT

# Training data accuracy
accuracy_TT <- sum(diag(table_TT))/sum(table_TT)
accuracy_TT

# Running the classifier on test data
pred_TT_test = predict(alzheimerLDA_TT, newdata=test[,c(1:10)])$class
table_TT_test<-table(pred_TT_test, test$Dementia)
table_TT_test
confusionMatrix(table_TT_test)

# Testing data accuarcy
sum(diag(table_TT_test)/sum(table_TT_test))




#============================================
# 2.

library(FactoMineR)
library(cluster) #Basic Clustering Algorithms

library(factoextra)
library(ggdensity)
library(ggpubr)
library(moments)
library(nortest)

# 1) Cluster Analysis on OnlineNewsPopularity Dataset

# Import the dataset using read_csv()
online_news_raw_dataset <- read.csv("OnlineNewsPopularity.csv", header=TRUE)
online_news_dataset <- online_news_raw_dataset

# Shape of dataset
dim(online_news_dataset)

# Count of missing values
sum(is.na(online_news_dataset))

# Delete rows with missing values
dataset <- na.omit(online_news_dataset)

# Shape of new dataset after listwise deletion
dim(online_news_dataset)

# Dropping the 1st URL column
final_online_news_dataset <- online_news_dataset[, c(47:56)]

str(final_online_news_dataset)

final_online_news_dataset[final_online_news_dataset == 0] <- 0.1

# Normalizing global_rate_positive_words
ad.test(final_online_news_dataset$global_rate_positive_words)
hist(final_online_news_dataset$global_rate_positive_words)
skewness(final_online_news_dataset$global_rate_positive_words)
final_online_news_dataset$global_rate_positive_words <- sqrt(final_online_news_dataset$global_rate_positive_words)

# Normalizing global_rate_negative_words
ad.test(final_online_news_dataset$global_rate_negative_words)
hist(final_online_news_dataset$global_rate_negative_words)
skewness(final_online_news_dataset$global_rate_negative_words)
final_online_news_dataset$global_rate_negative_words <- log10(final_online_news_dataset$global_rate_negative_words)

# Normalizing rate_positive_words
ad.test(final_online_news_dataset$rate_positive_words)
hist(final_online_news_dataset$rate_positive_words)
skewness(final_online_news_dataset$rate_positive_words)
#final_online_news_dataset$rate_positive_words <- log10(final_online_news_dataset$rate_positive_words)

# Normalizing rate_negative_words
ad.test(final_online_news_dataset$rate_negative_words)
hist(final_online_news_dataset$rate_negative_words)
skewness(final_online_news_dataset$rate_negative_words)
final_online_news_dataset$rate_negative_words <- sqrt(final_online_news_dataset$rate_negative_words)

# Normalizing avg_positive_polarity
ad.test(final_online_news_dataset$avg_positive_polarity)
hist(final_online_news_dataset$avg_positive_polarity)
skewness(final_online_news_dataset$avg_positive_polarity)
final_online_news_dataset$avg_positive_polarity <- sqrt(max(final_online_news_dataset$avg_positive_polarity+1) - final_online_news_dataset$avg_positive_polarity)

# Normalizing avg_negative_polarity
ad.test(final_online_news_dataset$avg_negative_polarity)
hist(final_online_news_dataset$avg_negative_polarity)
skewness(final_online_news_dataset$avg_negative_polarity)
#final_online_news_dataset$avg_negative_polarity <- log10(final_online_news_dataset$avg_negative_polarity)

# Normalizing min_positive_polarity
ad.test(final_online_news_dataset$min_positive_polarity)
hist(final_online_news_dataset$min_positive_polarity)
skewness(final_online_news_dataset$min_positive_polarity)
final_online_news_dataset$min_positive_polarity <- log10(final_online_news_dataset$min_positive_polarity)

# Normalizing max_positive_polarity
ad.test(final_online_news_dataset$max_positive_polarity)
hist(final_online_news_dataset$max_positive_polarity)
skewness(final_online_news_dataset$max_positive_polarity)
final_online_news_dataset$max_positive_polarity <- sqrt(max(final_online_news_dataset$max_positive_polarity+1) - final_online_news_dataset$max_positive_polarity)

# Normalizing min_negative_polarity
ad.test(final_online_news_dataset$min_negative_polarity)
hist(final_online_news_dataset$min_negative_polarity)
skewness(final_online_news_dataset$min_negative_polarity)
#final_online_news_dataset$min_negative_polarity <- sqrt(final_online_news_dataset$min_negative_polarity)

# Normalizing max_negative_polarity
ad.test(final_online_news_dataset$max_negative_polarity)
hist(final_online_news_dataset$max_negative_polarity)
skewness(final_online_news_dataset$max_negative_polarity)
final_online_news_dataset$max_negative_polarity <- log10(max(final_online_news_dataset$max_negative_polarity+1)-final_online_news_dataset$max_negative_polarity)


# Scaling the dataset
final_online_news_dataset_scaled <- scale(final_online_news_dataset)

# Optimal number of clusters using the K-Means
my.data.matrix <- final_online_news_dataset_scaled

my.k.choices <- 3:5
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)


# Run K-Means clustering with 3 clusters
set.seed(123)
final_online_news_dataset_kmeans <- kmeans(final_online_news_dataset_scaled, centers=3, iter.max=25, 
                                           nstart = 25)
final_online_news_dataset_kmeans

# Visualize
fviz_cluster(final_online_news_dataset_kmeans, data = final_online_news_dataset_scaled, 
             ellipse.type = "convex",
             palette = "jco",
             show.clust.cent = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())
