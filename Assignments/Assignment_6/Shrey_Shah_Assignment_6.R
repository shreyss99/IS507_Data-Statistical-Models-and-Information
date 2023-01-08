# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_6")

# 1) PCA on OnlineNewsPopularity Dataset

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

# Dropping the 1st URL column and last target column
new_online_news_dataset <- online_news_dataset[, c(2:60)]

library("factoextra")
library("psych")
library("GGally")
library("REdaS")
library("fmsb")

str(new_online_news_dataset)

# Check the fit of the data for PCA
KMO(new_online_news_dataset)
bartlett.test(new_online_news_dataset)
CronbachAlpha(new_online_news_dataset)

# You need to run the dataset in PCA function once before you decide on the number of components
pca = prcomp(new_online_news_dataset, scale = TRUE)
fviz_eig(pca)

# B) Number of components using Eigen values
plot(pca)
abline(1,0)
sum(get_eig(pca)$eigenvalue>1)

# Calculating standard deviation and variance of the components
std_dev <- pca$sdev
var <- std_dev^2

# Calculating the proportion of variance explained by components
prop_varex <- var/sum(var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

new_online_news_dataset

# C) Run a new PCA model based on the number of components
options(max.print = 10000)

# 1st PCA
pca_with_5_components_promax_1 = principal(new_online_news_dataset, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_1$loadings, sort=T)

# Dropping columns with no loadings
new_online_news_dataset_removed_1 = dplyr::select(new_online_news_dataset, select = -c(31:38))

# 2nd PCA
pca_with_5_components_promax_2 = principal(new_online_news_dataset_removed_1, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_2$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_2$loadings, cutoff=0.4, sort=T)

# Dropping columns with no loadings
new_online_news_dataset_removed_2 = dplyr::select(new_online_news_dataset_removed_1, -c(n_tokens_title,num_self_hrefs,
                                                                                        num_imgs,num_videos,
                                                                                        num_keywords,data_channel_is_lifestyle,
                                                                                        data_channel_is_entertainment,
                                                                                        data_channel_is_bus,data_channel_is_socmed,
                                                                                        data_channel_is_tech,kw_min_max,
                                                                                        self_reference_min_shares,
                                                                                        self_reference_max_shares,
                                                                                        self_reference_avg_sharess,
                                                                                        LDA_00,LDA_01,LDA_04,min_positive_polarity,
                                                                                        max_negative_polarity,title_subjectivity,
                                                                                        title_sentiment_polarity,
                                                                                        abs_title_subjectivity,
                                                                                        abs_title_sentiment_polarity))

# 3rd PCA
pca_with_5_components_promax_3 = principal(new_online_news_dataset_removed_2, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_3$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_3$loadings, cutoff=0.4, sort=T)

# Dropping columns with no loadings
new_online_news_dataset_removed_3 = dplyr::select(new_online_news_dataset_removed_2, -c(num_hrefs,data_channel_is_world,
                                                                                        kw_min_avg,LDA_02))

# 4th PCA
pca_with_5_components_promax_4 = principal(new_online_news_dataset_removed_3, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_4$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_4$loadings, cutoff=0.4, sort=T)

# Dropping columns with no loadings
new_online_news_dataset_removed_4 = dplyr::select(new_online_news_dataset_removed_3, -c(n_tokens_content,LDA_03))

# 5th PCA
pca_with_5_components_promax_5 = principal(new_online_news_dataset_removed_4, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_5$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_5$loadings, cutoff=0.706, sort=T)

# Dropping columns with no loadings
new_online_news_dataset_removed_5 = dplyr::select(new_online_news_dataset_removed_4, -c(avg_negative_polarity))

# 6th PCA
pca_with_5_components_promax_6 = principal(new_online_news_dataset_removed_5, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_6$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_6$loadings, cutoff=0.59, sort=T)



##################################################################################################################################

# 3) CCA in R

# Import the dataset using read_csv()
raw_dataset <- read.csv("Young People Responses.csv", header=TRUE)
dataset <- raw_dataset

# Shape of dataset
dim(dataset)

# Count of missing values
sum(is.na(dataset))

# Delete rows with missing values
dataset <- na.omit(dataset)

# Shape of new dataset after listwise deletion
dim(dataset)

new_dataset <- dataset

#Show Structure of Dataset
str(new_dataset, list.len=ncol(new_dataset))

str(new_dataset)

library(CCA)
install.packages("yacca")
library(yacca)

phobias <- new_dataset[,c(64:73)]
spending <- new_dataset[,c(134:140)]

# Canonical Correlation
cca_phobias_spendings = cca(spending, phobias)
cca_phobias_spendings

# 1a) Test Null Hypothesis that canonical correlations are 0
F.test.cca(cca_phobias_spendings)

# Helio plot for CV1
helio.plot(cca_phobias_spendings, cv=1, x.name='Spending', y.name='Phobias')

# Helio plot for CV2
helio.plot(cca_phobias_spendings, cv=2, x.name='Spending', y.name='Phobias')

summary(cca_phobias_spendings)

# Perform a Chi-Square test on the cca result
round(pchisq(cca_phobias_spendings$chisq, cca_phobias_spendings$df, lower.tail=F), 3)





# EXTRA CREDIT

# 4)

install.packages("readxl")
library(readxl)

# Import the dataset using read.table()
readers_raw_dataset <- read_excel("Readers.xls")
readers_dataset <- readers_raw_dataset

# Shape of dataset
dim(readers_dataset)

# Count of missing values
sum(is.na(readers_dataset))

# Delete rows with missing values
readers_dataset <- na.omit(readers_dataset)

# Shape of new dataset after listwise deletion
dim(readers_dataset)

new_readers_dataset <- readers_dataset
new_readers_dataset

sample_data <- matrix(c(5,7,2,18,46,20,19,29,39,12,40,49,3,7,16), nrow=5, ncol=3, byrow=TRUE)
dimnames(sample_data) <- list(c("E1","E2","E3","E4","E5"),
                              c("C1","C2","C3"))
sample_data
final_dataset <- as.table(sample_data)
final_dataset

install.packages("ca")
library(ca)

# 4.a) Mosaic Plot of the 2 categorical variables

install.packages("vcd")
library(vcd)

mosaicplot(final_dataset, shade=TRUE, ylab="Reading Level", xlab="Education Level Completed", 
           main="Reading Level vs Education Level Completed")

# 4.b) Correspondence Analysis and summary

fit=ca(final_dataset)
fit
summary(fit)

# Plotting correspondence analysis
plot(fit)
plot(fit, map='rowgreen', arrows=c(T, T))

# 4.3) 



