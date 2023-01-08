# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Project/")

# Import the dataset using read_csv()
raw_online_news_dataset <- read.csv("OnlineNewsPopularity-New.csv", header=TRUE)
online_news_dataset <- raw_online_news_dataset

# Shape of dataset
dim(online_news_dataset)

# Count of missing values
sum(is.na(online_news_dataset))

# Delete rows with missing values
online_news_dataset <- subset(online_news_dataset, online_news_dataset$Data_Channel != "9999")

# Shape of new dataset after deletion
dim(online_news_dataset)

# Dropping the 1st URL column, 2nd Timedelta column and last target column
x <- online_news_dataset[, c(3:48)]

# Dropping the 14th and 27th column
online_news_dataset_pca <- dplyr::select(x, select = -c('Data_Channel', 'Weekday'))

dim(online_news_dataset_pca)

library("factoextra")
library("psych")
library("nortest")
library(ggdensity)
library(ggpubr)
library(moments)

str(online_news_dataset_pca)

summary(online_news_dataset_pca)

# Remove outlier from n_unique_tokens
online_news_dataset_pca=online_news_dataset_pca[!online_news_dataset_pca$n_unique_tokens==701,]

# Replace 0 with 0.1 everywhere to help in log transformations
online_news_dataset_pca[online_news_dataset_pca == 0] <- 0.1

cova <- cov(online_news_dataset_pca)
eigen <- eigen(cova)
eigen

pca <- princomp(online_news_dataset_pca, cor='FALSE')
biplot(pca)

library(corrplot)
res <- cor(online_news_dataset_pca, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust", tl.col = 'black', type = 'full')

library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(res, hc.order =TRUE, type ="lower")

# Transformations for normality
ad.test(online_news_dataset_pca$n_tokens_title)
hist(online_news_dataset_pca$n_tokens_title)
skewness(online_news_dataset_pca$n_tokens_title)
#online_news_dataset_pca$n_tokens_title <- sqrt(online_news_dataset_pca$n_tokens_title)

ad.test(online_news_dataset_pca$n_tokens_content)
hist(online_news_dataset_pca$n_tokens_content)
skewness(online_news_dataset_pca$n_tokens_content)
online_news_dataset_pca$n_tokens_content <- sqrt(online_news_dataset_pca$n_tokens_content)

ad.test(online_news_dataset_pca$n_unique_tokens)
hist(online_news_dataset_pca$n_unique_tokens)
skewness(online_news_dataset_pca$n_unique_tokens)
#online_news_dataset_pca$n_unique_tokens <- log(online_news_dataset_pca$n_unique_tokens)

ad.test(online_news_dataset_pca$n_non_stop_words)
hist(online_news_dataset_pca$n_non_stop_words)
skewness(online_news_dataset_pca$n_non_stop_words)
#online_news_dataset_pca$n_non_stop_words <- log(online_news_dataset_pca$n_non_stop_words)

ad.test(online_news_dataset_pca$n_non_stop_unique_tokens)
hist(online_news_dataset_pca$n_non_stop_unique_tokens)
skewness(online_news_dataset_pca$n_non_stop_unique_tokens)
#online_news_dataset_pca$n_non_stop_unique_tokens <-sqrt(online_news_dataset_pca$n_non_stop_unique_tokens)

ad.test(online_news_dataset_pca$num_hrefs)
hist(online_news_dataset_pca$num_hrefs)
skewness(online_news_dataset_pca$num_hrefs)
online_news_dataset_pca$num_hrefs <-sqrt(online_news_dataset_pca$num_hrefs)

ad.test(online_news_dataset_pca$num_self_hrefs)
hist(online_news_dataset_pca$num_self_hrefs)
skewness(online_news_dataset_pca$num_self_hrefs)
online_news_dataset_pca$num_self_hrefs <-sqrt(online_news_dataset_pca$num_self_hrefs)

ad.test(online_news_dataset_pca$num_imgs)
hist(online_news_dataset_pca$num_imgs)
skewness(online_news_dataset_pca$num_imgs)
online_news_dataset_pca$num_imgs <-sqrt(online_news_dataset_pca$num_imgs)
online_news_dataset_pca$num_imgs <-log(online_news_dataset_pca$num_imgs)

ad.test(online_news_dataset_pca$num_videos)
hist(online_news_dataset_pca$num_videos)
skewness(online_news_dataset_pca$num_videos)
online_news_dataset_pca$num_videos <-sqrt(online_news_dataset_pca$num_videos)
online_news_dataset_pca$num_videos <-log(online_news_dataset_pca$num_videos)

ad.test(online_news_dataset_pca$average_token_length)
hist(online_news_dataset_pca$average_token_length)
skewness(online_news_dataset_pca$average_token_length)
#online_news_dataset_pca$average_token_length <-log(online_news_dataset_pca$average_token_length)

ad.test(online_news_dataset_pca$average_token_length)
hist(online_news_dataset_pca$average_token_length)
skewness(online_news_dataset_pca$average_token_length)
#online_news_dataset_pca$average_token_length <-log(online_news_dataset_pca$average_token_length)

ad.test(online_news_dataset_pca$num_keywords)
hist(online_news_dataset_pca$num_keywords)
skewness(online_news_dataset_pca$num_keywords)
#online_news_dataset_pca$num_keywords <-log(online_news_dataset_pca$num_keywords)

ad.test(online_news_dataset_pca$kw_min_min)
hist(online_news_dataset_pca$kw_min_min)
skewness(online_news_dataset_pca$kw_min_min)
online_news_dataset_pca$kw_min_min <-log(online_news_dataset_pca$kw_min_min)

ad.test(online_news_dataset_pca$kw_max_min)
hist(online_news_dataset_pca$kw_max_min)
skewness(online_news_dataset_pca$kw_max_min)
online_news_dataset_pca$kw_max_min <-sqrt(online_news_dataset_pca$kw_max_min)
online_news_dataset_pca$kw_max_min <-sqrt(online_news_dataset_pca$kw_max_min)

ad.test(online_news_dataset_pca$kw_avg_min)
hist(online_news_dataset_pca$kw_avg_min)
skewness(online_news_dataset_pca$kw_avg_min)
#online_news_dataset_pca$kw_avg_min <-log(online_news_dataset_pca$kw_avg_min)

ad.test(online_news_dataset_pca$kw_min_max)
hist(online_news_dataset_pca$kw_min_max)
skewness(online_news_dataset_pca$kw_min_max)
online_news_dataset_pca$kw_min_max <-sqrt(online_news_dataset_pca$kw_min_max)

ad.test(online_news_dataset_pca$kw_max_max)
hist(online_news_dataset_pca$kw_max_max)
skewness(online_news_dataset_pca$kw_max_max)
#online_news_dataset_pca$kw_max_max <-log(online_news_dataset_pca$kw_max_max)

ad.test(online_news_dataset_pca$kw_avg_max)
hist(online_news_dataset_pca$kw_avg_max)
skewness(online_news_dataset_pca$kw_avg_max)
online_news_dataset_pca$kw_avg_max <-sqrt(online_news_dataset_pca$kw_avg_max)

ad.test(online_news_dataset_pca$kw_min_avg)
hist(online_news_dataset_pca$kw_min_avg)
skewness(online_news_dataset_pca$kw_min_avg)
#online_news_dataset_pca$kw_min_avg <-log(online_news_dataset_pca$kw_min_avg)

ad.test(online_news_dataset_pca$kw_max_avg)
hist(online_news_dataset_pca$kw_max_avg)
skewness(online_news_dataset_pca$kw_max_avg)
online_news_dataset_pca$kw_max_avg <-sqrt(online_news_dataset_pca$kw_max_avg)

ad.test(online_news_dataset_pca$kw_avg_avg)
hist(online_news_dataset_pca$kw_avg_avg)
skewness(online_news_dataset_pca$kw_avg_avg)
online_news_dataset_pca$kw_avg_avg <-sqrt(online_news_dataset_pca$kw_avg_avg)

ad.test(online_news_dataset_pca$self_reference_min_shares)
hist(online_news_dataset_pca$self_reference_min_shares)
skewness(online_news_dataset_pca$self_reference_min_shares)
online_news_dataset_pca$self_reference_min_shares <-sqrt(online_news_dataset_pca$self_reference_min_shares)

ad.test(online_news_dataset_pca$self_reference_max_shares)
hist(online_news_dataset_pca$self_reference_max_shares)
skewness(online_news_dataset_pca$self_reference_max_shares)
online_news_dataset_pca$self_reference_max_shares <-sqrt(online_news_dataset_pca$self_reference_max_shares)

ad.test(online_news_dataset_pca$self_reference_avg_shares)
hist(online_news_dataset_pca$self_reference_avg_shares)
skewness(online_news_dataset_pca$self_reference_avg_shares)
online_news_dataset_pca$self_reference_avg_shares <-sqrt(online_news_dataset_pca$self_reference_avg_shares)

ad.test(online_news_dataset_pca$global_subjectivity)
hist(online_news_dataset_pca$global_subjectivity)
skewness(online_news_dataset_pca$global_subjectivity)
#online_news_dataset_pca$global_subjectivity <-sqrt(online_news_dataset_pca$global_subjectivity)

ad.test(online_news_dataset_pca$global_sentiment_polarity)
hist(online_news_dataset_pca$global_sentiment_polarity)
skewness(online_news_dataset_pca$global_sentiment_polarity)
#online_news_dataset_pca$global_sentiment_polarity <-sqrt(online_news_dataset_pca$global_sentiment_polarity)

ad.test(online_news_dataset_pca$global_rate_positive_words)
hist(online_news_dataset_pca$global_rate_positive_words)
skewness(online_news_dataset_pca$global_rate_positive_words)
online_news_dataset_pca$global_rate_positive_words <-sqrt(online_news_dataset_pca$global_rate_positive_words)

ad.test(online_news_dataset_pca$global_rate_negative_words)
hist(online_news_dataset_pca$global_rate_negative_words)
skewness(online_news_dataset_pca$global_rate_negative_words)
online_news_dataset_pca$global_rate_negative_words <-sqrt(online_news_dataset_pca$global_rate_negative_words)

ad.test(online_news_dataset_pca$rate_positive_words)
hist(online_news_dataset_pca$rate_positive_words)
skewness(online_news_dataset_pca$rate_positive_words)
#online_news_dataset_pca$rate_positive_words <-log(online_news_dataset_pca$rate_positive_words)

ad.test(online_news_dataset_pca$rate_negative_words)
hist(online_news_dataset_pca$rate_negative_words)
skewness(online_news_dataset_pca$rate_negative_words)
online_news_dataset_pca$rate_negative_words <-sqrt(online_news_dataset_pca$rate_negative_words)

ad.test(online_news_dataset_pca$avg_positive_polarity)
hist(online_news_dataset_pca$avg_positive_polarity)
skewness(online_news_dataset_pca$avg_positive_polarity)
#online_news_dataset_pca$avg_positive_polarity <-log(online_news_dataset_pca$avg_positive_polarity)

ad.test(online_news_dataset_pca$avg_negative_polarity)
hist(online_news_dataset_pca$avg_negative_polarity)
skewness(online_news_dataset_pca$avg_negative_polarity)
#online_news_dataset_pca$avg_negative_polarity <-log(online_news_dataset_pca$avg_negative_polarity)

ad.test(online_news_dataset_pca$min_positive_polarity)
hist(online_news_dataset_pca$min_positive_polarity)
skewness(online_news_dataset_pca$min_positive_polarity)
online_news_dataset_pca$min_positive_polarity <-sqrt(online_news_dataset_pca$min_positive_polarity)

ad.test(online_news_dataset_pca$min_negative_polarity)
hist(online_news_dataset_pca$min_negative_polarity)
skewness(online_news_dataset_pca$min_negative_polarity)
#online_news_dataset_pca$min_negative_polarity <-sqrt(online_news_dataset_pca$min_negative_polarity)

ad.test(online_news_dataset_pca$max_positive_polarity)
hist(online_news_dataset_pca$max_positive_polarity)
skewness(online_news_dataset_pca$max_positive_polarity)
#online_news_dataset_pca$max_positive_polarity <-sqrt(online_news_dataset_pca$max_positive_polarity)

ad.test(online_news_dataset_pca$max_negative_polarity)
hist(online_news_dataset_pca$max_negative_polarity)
skewness(online_news_dataset_pca$max_negative_polarity)
#online_news_dataset_pca$max_negative_polarity <-log(online_news_dataset_pca$max_negative_polarity)

ad.test(online_news_dataset_pca$title_subjectivity)
hist(online_news_dataset_pca$title_subjectivity)
skewness(online_news_dataset_pca$title_subjectivity)
online_news_dataset_pca$title_subjectivity <-sqrt(online_news_dataset_pca$title_subjectivity)

ad.test(online_news_dataset_pca$abs_title_subjectivity)
hist(online_news_dataset_pca$abs_title_subjectivity)
skewness(online_news_dataset_pca$abs_title_subjectivity)
#online_news_dataset_pca$abs_title_subjectivity <-sqrt(online_news_dataset_pca$abs_title_subjectivity)

ad.test(online_news_dataset_pca$abs_title_sentiment_polarity)
hist(online_news_dataset_pca$title_sentiment_polarity)
skewness(online_news_dataset_pca$title_sentiment_polarity)
#online_news_dataset_pca$abs_title_sentiment_polarity <-sqrt(online_news_dataset_pca$abs_title_sentiment_polarity)

ad.test(online_news_dataset_pca$self_reference_avg_shares)
hist(online_news_dataset_pca$self_reference_avg_shares)
skewness(online_news_dataset_pca$self_reference_avg_shares)
online_news_dataset_pca$self_reference_avg_shares <-sqrt(online_news_dataset_pca$self_reference_avg_shares)

sum(is.na(online_news_dataset_pca))
online_news_dataset_pca <- na.omit(online_news_dataset_pca)

# Scaling the data
online_news_dataset_pca_scaled <- scale(online_news_dataset_pca)

str(online_news_dataset_pca_scaled)

summary(online_news_dataset_pca_scaled)

# Converting scaled data to matrix
online_news_dataset_pca_scaled <- as.data.frame(online_news_dataset_pca_scaled)



# Check the fit of the data for PCA
KMO(online_news_dataset_pca_scaled)
bartlett.test(online_news_dataset_pca_scaled)
bartlett.test(online_news_dataset_pca)
CronbachAlpha(online_news_dataset_pca_scaled)

# You need to run the dataset in PCA function once before you decide on the number of components
pca = prcomp(online_news_dataset_pca_scaled, scale = TRUE)
fviz_eig(pca, addlabels=TRUE)

# Eigen values of 5 components
get_eig(pca)$eigenvalue[1:5]  

# B) Number of components using Eigen values
plot(pca)
abline(1,0)

# Calculating standard deviation and variance of the components
std_dev <- pca$sdev
var <- std_dev^2

# Calculating the proportion of variance explained by components
prop_varex <- var/sum(var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# C) Run a new PCA model based on the number of components
options(max.print = 10000)

# 1st PCA
pca_with_5_components_promax_1 = principal(online_news_dataset_pca_scaled, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_1$loadings, sort=T)

comps = print(pca_with_5_components_promax_1$loadings, cutoff=0.47, sort=T)

# Dropping columns with no loadings
online_news_dataset_pca_removed_1 = dplyr::select(online_news_dataset_pca_scaled, select = -c(n_tokens_title,
                                                                                       num_self_hrefs, num_videos,
                                                                                       num_keywords, kw_max_min,
                                                                                       kw_avg_min, kw_max_avg,
                                                                                       LDA_00, LDA_01, LDA_02,
                                                                                       LDA_03, LDA_04,
                                                                                       global_rate_negative_words,
                                                                                       title_sentiment_polarity,
                                                                                       avg_positive_polarity,
                                                                                       max_positive_polarity,
                                                                                       title_subjectivity,
                                                                                       title_sentiment_polarity,
                                                                                       abs_title_subjectivity,
                                                                                       abs_title_sentiment_polarity))



# 2nd PCA
pca_with_5_components_promax_2 = principal(online_news_dataset_pca_removed_1, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_2$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_2$loadings, cutoff=0.48, sort=T)

# Dropping columns with no loadings
online_news_dataset_pca_removed_2 = dplyr::select(online_news_dataset_pca_removed_1, -c(max_negative_polarity))
                                                                                    
# 3rd PCA
pca_with_5_components_promax_3 = principal(online_news_dataset_pca_removed_2, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_3$loadings, sort=T)

# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_3$loadings, cutoff=0.5, sort=T)

# Dropping columns with no loadings
online_news_dataset_pca_removed_3 = dplyr::select(online_news_dataset_pca_removed_2, -c(global_subjectivity,
                                                                                        min_positive_polarity,
                                                                                        avg_negative_polarity,
                                                                                        min_negative_polarity))

# 4th PCA
pca_with_5_components_promax_4 = principal(online_news_dataset_pca_removed_3, rotate="promax", nfactors = 5, scores = TRUE)
comps = print(pca_with_5_components_promax_4$loadings, sort=T)

# 4th PCA
# Increasing the cutoff to remove crossloadings
comps = print(pca_with_5_components_promax_4$loadings, cutoff=0.45, sort=T)

# Summary of final data
summary(online_news_dataset_pca_removed_3)

library(pastecs)
stat.desc(online_news_dataset_pca_removed_3)

stat.desc(online_news_dataset_pca)

