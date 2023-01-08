# Load library readr to read the dataset
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set working directory
setwd("~/Desktop/IS507 - Data, Statistical Models and Information/Assignments/Assignment_5")

# Import the dataset using read_csv()
raw_dataset <- read.table("16personality.csv", sep="\t", header=TRUE)
dataset <- raw_dataset

# Shape of dataset
dim(dataset)

# Count of missing values
sum(is.na(dataset))

# Delete rows with missing values
dataset <- na.omit(dataset)

# Shape of new dataset after listwise deletion
dim(dataset)

new_dataset <- dataset[, c(1:162)]
new_dataset

# PROBLEM 2

library("factoextra")
library("psych")
library("GGally")
library("REdaS")

#cov(new_dataset)


# A) Number of components using Scree Plot and Knee Method

# You need to run the dataset in PCA function once before you decide on the number of components
pca = prcomp(new_dataset, scale = TRUE)
fviz_eig(pca)

# B) Number of components using Eigen values
plot(pca)
abline(1,0)
sum(get_eig(pca)$eigenvalue>1)

# Calculating standard deviation and variance of the components
std_dev <- pca$sdev
var <- std_dev^2
var[1:10]

# Calculating the proportion of variance explained by components
prop_varex <- var/sum(var)
prop_varex[1:10]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# C) Run a new PCA model based on the number of components

# PCA 1
options(max.print = 10000)

pca_with_7_components = principal(new_dataset, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.45 trying to remove cross loadings
comps = print(pca_with_7_components$loadings, cutoff=0.45, sort=T)


# PCA 2
# Removing variables with 0 intercorrelations
removed_vars = new_dataset %>% select(-c('A8','A9','A10','B7','B8','B9','B11','C1','C2','D7','D8','D9','D10',
                                         'E3','E5','E9','E10','F1','F3','F5','F8','H1','H2','H3','H4','H5','H7','H10',
                                         'I1','I2','I3','I4','I5','I6','J2','J4','J5','J8','K8','K10', 'L6','L8',
                                         'M1','M3','M5','N5','N7','N10','O1','O2','O10','P3','P4','P6','P7'))

pca2_with_7_components = principal(removed_vars, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca2_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.48 trying to remove cross loadings
comps = print(pca2_with_7_components$loadings, cutoff=0.48, sort=T)


# PCA 3
# Removing variables with 0 intercorrelations
removed_vars_2 = removed_vars %>% select(-c('B13','C3','C4','D3','E7','G8','H6','J1','J3','K2','M2','N2','N3','O3'))

pca3_with_7_components = principal(removed_vars_2, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca3_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.47 trying to remove cross loadings
comps = print(pca3_with_7_components$loadings, cutoff=0.47, sort=T)


# PCA 4
# Removing variables with 0 intercorrelations
removed_vars_3 = removed_vars_2 %>% select(-c('E6','J7','N6'))

pca4_with_7_components = principal(removed_vars_3, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca4_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.48 trying to remove cross loadings
comps = print(pca4_with_7_components$loadings, cutoff=0.48, sort=T)


# PCA 5
# Removing variables with 0 intercorrelations
removed_vars_4 = removed_vars_3 %>% select(-c('L10','N1','O9'))

pca4_with_7_components = principal(removed_vars_4, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca4_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.481 trying to remove cross loadings
comps = print(pca4_with_7_components$loadings, cutoff=0.481, sort=T)


# PCA 6
# Removing variables with 0 intercorrelations
removed_vars_5 = removed_vars_4 %>% select(-c('O6'))

pca5_with_7_components = principal(removed_vars_5, rotate="varimax", nfactors = 7, scores = TRUE)

comps = print(pca5_with_7_components$loadings, cutoff=0.4, sort=T)

# New components with cutoff 0.448 trying to remove cross loadings
comps = print(pca5_with_7_components$loadings, cutoff=0.48, sort=T)


# F) Max and Min scores for each component

scores <- pca5_with_7_components$scores

max_scores = apply(scores, 2, max)
min_scores = apply(scores, 2, min)
print(max_scores)
print(min_scores)


# G) Factor analysis

factor_analysis = factanal(removed_vars_5,7)
print(factor_analysis$loadings, cutoff = 0.4, sort=TRUE)

print(factor_analysis$loadings, cutoff = 0.47, sort=TRUE)


