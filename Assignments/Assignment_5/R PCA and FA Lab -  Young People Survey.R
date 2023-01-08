#Author:  Stephanie Besser
#Date: March 25, 2021

#Lab: Principal Component Analysis (PCA) and Factor Analysis in R

#Using Young People Survey Datasets

#Note: Run Shortcut:  CTRL+Enter

#Libraries
library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations
##############################################################################################

#Set Working Directory
setwd('C:/Users/sbess/Downloads')


#Read in Datasets

responses <- read.csv(file="Responses.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(responses)
#1,010-Sample Size and 150 variables

#Show for first 6 rows of data
head(responses)

names(responses)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(responses))
#571 total missing values (571 cells with missing data)


#Treat Missing Values

#Listwise Deletion
responses2 <- na.omit(responses)

#Check new data has no missing data
sum(is.na(responses2))


#################################################################################################################

#Show Structure of Dataset
str(responses2, list.len=ncol(responses2))

str(responses2)

#Show column Numbers
names(responses2)

#Categorical Variables (Var_num):  Smoking (74), Alcohol (75), Punctuality (108), Lying (109), Internet.usuage (133), Gender (145), 
#                         Left...right.handed (146), Education (147), Only.child(148), Village.town (149), House...block.of.flats (150)


#Create new subsets of data (Numeric Variables Only)

responses3 <- responses2[,c(1:73,76,77:107,110:132,134:140,141:144)]

music <- responses2[,c(1:19)]
movie <- responses2[,c(20:31)]
hobbies_interests <- responses2[,c(32:63)]
phobias <- responses2[,c(64:73)]
health <- responses2[,76]
personal_views_opinions <- responses2[,c(77:107,110:132)]
spending <- responses2[,c(134:140)]
demographics <- responses2[,c(141:144)]



#Show descriptive statistics

#Normality Rule of Thumb with Skewnewss and Kurtosis (think normal bell curve):
#Short Way:
#If skewnewss is close to 0, the distribution is normal.
#If Kurtosis is -3 or 3, the distribution is normal.

#If skewness is less than -1 or greater than 1, the distribution is highly skewed.
#If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
#If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.

#Test of Contradiction

#Ho:  Data is Normal (p > 0.05)
#Ha:  Data is Not Normal (p < 0.05)

#Shapiro-Wilk test (<1,000 sample size)
shapiro.test(responses3$Mathematics)

#Anderson-Darling Test (1,000-2,000 Sample Size)
library(DescTools)

AndersonDarlingTest(responses3$Mathematics)

#Jarque-Bera test (>3,000 Sample Size)

JarqueBeraTest(responses3$Mathematics)


library(psych)
describe(music)
describe(movie)
describe(hobbies_interests)
describe(phobias)
describe(health)
describe(public_views_opinions)
describe(spending)
describe(demographics)



#############################################################################################################

#Exploratory Analysis Graphing

#In order to use the graphing functions, you need to chunk the data into smaller subsets of data.

#GGpairs
p1 <- ggpairs(music[,c(2:8)])
p1

#Note: Does not show boxplots or bar charts, because there is no categorical variable in the music dataset

# To save the ggplot as png
ggsave("p1.png")

#Boxplots
boxplot(music[,2:8],data=music, main="Personality Traits, Views, and Opinions", xlab="Personality", col = c("blue","green"))


#Check for Multicollinearity with Correlations


M<-cor(music, method="spearman")
M

round(M,2)

#personality
corrplot(cor(M,method="spearman"), method = "number", type = "lower")
#High Correlations: God, Final Judgement, Number.of.friends, and Loneliness

#GGplot Correlation
ggcorr(music[,1:20], method = c("pairwise","spearman"), label=TRUE)

ggcorr(music, method = c("pairwise","spearman"))

# Run a correlation test to see how correlated the variables are.  Which correlations are significant
options("scipen"=100, "digits"=3)

round(cor(music), 2)
MCorrTest = corr.test(music, adjust="none")
MCorrTest

M = MCorrTest$p
M
round(M,2)

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)


##################################################
#PCA/FA
##################################################

#Test KMO Sampling Adequacy

library(psych)
KMO(music)
#Overall MSA =  0.77

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(music)
#p-value < 2.22e-16 (Very Small Number)


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(music, check.keys = TRUE)
#raw_alpha = 0.74

library(fmsb)
CronbachAlpha(music)

#######################################################
#Parallel Analysis (Horn's parallel analysis)

#Created a Psychologist John L. Horn in 1965

#Closest to Heuristic Determination of Number of Components or Factors

#Compares actual eigenvalues with ones from a Monto-Carlo simulated dataset of
#the same size

#Dependent upon sample size, correlation coefficient, and how items fall on 
#components

library(psych)

comp <- fa.parallel(music)
comp

#######################################################
#Create PCA
p = prcomp(music, center=T, scale=T)
p

#Check Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

########################################################

# The Psych package has a wonderful PCA function that allows many more options
# including build-in factor rotation, specifying a number of factors to include 
# and automatic "score" generation

#Best Way to Conduct PCA Analysis

music2<-music[,3:19]

p2 = psych::principal(music2, rotate="varimax", nfactors=3, scores=TRUE)
p2

p3<-print(p2$loadings, cutoff=.47, sort=T)


#PCAs Other Available Information

ls(p2)

p2$values
p2$communality
p2$rot.mat

########################################################################################

#Calculating scores

scores <- p2$scores

cor(scores)

summary(scores)

scores_1 <- scores[,1]

min_score <- min(scores_1)
min_score

max_score <- max(scores_1)
max_score

summary(scores_1)

scores_2 <- scores[,2]
scores_3 <- scores[,3]


#Conducting Factor Analysis

fit = factanal(music2, 3)
print(fit$loadings, cutoff=.4, sort=T)

#######################################################################################
#Additional PCA Visualizations

#Using Factoextra
library(factoextra)

p3 <- prcomp(music2, scale = TRUE) 
fviz_eig(p3)

#PCA Individuals
pI<-fviz_pca_ind(p3,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
pI

#PCA Variables
pca_var<-fviz_pca_var(p3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca_var

#Biplot
bi_plot<-fviz_pca_biplot(p3, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

bi_plot

library("FactoMineR")
p4 <- PCA(music2, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib, 11)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(p4, choice = "var", axes = 2, top = 10)


library(ade4)
p5 <- dudi.pca(music2,
               scannf = FALSE,   # Hide scree plot
               nf = 3          # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables2$contrib, 11)

library("corrplot")
corrplot(variables2$contrib, is.corr=FALSE)    