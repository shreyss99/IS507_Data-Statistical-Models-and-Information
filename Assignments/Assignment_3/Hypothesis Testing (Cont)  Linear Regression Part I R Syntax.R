#Research Question:  Is there a difference between ages for Baseline EF (Ejection Fraction)?

#Test for Normality and Check Normality Visually with Histogram with Normal Curve

byf.shapiro(as.matrix(stress_echo$baseEF)~age_tertile2,data=stress_echo)

library(ggplot2)

ggplot(stress_echo, aes(x = baseEF)) +
  geom_histogram(aes(color = age_tertile2, fill=age_tertile2),
                 position = "identity", bins = 10, alpha=0.4) +
  scale_color_manual(values = c("blue", "orange","purple")) +
  scale_fill_manual(values = c("blue", "orange","purple"))

#What do the tests of Normality tell us?


#What test is appropriate to use and why?


#Check for Equal Variances
library(DescTools)
LeveneTest(stress_echo$baseEF ~stress_echo$age_tertile2)

#What do we learn from the LeveneTest?



#One-Way ANOVA

str(stress_echo)

stress_echo$age_tertile2 <- as.factor(stress_echo$age_tertile2)

fit <- aov(baseEF ~ age_tertile2, data=stress_echo)

summary(fit)


#Post-Hoc Analysis - If ANOVA is significant
TukeyHSD(fit)


#Kruskal-Wallis test - Nonparametric ANOVA

kruskal.test(baseEF~age_tertile2, data = stress_echo) # where y1 is numeric and A is a factor


#Post-Hoc Analysis, if Kruskal-Wallis is significant
library(FSA)
dunnTest(baseEF~age_tertile2, data = stress_echo)

################################################################################
################################################################################
#Research Question:  Is there an association between Baseline BP and Baseline EF?


#Check for Normality

shapiro.test(stress_echo$basebp)

shapiro.test(stress_echo$baseEF)

#What is the appropriate correlation to use?


cor.test(stress_echo$basebp, stress_echo$baseEF, alternative = "two.sided", method = "spearman", conf.level = 0.95)
# where x and y are numeric vectors, where they are ordinal or not normally distributed.

################################################################################
#Research Question: What explains a patient's baseline systolic blood pressure?

names(stress_echo)

reg_dataset <- stress_echo[, c(2:4,8, 16, 18:20, 25, 26, 28:31, 36:38)]

str(reg_dataset)

#Check Spearman Correlations

library(corrplot)

corrplot(cor(reg_dataset, method = "spearman"))

corrplot(cor(reg_dataset, method = "spearman"), method="number")

#Removed basehr

names(reg_dataset)

reg_dataset2 <- reg_dataset[,c(2:17)]

names(reg_dataset2)


#Removed basedp

names(reg_dataset)

reg_dataset3 <- reg_dataset[,c(1,2,4:17)]

names(reg_dataset3)

#Using Stepwise Multiple Linear Regression


null = lm(basebp ~ 1, data=reg_dataset3)
null

full = lm(basebp ~ ., data=reg_dataset3)
full

#Forward Regression
train_Forward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(train_Forward)

#Backward Regression
train_Backward = step(full, direction="backward")
summary(train_Backward)

#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
summary(train_Step)

#Using Manual Multiple Linear Regression

#Create Initial Linear Regression Model with Enter Method

model1 <- lm(basebp ~ ., data=reg_dataset)
model1

#Check VIF
library(DescTools)
VIF(model1)

#What are issues with the model?

summary(model1)


