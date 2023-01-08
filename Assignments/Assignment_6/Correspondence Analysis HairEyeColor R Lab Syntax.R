
# This dataset is a 3D table with two "pages"
# One for men and one for women

head(HairEyeColor)

str(HairEyeColor)

Men = HairEyeColor[,,1]
Women = HairEyeColor[,,2]

#Contingency Tables
Men
Women

library(ca)

prop.table(Men, 1)  # Row Percentages
prop.table(Men, 2)  # Col Percentages

fit = ca(Men)
summary(fit)
fit

plot(fit)
Men

plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(T, T))

# Now lets look at women
prop.table(Women, 1)  # Row Percentages
prop.table(Women, 2)  # Col Percentages

fit = ca(Women)
summary(fit)
fit

plot(fit)
Women

plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

library(vcd)
mosaic(Men, shade=TRUE, legend=TRUE)
