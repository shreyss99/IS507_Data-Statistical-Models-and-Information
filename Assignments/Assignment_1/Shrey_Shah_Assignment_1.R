# Question 4
#====================================

install.packages("tigerstats")

library(tigerstats)

#1. Z < -1.35
#--------------
pnormGC(bound=-1.35,region="below",mean=0,sd=1,graph=TRUE)

#1. Z > 1.48
#--------------
pnormGC(bound=1.48,region="above",mean=0,sd=1,graph=TRUE)

#1. -0.4 < Z < 1.5
#--------------
pnormGC(bound=c(-0.4,1.5),region="between",mean=0,sd=1,graph=TRUE)

#1. |Z| > 2
# => Z < -2 or Z > 2 
#--------------
pnormGC(bound=c(-2,2),region="outside",mean=0,sd=1,graph=TRUE)



# Question 6
#====================================

#b)
# Below is one entire command and needs to be run together
plot(dnorm, from=-3, to=3)
abline(v=1.286, col="blue")
abline(v=0.522, col="red")
text(2.0, 0.4, "Verbal : 1.286",col="blue") 
text(-0.5, 0.1, "Quantitative : 0.522", col="red")

#e)

#Verbal percentile
pnorm(1.286, 0, 1)

#Quantitative percentile
pnorm(0.522, 0, 1)
