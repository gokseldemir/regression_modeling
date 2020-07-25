getwd()

# poisson distribution

N <- 10000
x <- rpois(N, 20)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue',
     main='Poisson distribution, lambda=20')
lines(density(x,bw=1), col='red', lwd=3)

# negative binomial distribution

N <- 10000
x <- rnbinom(N, 20, 0.01)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue',
     main='Negative binomial distribution, n=20, p=.95')
lines(density(x,bw=1), col='red', lwd=3)


####################################################################################

#first data is for SFU, second is for home
getwd()
mydata <- read.csv("HPG_863_assn_wTurkiye.csv")

attach(mydata)
summary(mydata)

library(MASS)
library(pscl)
library(AER)
library(sandwich)
library(lmtest)
library(psych)
library(mfx)

describe(mydata)[,c(3,4,5,8,9,10,11,12)]

hist(member)

glm.control(epsilon = 1e-8, maxit = 1000, trace = FALSE)

#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$member, mydata$schooling),3)
round(cor(mydata$member, mydata$households),3)
round(cor(mydata$member, mydata$marriage),3)
round(cor(mydata$member, mydata$divorce),3)
round(cor(mydata$member, mydata$waste_serv),3)
round(cor(mydata$member, mydata$sex_ratio),3)
round(cor(mydata$member, mydata$beds),3)
round(cor(mydata$member, mydata$migration),3)
round(cor(mydata$member, mydata$suicide),3)
round(cor(mydata$member, mydata$population),3)

cor.test(mydata$member,mydata$schooling, method=c("pearson")) #test correlation significance. spearman or pearson
cor.test(mydata$member,mydata$households, method=c("pearson"))
cor.test(mydata$member,mydata$marriage, method=c("pearson"))
cor.test(mydata$member,mydata$divorce, method=c("pearson"))
cor.test(mydata$member,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$member,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$member,mydata$beds, method=c("pearson"))
cor.test(mydata$member,mydata$migration, method=c("pearson"))
cor.test(mydata$member,mydata$suicide, method=c("pearson"))
cor.test(mydata$member,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$schooling, mydata$households),3)
round(cor(mydata$schooling, mydata$marriage),3)
round(cor(mydata$schooling, mydata$divorce),3)
round(cor(mydata$schooling, mydata$waste_serv),3)
round(cor(mydata$schooling, mydata$sex_ratio),3)
round(cor(mydata$schooling, mydata$beds),3)
round(cor(mydata$schooling, mydata$migration),3)
round(cor(mydata$schooling, mydata$suicide),3)
round(cor(mydata$schooling, mydata$population),3)

cor.test(mydata$schooling,mydata$households, method=c("pearson"))
cor.test(mydata$schooling,mydata$marriage, method=c("pearson"))
cor.test(mydata$schooling,mydata$divorce, method=c("pearson"))
cor.test(mydata$schooling,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$schooling,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$schooling,mydata$beds, method=c("pearson"))
cor.test(mydata$schooling,mydata$migration, method=c("pearson"))
cor.test(mydata$schooling,mydata$suicide, method=c("pearson"))
cor.test(mydata$schooling,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$households, mydata$marriage),3)
round(cor(mydata$households, mydata$divorce),3)
round(cor(mydata$households, mydata$waste_serv),3)
round(cor(mydata$households, mydata$sex_ratio),3)
round(cor(mydata$households, mydata$beds),3)
round(cor(mydata$households, mydata$migration),3)
round(cor(mydata$households, mydata$suicide),3)
round(cor(mydata$households, mydata$population),3)

cor.test(mydata$households,mydata$marriage, method=c("pearson"))
cor.test(mydata$households,mydata$divorce, method=c("pearson"))
cor.test(mydata$households,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$households,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$households,mydata$beds, method=c("pearson"))
cor.test(mydata$households,mydata$migration, method=c("pearson"))
cor.test(mydata$households,mydata$suicide, method=c("pearson"))
cor.test(mydata$households,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$marriage, mydata$divorce),3)
round(cor(mydata$marriage, mydata$waste_serv),3)
round(cor(mydata$marriage, mydata$sex_ratio),3)
round(cor(mydata$marriage, mydata$beds),3)
round(cor(mydata$marriage, mydata$migration),3)
round(cor(mydata$marriage, mydata$suicide),3)
round(cor(mydata$marriage, mydata$population),3)

cor.test(mydata$marriage,mydata$divorce, method=c("pearson"))
cor.test(mydata$marriage,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$marriage,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$marriage,mydata$beds, method=c("pearson"))
cor.test(mydata$marriage,mydata$migration, method=c("pearson"))
cor.test(mydata$marriage,mydata$suicide, method=c("pearson"))
cor.test(mydata$marriage,mydata$population, method=c("pearson"))


#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$divorce, mydata$waste_serv),3)
round(cor(mydata$divorce, mydata$sex_ratio),3)
round(cor(mydata$divorce, mydata$beds),3)
round(cor(mydata$divorce, mydata$migration),3)
round(cor(mydata$divorce, mydata$suicide),3)
round(cor(mydata$divorce, mydata$population),3)

cor.test(mydata$divorce,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$divorce,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$divorce,mydata$beds, method=c("pearson"))
cor.test(mydata$divorce,mydata$migration, method=c("pearson"))
cor.test(mydata$divorce,mydata$suicide, method=c("pearson"))
cor.test(mydata$divorce,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$waste_serv, mydata$sex_ratio),3)
round(cor(mydata$waste_serv, mydata$beds),3)
round(cor(mydata$waste_serv, mydata$migration),3)
round(cor(mydata$waste_serv, mydata$suicide),3)
round(cor(mydata$waste_serv, mydata$population),3)

cor.test(mydata$waste_serv,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$waste_serv,mydata$beds, method=c("pearson"))
cor.test(mydata$waste_serv,mydata$migration, method=c("pearson"))
cor.test(mydata$waste_serv,mydata$suicide, method=c("pearson"))
cor.test(mydata$waste_serv,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$sex_ratio, mydata$beds),3)
round(cor(mydata$sex_ratio, mydata$migration),3)
round(cor(mydata$sex_ratio, mydata$suicide),3)
round(cor(mydata$sex_ratio, mydata$population),3)

cor.test(mydata$sex_ratio,mydata$beds, method=c("pearson"))
cor.test(mydata$sex_ratio,mydata$migration, method=c("pearson"))
cor.test(mydata$sex_ratio,mydata$suicide, method=c("pearson"))
cor.test(mydata$sex_ratio,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$beds, mydata$migration),3)
round(cor(mydata$beds, mydata$suicide),3)
round(cor(mydata$beds, mydata$population),3)

cor.test(mydata$beds,mydata$migration, method=c("pearson"))
cor.test(mydata$beds,mydata$suicide, method=c("pearson"))
cor.test(mydata$beds,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$migration, mydata$suicide),3)
round(cor(mydata$migration, mydata$population),3)

cor.test(mydata$migration,mydata$suicide, method=c("pearson"))
cor.test(mydata$migration,mydata$population, method=c("pearson"))

#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$suicide, mydata$population),3)

cor.test(mydata$suicide,mydata$population, method=c("pearson"))


#-----------------------------------------------------------------------------
#correlation for 2 variables, rounds the correlation table to 3 decimal places
round(cor(mydata$population, mydata$schooling),3)
round(cor(mydata$population, mydata$households),3)
round(cor(mydata$population, mydata$marriage),3)
round(cor(mydata$population, mydata$divorce),3)
round(cor(mydata$population, mydata$waste_serv),3)
round(cor(mydata$population, mydata$sex_ratio),3)
round(cor(mydata$population, mydata$beds),3)
round(cor(mydata$population, mydata$migration),3)
round(cor(mydata$population, mydata$suicide),3)
round(cor(mydata$population, mydata$member),3)

cor.test(mydata$population,mydata$schooling, method=c("pearson")) #test correlation significance. spearman or pearson
cor.test(mydata$population,mydata$households, method=c("pearson"))
cor.test(mydata$population,mydata$marriage, method=c("pearson"))
cor.test(mydata$population,mydata$divorce, method=c("pearson"))
cor.test(mydata$population,mydata$waste_serv, method=c("pearson"))
cor.test(mydata$population,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$population,mydata$beds, method=c("pearson"))
cor.test(mydata$population,mydata$migration, method=c("pearson"))
cor.test(mydata$population,mydata$suicide, method=c("pearson"))
cor.test(mydata$population,mydata$member, method=c("pearson"))

par(mfrow = c(2,2))
plot(mydata$member, mydata$marriage)
plot(mydata$member, mydata$waste_serv)
plot(mydata$member, mydata$sex_ratio)
plot(mydata$member, mydata$population)

# Poisson Model

poisson <- glm(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
               + beds + migration + suicide + population1, family = poisson)

summary(poisson)
vif(poisson) # variance inflation factors
sqrt(vif(poisson)) > 2 # VIF that may be problematic
AIC(poisson)
dispersiontest(poisson)
dispersiontest(poisson, trafo=2) #most common functional form of variance

## Interpreting results: Odds ratio
exp(poisson$coefficients)

### Poisson regression odds ratios

poisson1 <- poissonirr(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                       + beds + migration + suicide + population1, data=mydata, 
                       robust = TRUE, clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

print(poisson1)

### Poisson regression marginal effects

poisson2 <- poissonmfx(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                       + beds + migration + suicide + population1, data=mydata, atmean = FALSE, robust = TRUE, 
                       clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

print(poisson2)

#Negative Binomial Model

negbin1 <- glm.nb(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                  + beds + migration + suicide + population1 )

summary(negbin1)
vif(negbin1) # variance inflation factors
sqrt(vif(negbin1)) > 2 # VIF that may be problematic
AIC(negbin1)
coeftest(negbin1, vcov = vcovHAC)

## Interpreting results: relative risk ratios
exp(negbin1$coefficients)

# Negative binomial regression IIR values

negbin11 <- negbinirr(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                      + beds + migration + suicide + population1, data=mydata, 
                      robust = TRUE, clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = glm.control())
print(negbin11)


# Negative binomial regression marginal effects


negbin12 <- negbinmfx(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                      + beds + migration + suicide + population1, data=mydata, atmean=FALSE, robust = TRUE, 
                      clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = glm.control())
print(negbin12)

# lrtest(negbin1, negbin2)

# Hurdle/Truncated/Two-Part Poisson Model...first set of variables is for the count, second set is for the binary

hpoisson <- hurdle(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                   + beds + migration + suicide + population1 | schooling + households + marriage + divorce + waste_serv + sex_ratio
                   + beds + migration + suicide + population1, link = "logit", dist = "poisson")

summary(hpoisson)
AIC(hpoisson)

# Hurdle/Truncated/Two-Part Negative Binomial Model...first set of variables is for the count, second set is for the binary


hnegbin <- hurdle(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                  + beds + migration + suicide + population1 | schooling + households + marriage + divorce + waste_serv + sex_ratio
                  + beds + migration + suicide + population1, link = "logit", dist = "negbin")

summary(hnegbin)
AIC(hnegbin)

# Zero-Inflated Poisson Model...first set of variables is for the count, second set is for the binary

zip <- zeroinfl(member ~ schooling + households + marriage + divorce + waste_serv + sex_ratio
                + beds + migration + suicide + population1| schooling + households + marriage + divorce + waste_serv + sex_ratio
                + beds + migration + suicide + population1, link = "logit", dist = "poisson")

summary(zip)
AIC(zip)


# Zero-Inflated Negative Binomial Model...first set of variables is for the count, second set is for the binary


zinb1 <- zeroinfl(member ~ schooling + marriage + divorce + waste_serv + sex_ratio
                 + beds + migration + suicide  + population1 | schooling + marriage + divorce + waste_serv + sex_ratio
                 + beds + migration + suicide + population1, link = "logit", dist = "negbin")

summary(zinb1)
AIC(zinb1)
vif(zinb) # variance inflation factors
sqrt(vif(zinb)) > 2 # VIF that may be problematic


vuong(zinb1, negbin1) # model1 > model2, with p-value 
