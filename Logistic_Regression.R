mydata <- read.csv("hpgtuik_MLE_vs2.csv")

getwd()



attach(mydata)


summary(mydata)

library(psych)
describe(mydata)[,c(3,4,5,8,9,10,11,12)]

library(sandwich)
library(car)
library(lmtest)

# bnec bner mischief theft tfv tov bnec_r bner_r bner_r_high mischief_r theft_r tfv_r tov_r
#  popchange5y + rented + mjrrep + oldhouses + move1y + move5y + unemprate + 
#  postsec + avgdwelval000s + avgrent00s + avgfaminc000s + lowinc + govtass + 
#  immigrants + recimm + vismin + aboriginal + ehet

#terrorist_dicho 
# suicide + marriage + divorce + illiteracy + schooling + migration + population
# density_vs2 + Labour_force + unemployment + GDP + Beds + density + sex_ratio
# voters + ln_voters + Labour_force +


table(terrorist_dicho)
table(terrorist_dicho)/sum(table(terrorist_dicho))

## Logit Regression Model
model1 <- glm(terrorist_dicho ~ illiteracy + migration + GDP + marriage, family = binomial (link = "logit"))

summary(model1)  
vif(model1) #variance inflation factor
sqrt(vif(model1)) > 2 #VIF that may be problematic


## Probit Regression Model
model2 <- glm(terrorist_dicho ~ illiteracy + migration + GDP +  marriage, family = binomial (link = "probit"))

summary(model2)  
vif(model2) #variance inflation factor
sqrt(vif(model2)) > 2 #VIF that may be problematic


## Interpreting results 
# Odds ratio
exp(model1$coefficients)
exp(model2$coefficients)


# Logit model average marginal effects
logitscalar <- mean(dlogis(predict(model1, type = "link"))) 
logitscalar * coef(model1)

# Probit model average marginal effects
logitscalar <- mean(dlogis(predict(model2, type = "link"))) 
logitscalar * coef(model2)

# Model predicted probabilities
pmodel1 <- predict(model1, type = "response")
summary(pmodel1)

pmodel2 <- predict(model2, type = "response")
summary(pmodel2)

# Percent correctly predicted values OR Count R2. Sum the true/all values  
table(true = terrorist_dicho, pred = round(fitted(model1))) # 0.67
table(true = terrorist_dicho, pred = round(fitted(model2))) # 0.67

# McFadden's Pseudo R-squared
model1_0 <- update(model1, formula = terrorist_dicho ~ 1)
McFadden <- 1-as.vector(logLik(model1)/logLik(model1_0))
McFadden # 0.06

model2_0 <- update(model2, formula = terrorist_dicho ~ 1)
McFadden <- 1-as.vector(logLik(model2)/logLik(model2_0))
McFadden # 0.06

# Test of models
lrtest(model1, model2)

