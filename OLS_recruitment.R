getwd()

mydata <- read.csv("hpgtuik_ready_vs2.csv")

options(max.print=1000000)
attach(mydata)

summary(mydata)
library(psych)

describe(mydata)[,c(3,4,5,8,9,10,11,12)]
# des <- describe(mydata) #these are just two other options to name your descriptive something and then print in a particular way
# print(des,digits=3)

round(cor(mydata, method = c("pearson")),3) #rounds the correlation table to 3 decimal places, spearman or pearson

res <- cor(mydata)
round(res, 2)

cor.test(mydata$terrorists,mydata$suicide, method=c("pearson")) #test correlation significance. spearman or pearson
cor.test(mydata$terrorists,mydata$marriage, method=c("pearson"))
cor.test(mydata$terrorists,mydata$divorce, method=c("pearson"))
cor.test(mydata$terrorists,mydata$illiteracy, method=c("pearson"))
cor.test(mydata$terrorists,mydata$schooling, method=c("pearson"))
cor.test(mydata$terrorists,mydata$migration, method=c("pearson"))
cor.test(mydata$terrorists,mydata$population, method=c("pearson"))
cor.test(mydata$terrorists,mydata$density_vs2, method=c("pearson"))
cor.test(mydata$terrorists,mydata$Labour_force, method=c("pearson"))
cor.test(mydata$terrorists,mydata$unemployment, method=c("pearson"))
cor.test(mydata$terrorists,mydata$GDP, method=c("pearson"))
cor.test(mydata$terrorists,mydata$Beds, method=c("pearson"))
cor.test(mydata$terrorists,mydata$density, method=c("pearson"))
cor.test(mydata$terrorists,mydata$sex_ratio, method=c("pearson"))
cor.test(mydata$terrorists,mydata$voters, method=c("pearson"))
cor.test(mydata$terrorists,mydata$ln_voters, method=c("pearson"))

# name of the variables for convenience 
# mydata$terrorists,mydata$suicide, mydata$marriage,mydata$divorce,mydata$illiteracy,mydata$schooling,mydata$migration,
# mydata$population,mydata$density_vs2,mydata$Labour_force,mydata$unemployment,mydata$GDP,mydata$Beds,mydata$density,mydata$sex_ratio,

round(cor(mydata$terrorists, mydata$voters),3) #correlation for 2 variables, rounds the correlation table to 3 decimal places
plot(mydata$terrorists, mydata$voters) # plots your data


cor.test(mydata$terrorists, mydata$Beds, method=c("spearman"))
cor.test(mydata$popchange5y, mydata$rented, method=c("spearman"))
cor.test(mydata$unemprate, mydata$postsec, method=c("spearman"))
cor.test(mydata$avgdwelval000s, mydata$avgfaminc000s, method=c("spearman"))

par(mfrow = c(2,2))
plot(mydata$terrorists, mydata$divorce)
plot(mydata$terrorists, mydata$illiteracy)
plot(mydata$terrorists, mydata$schooling)
plot(mydata$terrorists, mydata$migration)

par(mfrow = c(2,2))
plot(mydata$terrorists, mydata$GDP)
plot(mydata$terrorists, mydata$Labour_force)
plot(mydata$terrorists, mydata$Beds)
plot(mydata$terrorists, mydata$ln_voters)

library(sandwich)
library(car)
library(lmtest)

# bnec bner mischief theft tfv tov bnec_r bner_r bner_r_high mischief_r theft_r tfv_r tov_r
#  popchange5y + rented + mjrrep + oldhouses + move1y + move5y + unemprate + 
#  postsec + avgdwelval000s + avgrent00s + avgfaminc000s + lowinc + govtass + 
#  immigrants + recimm + vismin + aboriginal + ehet

model1 <- lm(terrorists ~ suicide + marriage + divorce + illiteracy + schooling + migration + density_vs2 +
             GDP + Beds + sex_ratio + ln_voters + density, data=mydata)

summary(model1)
vif(model1) # variance inflation factors
sqrt(vif(model1)) > 2 # VIF that may be problematic
bptest(model1) # Breusch-Pagan test for heteroskedasticity
dwtest(model1) # Durbin-Watson test for autocorrelation
coeftest(model1, vcov = hccm) #White's heteroskedasticity-corrected covariance matrix
coeftest(model1, vcov = vcovHAC) #White's heteroskedasticity-autocorrelation consistent covariance matrix

residuals.m1 = resid(model1)
plot(residuals.m1,mydata$terrorists)

model2 <- lm(terrorists ~ divorce + illiteracy + migration + GDP + Beds + ln_voters, data=mydata)

summary(model2)
vif(model2) 
sqrt(vif(model2)) > 2 
bptest(model2)
dwtest(model2)
coeftest(model2, vcov = vcovHAC)

residuals.m2 = resid(model2)
plot(residuals.m2,mydata$terrorists)


lrtest(model1, model2) # joint significance test


model3 <- lm(terrorists ~ divorce + illiteracy + migration + GDP + Beds , data=mydata)

summary(model3)
vif(model3) 
sqrt(vif(model3)) > 2 
bptest(model3)
dwtest(model3)
coeftest(model3, vcov = vcovHAC)

residuals.m3 = resid(model3)
p0lot(residuals.m3,mydata$terrorists).
0