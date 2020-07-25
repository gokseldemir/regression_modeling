
install.packages("rms")
library(rms)

mydata <- read.csv("W:/SFU_Vault/crim863da.csv")

mydata <- read.csv("W:/SFU_Vault/crim863ct.csv")


attach(mydata)



# bnec bner mischief theft tfv tov bnec_r bner_r bner_r_high mischief_r theft_r tfv_r tov_r
#  popchange5y + rented + mjrrep + oldhouses + move1y + move5y + unemprate + 
#  postsec + avgdwelval000s + avgrent00s + avgfaminc000s + lowinc + govtass + 
#  immigrants + recimm + vismin + aboriginal + ehet


Y <- cbind(bne_rank)
X <- cbind(recimm5yrs, vismin, unemprate, rented, meddwelv_a, medinc000s)
Xvar <- c("recimm5yrs", "vismin", "unemprate", "rented", "meddwelv_a", "medinc000s")

# Descriptive statistics
summary(X)
table(Y)
round(table(Y)/sum(table(Y)),3)

# Ordered logit model coefficients
ddist<- datadist(Xvar)
options(datadist='ddist')

ologit<- lrm(Y ~ X, data=mydata)
print(ologit)

# Ordered logit model odds ratio
exp(ologit$coefficients)

# Ordered logit predicted probabilities
# xmeans <- colMeans(X)
# newdata1 <- data.frame(t(xmeans))
fitted <- predict(ologit, newdata=mydata, type="fitted.ind")
colMeans(fitted)
round(table(Y)/sum(table(Y)),5)




library(MASS)

fbne = factor(bne_rank)

ologit <- polr(fbne ~ recimm5yrs + vismin + unemprate + rented + meddwelv_a + medinc000s, data = mydata, Hess=TRUE, model=TRUE, method = c("logistic"))
summary(ologit)

# Ordered logit model odds ratio
exp(ologit$coefficients)

# Testing proportional odds (parallel lines) assumption
# Brant test

install.packages("brant")
library(brant)

brant(ologit)



