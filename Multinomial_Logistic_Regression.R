mydata <- read.csv("hpgtuik_MLE_vs2.csv")
mydata <- read.csv("W:/TPS_Clusters_Violence_Census.csv")


attach(mydata)
summary(mydata)

library(psych)
describe(mydata)[,c(3,4,5,8,9,10,11,12)]

library(sandwich)
library(car)
library(lmtest)

install.packages("mlogit")
library(mlogit)



# bnec bner mischief theft tfv tov bnec_r bner_r bner_r_high mischief_r theft_r tfv_r tov_r
#  popchange5y + rented + mjrrep + oldhouses + move1y + move5y + unemprate + 
#  postsec + avgdwelval000s + avgrent00s + avgfaminc000s + lowinc + govtass + 
#  immigrants + recimm + vismin + aboriginal + ehet

table(Aggregate)
round(table(Aggregate)/sum(table(Aggregate)),3)

table(Assault)
round(table(Assault)/sum(table(Assault)),3)



#reshape data from wide to long format
mldata<-mlogit.data(mydata, choice="LISA", shape="wide")
mldata[1:20,]

length(LISA)
length(Assault)
#and
nrow(mldata)

## Multinomial Logit Regression Model; "LISA ~ 1" means no variables specific to outcome...conditional logit model
mlogit.model1 <- mlogit(LISA ~ 1 | recimm5yrs + vismin + unemprate + rented + 
                          meddwelv_a + medinc000s, data = mldata, reflevel="Insig")

summary(mlogit.model1)  

## Interpreting results: Odds ratio
exp(mlogit.model1$coefficients)

# Multinomial logit regression marginal effects
m.effects <- function (object, covariate = NULL, type = c("aa", "ar", "rr", 
                                                          "ra"), data = NULL, ...) 
{
  type <- match.arg(type)
  if (is.null(data)) {
    P <- predict(object, returnData = TRUE)
    data <- attr(P, "data")
    attr(P, "data") <- NULL
  }
  else P <- predict(object, data)
  newdata <- data
  J <- length(P)
  alt.levels <- names(P)
  pVar <- substr(type, 1, 1)
  xVar <- substr(type, 2, 2)
  cov.list <- strsplit(as.character(attr(formula(object), "rhs")), " + ", fixed = TRUE)
  rhs <- sapply(cov.list, function(x) length(na.omit(match(x, 
                                                           covariate))) > 0)
  rhs <- (1:length(cov.list))[rhs]
  eps <- 1e-05
  if (rhs %in% c(1, 3)) {
    if (rhs == 3) {
      theCoef <- paste(alt.levels, covariate, sep = ":")
      theCoef <- coef(object)[theCoef]
    }
    else theCoef <- coef(object)[covariate]
    me <- c()
    for (l in 1:J) {
      newdata[l, covariate] <- data[l, covariate] + eps
      newP <- predict(object, newdata)
      me <- rbind(me, (newP - P)/eps)
      newdata <- data
    }
    if (pVar == "r") 
      me <- t(t(me)/P)
    if (xVar == "r") 
      me <- me * matrix(rep(data[[covariate]], J), J)
    dimnames(me) <- list(alt.levels, alt.levels)
  }
  if (rhs == 2) {
    newdata[, covariate] <- data[, covariate] + eps
    newP <- predict(object, newdata)
    me <- (newP - P)/eps
    if (pVar == "r") 
      me <- me/P
    if (xVar == "r") 
      me <- me * data[[covariate]]
    names(me) <- alt.levels
  }
  me
}

m <- mlogit(terrorist_ML ~ 1 | illiteracy + migration + density + marriage , data = mldata, reflevel="0")


#z <- with(mldata, data.frame(recimm5yrs=mean(recimm5yrs), vismin=mean(vismin), unemprate=mean(unemprate), 
#                             rented=mean(rented), meddwelv_a=mean(meddwelv_a), medinc000s=mean(medinc000s)))

z <- with(mldata, data.frame(recimm5yrs=tapply(recimm5yrs, index(m)$alt, mean), vismin=tapply(vismin, index(m)$alt, mean), 
                             unemprate=tapply(unemprate, index(m)$alt, mean), rented=tapply(rented, index(m)$alt, mean), 
                             meddwelv_a=tapply(meddwelv_a, index(m)$alt, mean), medinc000s=tapply(medinc000s, index(m)$alt, mean)))


z #check to see if the means are calculated correctly
describe(mydata)[,c(3,4,5,8,9,10,11,12)]
round(m.effects(mlogit.model1, covariate="unemprate", data=z),5)


# Hausman-McFadden test of independence of irrelevant alternative
m1 <- mlogit(terrorist_ML ~ 1 | illiteracy + migration + density + marriage , data = mldata, reflevel="0")
m2 <- mlogit(terrorist_ML ~ 1 | illiteracy + migration + density + marriage , data = mldata, reflevel="0", 
             alt.subset=c("0", "1", "1"))
hmftest(m1, m2)


## Probit Regression Model
mprobit.model1 <- mlogit(LISA ~ 1 | recimm5yrs + vismin + unemprate + rented + 
                           meddwelv_a + medinc000s , data = mldata, reflevel="Insig", probit=TRUE)

summary(mprobit.model1)  

## Interpreting results: Odds ratio
exp(mprobit.model1$coefficients)

# Multinomial probit regression marginal effects
effects(mprobit.model1, covariate="medinc000s", data=z)



## Another option for multinomial logistic regression

install.packages("nnet")
library(nnet)

mydata$terrorist_ML <- relevel(mydata$terrorist_ML, ref = "1")
test <- multinom(terrorist_ML ~ illiteracy + migration + density + marriage, data = mydata)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))
