mydata <- read.csv("/Users/gfsd/Desktop/OneDrive - sfu.ca/SFU/CRIM863/assn3_panel_data/Canada_Provinces_Crime_Data_1981_2013.csv")

attach(mydata)

summary(mydata)

library(sandwich)
library(car)
library(lmtest)

library(plm)

# Year           Prov        lnr_tot       lnr_violent      lnr_sexass       lnr_hom           lnr_ass  
# lnr_rob         lnr_prop        lnr_shop       lnr_theft        lnr_bne         lnr_tov         lnr_tfv  
# ln_ymales + ymales_per + Gini + lngdp + lngdpt1 + gdp_bills + gdpt1_bills 
# Unemployment.Rate  + Unemployment.Rate..t.1 + ln_unemp53 + unemp53_per + ln_lowinc + lowinc_per 
# alc_per + ln_alcohol + Officers.per.Capita + Incidents.per.Officer + ln_numoff + ln_off_p_cap 
# ln_inc_p_off + ln_correxp + correxp_per + imm_per + iymales_per + netim_per
# netiymales_per +pimm_per + pimmym_per + pimmn_per + pimmymn_per + imm_per5   
# iymales_per5 + netim_per5 + netiymales_per5 + pimm_per5 + pimmym_per5 + pimmn_per5
# pimmymn_per5 + BC + AB + SK + MN + ON + QU + NB + NS + PEI + NFLD    

model1 <- lm(lnr_bne ~ Unemployment.Rate + unemp53_per +  ymales_per  + pimmymn_per5
             + Officers.per.Capita + alc_per + Incidents.per.Officer + iymales_per + CUSFTA +	NAFTA, data=mydata)

summary(model1)
vif(model1) # variance inflation factors
sqrt(vif(model1)) > 2 # VIF that may be problematic
bptest(model1) # Breusch-Pagan test for heteroskedasticity
dwtest(model1) # Durbin-Watson test for autocorrelation
coeftest(model1, vcov = hccm) #White's heteroskedasticity-corrected covariance matrix
coeftest(model1, vcov = vcovHAC) #White's heteroskedasticity-autocorrelation consistent covariance matrix

model2 <- lm(lnr_bne ~ Unemployment.Rate +  ymales_per  + pimmymn_per5
             + alc_per + Incidents.per.Officer + iymales_per + CUSFTA +	NAFTA + 
               BC + AB + SK + MN + ON + QU + NB + NS + PEI , data=mydata)

summary(model2)
vif(model2) # variance inflation factors
sqrt(vif(model2)) > 2 # VIF that may be problematic
bptest(model2) # Breusch-Pagan test for heteroskedasticity
dwtest(model2) # Durbin-Watson test for autocorrelation
coeftest(model2, vcov = hccm) #White's heteroskedasticity-corrected covariance matrix
coeftest(model2, vcov = vcovHAC) #White's heteroskedasticity-autocorrelation consistent covariance matrix


#setting up panel data
# id is cross-sectional variable identifier and t is the temporal variable identifier
#data need to be sorted by id then by t

pdata <- pdata.frame(mydata, index=c("id","t"))

#Pooled OLS model

pooling <- plm(lnr_bne ~ Unemployment.Rate +  ymales_per  + pimmymn_per5
               + alc_per + Incidents.per.Officer + iymales_per + CUSFTA +	NAFTA + 
                 BC + AB + SK + MN + ON + QU + NB + NS + PEI , data=pdata, model="pooling")
summary(pooling)


#Fixed effects model

fixed <- plm(lnr_bne ~ Unemployment.Rate +  ymales_per  + pimmymn_per5
             + alc_per + Incidents.per.Officer + iymales_per + CUSFTA +	NAFTA + 
               BC + AB + SK + MN + ON + QU + NB + NS + PEI , data=pdata, model="within")
summary(fixed)

bptest(fixed)

#Random effects #iymales_per + 
random <- plm(lnr_bne ~ Unemployment.Rate +  ymales_per  + pimmymn_per5
              + alc_per + Incidents.per.Officer + iymales_per + CUSFTA +	NAFTA + 
                BC + AB + SK + MN + ON + QU + NB + NS + PEI , data=pdata, model="random")
summary(random)

#LM test for random effects versus OLS
plmtest(pooling)


#LM test for fixed effects versus OLS
pFtest(fixed, pooling)


#Hausman test for fixed versus random effects
phtest(random, fixed)




