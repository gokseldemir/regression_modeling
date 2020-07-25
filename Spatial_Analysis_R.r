library(ctv)
library(rgdal)
library(spdep)
library(spatial)
library(lmtest)
library(MASS)

## Importing the shapefile. FIrst is home, second is work

data <- readOGR(dsn = "W:/Dropbox/Martin/Data_Manuals/GIS_Data/Census2016", layer = "Vancouver_DAs_Census_2016_UTMz10_ResAmbPop_Crime_SES_Variables_NonZero")


class(data)
plot(data)

summary(data)

## Choose a neighborhood criterion
data_queen <- poly2nb(data, queen = TRUE) # Queen's contiguity
data_queen
summary(data_queen)


data_queen2 = nblag(data_queen,2) #second order Queen's contiguity
data_queen2
summary(data_queen2)

## Visualizing maps  
coords <- coordinates(data) # Create centroids 

## Assign weights to the areas that are linked 
data_queen_w <- nb2listw(data_queen)




## Examine spatial autocorrelation
moran.test(data$tfv_rrate, data_queen_w)

moran.plot(data$tfv_rrate, data_queen_w, labels = as.character(data$FID), xlim = c(-1000,2000), ylim = c(-200,1000))

moran.test(data$YOUNGMALES, data_queen_w)

moran.plot(data$YOUNGMALES, data_queen_w, labels = as.character(data$FID), xlim = c(-1000,2000), ylim = c(-200,1000))


## Linear model - Checking residuals for spatial autocorrelation
#percent married not included...variable is problematic
model1 <- lm(tfv_rrate ~ YOUNGMALES + APARTOTHER + 
               SINGLEDET + SINGLE + LONEPARENT, data = data)
summary(model1)


data$lmresid <- residuals(model1) # new column with residuals from model1
lm.morantest(model1, data_queen_w) # Global Moran's for regression residuals



## Lagrange Multiplier diagnostics for spatial dependence in linear models
lm.LMtests(model1, data_queen_w, test="all")


## Running a spatial error model
modelerr_queen <- errorsarlm(tfv_rrate ~ YOUNGMALES + APARTOTHER + 
                               SINGLEDET + SINGLE + LONEPARENT, data = data, tol.solve = 1.0e-30, data_queen_w)
summary(modelerr_queen)

data$seq1resid <- residuals(modelerr_queen) # new column with residuals from model1
moran.test(data$seq1resid, data_queen_w) # Global Moran's for regression residuals


## Running a spatial lag model

modellag_queen1 <- lagsarlm(tfv_rrate ~ YOUNGMALES + APARTOTHER + SINGLEDET + 
                             SINGLE + LONEPARENT, data = data, tol.solve = 1.0e-30, data_queen_w)
summary(modellag_queen1)

data$slq1resid <- residuals(modellag_queen1) # new column with residuals from model1
moran.test(data$slq1resid, data_queen_w) # Global Moran's for regression residuals



modellag_queen2 <- lagsarlm(tfv_rrate ~ YOUNGMALES +  
                             SINGLE + LONEPARENT, data = data, tol.solve = 1.0e-30, data_queen_w)
summary(modellag_queen2)



lrtest(modellag_queen1, modellag_queen2)


library(sphet) #note the lrtest does not work for sphet

sphet1 <- spreg(tfv_rrate ~ YOUNGMALES + APARTOTHER + SINGLEDET + SINGLE + 
                  LONEPARENT, data=data , listw= data_queen_w,  het = TRUE, verbose = FALSE)

summary(sphet1)

data$sphetq1resid <- residuals(sphet1) # new column with residuals from model1
moran.test(data$sphetq1resid, data_queen_w) # Global Moran's for regression residuals
