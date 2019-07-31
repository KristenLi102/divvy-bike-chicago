# read data
bike_raw <- read.csv("~/Downloads/bike.csv")

# library
library(dplyr)
library(glmnet)
library(car)
library(ggplot2)

# select criminal variables
bike <- bike_raw %>% select(ID, PARK_AREA_ACRES,CTA_BUS_STATIONS,CTA_TRAIN_STATIONS,BIKE_ROUTES,Limited_Business_License,
                        Retail_Food_Establishment,CAPACITY,PER_CAPITA_INCOME,POPULATION_SQ_MILE,CBD,MINORITY,EDU,
                        ASSAULT,BATTERY,BURGLARY,CRIMINAL_TRESPASS,DECEPTIVE_PRACTICE,HOMICIDE,NARCOTICS,ROBBERY,
                        THEFT,trips)
names(bike_raw)

# EDA
par(mfrow=c(1,1))
plot(bike$CTA_BUS_STATIONS,bike$CTA_TRAIN_STATIONS)
cor(bike$CTA_BUS_STATIONS,bike$CTA_TRAIN_STATIONS)
bike$sq_EDU = bike$EDU^2

boxplot(POPULATION_SQ_MILE~CBD,data=bike)
summary(aov(POPULATION_SQ_MILE~CBD,data=bike))

bike$log_POPULATION_SQ_MILE = log(bike$POPULATION_SQ_MILE)
hist(bike$PER_CAPITA_INCOME)
scatter.smooth(bike$log_POPULATION_SQ_MILE,bike$trips)
qplot(CTA_BUS_STATIONS, trips, colour=as.factor(CBD), data = bike)
bike$log_CAPACITY <- log(bike$CAPACITY)

# create cbd index (cta bus station, cta train station, LBL, RFE, Income)
normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
bike[c(3,4,6,7,9)] <- apply(bike[c(3,4,6,7,9)], 2, normFunc)
bike$CBD_INDEX = (bike$Limited_Business_License+bike$Retail_Food_Establishment+
                    bike$PER_CAPITA_INCOME+bike$CTA_BUS_STATIONS+bike$CTA_TRAIN_STATIONS)/5

# cbd index 2 (cta bus station, cta train station, LBL, RFE, Income, capacity)
plot(bike_raw$CAPACITY,bike_raw$CBD)
summary(aov(CAPACITY~CBD, data = bike))
TukeyHSD(aov(CAPACITY~as.factor(CBD), data = bike))

bike %>% group_by(CBD) %>% summarise(
  n=n(),
  mean=mean(CAPACITY),
  min = min(CAPACITY),
  max = max(CAPACITY)
)

bike$CAPACITY2 = bike$CAPACITY
bike["CAPACITY2"] <- apply(bike["CAPACITY2"], 2, normFunc)
bike$CBD_INDEX2 = (bike$Limited_Business_License+bike$Retail_Food_Establishment+
                    bike$PER_CAPITA_INCOME+bike$CTA_BUS_STATIONS+bike$CTA_TRAIN_STATIONS+bike$CAPACITY2)/6

par(mfrow=c(1,1))
plot(bike$CBD_INDEX,bike$trips)
boxplot(trips~CBD,data=bike)

with(bike, interaction.plot(CBD_INDEX,CBD, trips, col=1:2))
interaction.plot(bike$CBD_INDEX,bike$CBD,bike$trips, col=1:2)

### crime exploration
# par(mfrow=c(1,1))
scatter.smooth(bike$THEFT,bike$trips)
cor(bike$THEFT,bike$trips)

scatter.smooth(bike$ROBBERY,bike$trips)
cor(bike$ROBBERY,bike$trips)

scatter.smooth(bike$NARCOTICS,bike$trips)
cor(bike$NARCOTICS,bike$trips)

scatter.smooth(bike$HOMICIDE,bike$trips)
cor(bike$HOMICIDE,bike$trips)

scatter.smooth(bike$DECEPTIVE_PRACTICE,bike$trips)
cor(bike$DECEPTIVE_PRACTICE,bike$trips)

scatter.smooth(bike$CRIMINAL_TRESPASS,bike$trips)
cor(bike$CRIMINAL_TRESPASS,bike$trips)

scatter.smooth(bike$BURGLARY,bike$trips)
cor(bike$BURGLARY,bike$trips)

scatter.smooth(bike$BATTERY,bike$trips)
cor(bike$BATTERY,bike$trips)

scatter.smooth(bike$ASSAULT,bike$trips)
cor(bike$ASSAULT,bike$trips)

lazy = lm(trips~THEFT+ROBBERY+NARCOTICS+HOMICIDE+DECEPTIVE_PRACTICE+CRIMINAL_TRESPASS+
            BURGLARY+BATTERY+ASSAULT,bike)
summary(lazy)
vif(lazy)

## transform
hist(bike$HOMICIDE)
hist(bike$BURGLARY)

bike$sqrtHOMICIDE = sqrt(bike$HOMICIDE)
bike$logBURGLARY = log(bike$BURGLARY)
bike$log_CAPACITY = log(bike$CAPACITY)
bike_crime <- bike %>% select(ID,THEFT,ROBBERY,NARCOTICS,sqrtHOMICIDE,DECEPTIVE_PRACTICE,
                              CRIMINAL_TRESPASS,logBURGLARY,BATTERY,ASSAULT)
bike_crime[,-1] <- data.frame(scale(bike_crime[,-1]))
bike_new <- bike %>% select(ID,trips,PARK_AREA_ACRES,BIKE_ROUTES,log_CAPACITY,log_POPULATION_SQ_MILE,
                            CBD,CBD_INDEX,CBD_INDEX2,MINORITY,sq_EDU)
bike_model <- full_join(bike_crime,bike_new)

bike_model$CrimePositive = (bike_model$CRIMINAL_TRESPASS+bike_model$ROBBERY+bike_model$THEFT+bike_model$DECEPTIVE_PRACTICE+
                              bike_model$BATTERY+bike_model$ASSAULT)/6
bike_model$CrimeNegative = (bike_model$sqrtHOMICIDE+bike_model$NARCOTICS+bike_model$logBURGLARY)/3
# bike_model$CrimeFlat = NULL

####################################################################################################
#                                            BEST MODEL                                            #
####################################################################################################
# bike$CBD_INDEX2 = (bike$Limited_Business_License+bike$Retail_Food_Establishment+bike$PER_CAPITA_INCOME+bike$CTA_BUS_STATIONS+bike$CTA_TRAIN_STATIONS+bike$CAPACITY2)/6
# CrimePositive = (bike_model$CRIMINAL_TRESPASS+bike_model$ROBBERY+bike_model$THEFT+bike_model$DECEPTIVE_PRACTICE+bike_model$BATTERY+bike_model$ASSAULT)/6
# CrimeNegative = (bike_model$sqrtHOMICIDE+bike_model$NARCOTICS+bike_model$logBURGLARY)/3
goodmodel <- lm(trips~PARK_AREA_ACRES+sqrt(BIKE_ROUTES)+CBD_INDEX2+MINORITY+sq_EDU+CrimePositive+CrimeNegative, bike_model)
summary(goodmodel)
vif(goodmodel)
yhat = predict(goodmodel, bike_model)
mean((bike_model$trips - yhat)^2)      # compute test set MSE 0.2630451

# ridge
x = model.matrix(trips~PARK_AREA_ACRES+sqrt(BIKE_ROUTES)+CBD_INDEX2+MINORITY+sq_EDU+CrimePositive+CrimeNegative, bike_model)
fit.ridge = glmnet(x, bike_model$trips, alpha=0)
plot(fit.ridge, xvar="lambda", label = TRUE)
fit.cv = cv.glmnet(x, bike_model$trips, alpha=0)
fit.cv$lambda.min        # optimal value of lambda 0.06696811
abline(v=log(fit.cv$lambda.min))
plot(fit.cv)
yhat = predict(fit.ridge, s=fit.cv$lambda.min, x)
mean((bike_model$trips - yhat)^2)      # compute test set MSE 0.2650768

# lasso
fit.lasso = glmnet(x, bike_model$trips, alpha=1)
plot(fit.lasso, xvar="lambda", label = TRUE)
fit.cv = cv.glmnet(x, bike_model$trips, alpha=1)
fit.cv$lambda.min        # optimal value of lambda 0.002093233
abline(v=log(fit.cv$lambda.min))
plot(fit.cv)
yhat = predict(fit.lasso, s=fit.cv$lambda.min, x)
mean((bike_model$trips - yhat)^2)      # compute test set MSE 0.2630863
