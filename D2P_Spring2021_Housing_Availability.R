library(faraway)
housing <- read.csv("C:/Users/adell/Desktop/american_community_survey_nbrhd_2013_2017.csv", header = TRUE,)
filthousing = subset(housing, select = c(NBHD_NAME, VACANT_HU, PCT_TWOORMORE_RACES, MEDIAN_AGE_ALL, TTL_HOUSING_UNITS, FAMILY_HOUSEHOLDS, MED_GROSS_RENT, MEDIAN_HOME_VALUE, PCT_FAM_POVERTY))   

rownames(filthousing) <- filthousing$NBHD_NAME                             
summary(filthousing)
filthousing$MEDIAN_HOME_VALUE = as.numeric(filthousing$MEDIAN_HOME_VALUE)
filthousing$MED_GROSS_RENT = as.numeric(filthousing$MED_GROSS_RENT)

filthousing  = na.action = na.exclude(filthousing)
#Initial Data Exploration
plot(density(filthousing$VACANT_HU))
plot(density(filthousing$PCT_TWOORMORE_RACES))
plot(density(filthousing$MEDIAN_AGE_ALL))
plot(density(filthousing$TTL_HOUSING_UNITS))
plot(density(filthousing$FAMILY_HOUSEHOLDS))
plot(density(filthousing$MED_GROSS_RENT))
plot(density(filthousing$MEDIAN_HOME_VALUE))
plot(density(filthousing$PCT_FAM_POVERTY))
#Make Transformations
filthousing$sqrt_PCT_FAM_POVERTY = sqrt(filthousing$PCT_FAM_POVERTY)
filthousing$sqrt_VACANT_HU = sqrt(filthousing$VACANT_HU)
filthousing$sqrt_MEDIAN_HOME_VALUE = sqrt(filthousing$MEDIAN_HOME_VALUE)
filthousing$sqrt_MED_GROSS_RENT = sqrt(filthousing$MED_GROSS_RENT)
filthousing$sqrt_FAMILY_HOUSEHOLDS = sqrt(filthousing$FAMILY_HOUSEHOLDS)
filthousing$sqrt_PCT_TWOORMORE_RACES = sqrt(filthousing$PCT_TWOORMORE_RACES)
filthousing$sqrt_TTL_HOUSING_UNITS = sqrt(filthousing$TTL_HOUSING_UNITS)
##Data exploration
library(car)
library(perturb)
pairs(~sqrt_VACANT_HU + sqrt_PCT_TWOORMORE_RACES + MEDIAN_AGE_ALL+ sqrt_TTL_HOUSING_UNITS+ sqrt_FAMILY_HOUSEHOLDS+ MED_GROSS_RENT+ sqrt_MEDIAN_HOME_VALUE+ sqrt_PCT_FAM_POVERTY, data = filthousing, col = "darkgreen", upper.panel=NULL, cex.labels = .5)
par(mar=c(1,1,1,1))
dens <- density(filthousing$sqrt_VACANT_HU)
plot(dens, frame = FALSE, col = "darkgreen", 
     main = "sqrt(Vacant Housing in Denver)") 
polygon(dens, col = "darkgreen")

#Assess for collinearity and amputate variables as needed
housinlmod <- lm(sqrt_VACANT_HU ~ sqrt_PCT_TWOORMORE_RACES + MEDIAN_AGE_ALL+ sqrt_TTL_HOUSING_UNITS+ sqrt_FAMILY_HOUSEHOLDS+ MED_GROSS_RENT+ sqrt_MEDIAN_HOME_VALUE+ sqrt_PCT_FAM_POVERTY, data = filthousing)
sumary(housinlmod)
x = model.matrix(housinlmod)
x = x[,-1] 
round(cor(x), 2)
vif(housinlmod)
colldiag(housinlmod, scale = TRUE, add.intercept = TRUE)

##remove sqrt_FAMILY_HOUSEHOLDS
housinlmod2 = update(housinlmod, .~. -sqrt_FAMILY_HOUSEHOLDS)
x = model.matrix(housinlmod2)
x = x[,-1] 
round(cor(x), 2)
vif(housinlmod2)
colldiag(housinlmod2, scale = TRUE, add.intercept = TRUE)


##remove sqrt_PCT_FAM_POVERTY
housinlmod3 = update(housinlmod2, .~. -sqrt_PCT_FAM_POVERTY)
x = model.matrix(housinlmod3)
x = x[,-1] 
round(cor(x), 2)
vif(housinlmod3)
colldiag(housinlmod3, scale = TRUE, add.intercept = TRUE)


##remove MEDIAN_AGE_ALL
housinlmod4 = update(housinlmod3, .~. -MEDIAN_AGE_ALL)
x = model.matrix(housinlmod4)
x = x[,-1]
round(cor(x), 2)
vif(housinlmod4)
colldiag(housinlmod4, scale = TRUE, add.intercept = TRUE)





####Variable selection
# use alpha_crit = 0.05
sumary(housinlmod4)
lmod2 <- update(housinlmod4, . ~ . -sqrt_PCT_TWOORMORE_RACES)
sumary(lmod2)
lmod3 <- update(lmod2, . ~ . - MED_GROSS_RENT)
sumary(lmod3)



# model selection in terms of AIC
library(leaps)
b <- regsubsets(sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + MED_GROSS_RENT + sqrt_MEDIAN_HOME_VALUE, data = filthousing)
rs <- summary(b) 
rs$which
plot(b)
n = nobs(lmod2)
AIC <- n*log(rs$rss/n) + (2:4)*2
plot(AIC ~ I(1:3), ylab="AIC", xlab="Number of Predictors", col = "blue", cex = 3, pch = 16)
# Construct C_p plot 

# Construct adjusted R^2 plot
which.max(rs$adjr2)
subsets(b, statistic = "adjr2", legend = FALSE, col = "blue")


## compare with 3 vs with 2 regressors:
library(caret)
f1 = sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + MED_GROSS_RENT + sqrt_MEDIAN_HOME_VALUE
f2 = sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + sqrt_MEDIAN_HOME_VALUE
cv_5fold = trainControl(method = "cv", number = 5) # 5-fold crossvalidation train/test data
modela = train(f1, data = filthousing, trControl = cv_5fold,
               method = "lm")
modelb = train(f2, data = filthousing, trControl = cv_5fold,
               method = "lm")
# compare mse (rmse) for the three models using 5-fold cv
print(modela) # p = 3
print(modelb) # p = 2

###Model with three regressors performs better.

housinglmod <- lm(sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + MED_GROSS_RENT + sqrt_MEDIAN_HOME_VALUE, data = filthousing)
sumary(housinglmod)
#plot yhat vs y
plot(predict(housinglmod),filthousing$VACANT_HU, 
     xlab="yhat",ylab="y", col = "blue")
abline(l <- lm(filthousing$VACANT_HU~ predict(housinglmod)), col = "red")

##investigating structure
par(mar=c(1,1,1,1))
residualPlots(housinglmod)
marginalModelPlots(housinglmod)
avPlots(housinglmod, id = FALSE)
crPlot(housinglmod)


##more investigating?
h <- hatvalues(housinglmod)
neighborhoods <- filthousing$NBHD_NAME
halfnorm(h, nlab = 2, labs = neighborhoods, ylab = "leverage")
infIndexPlot(housinglmod, vars = "hat")

outlierTest(housinglmod)
infIndexPlot(housinglmod, c(vars = "Studentized", "Bonf"))
cook <- cooks.distance(housinglmod)
halfnorm(cook, n=1, labs = neighborhoods, ylab = "Cook's Distances", col = "blue")
lmod2 <- lm(sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + MED_GROSS_RENT + sqrt_MEDIAN_HOME_VALUE, data = filthousing, subset = (neighborhoods != "Union Station"))
compareCoefs(housinglmod, lmod2)
dfbetasPlots(housinglmod, id.n = 1, lab = neighborhoods)
influencePlot(housinglmod)
plot(housinglmod, which = 4)
plot(housinglmod, col = "blue")

residualPlot(housinglmod)
plot(housinglmod, which = 1)
residualPlots(housinglmod, quadratic = FALSE, fitted = FALSE, tests = FALSE)
qqPlot(housinglmod)




plot(residuals(housinglmod) ~ filthousing$MED_YR_STRUCTURE_BUILT, 
     ylab = "residuals")
abline(h = 0)

# plot successive pairs of residuals
n = nobs(housinglmod)
plot(tail(residuals(housinglmod), n - 1) ~ head(residuals(housinglmod), n - 1), 
     xlab = expression(hat(epsilon)[i]), 
     ylab = expression(hat(epsilon)[i + 1]))
abline(h = 0, v = 0, col = grey(0.75))

shapiro.test(residuals(housinglmod))


library(lmtest)
dwtest(sqrt_VACANT_HU ~ sqrt_TTL_HOUSING_UNITS + MED_GROSS_RENT + sqrt_MEDIAN_HOME_VALUE, data = filthousing)

