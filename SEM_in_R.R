#install.packages("lavaan", repos="http://www.da.ugent.be", type="source")
install.packages("lavaan.survey")
install.packages("survey")
library(lavaan)
library(lavaan.survey)
library(survey)
library(psych)
library(GPArotation)

normalize <- function(x) {
  x <- sweep(x, 2, apply(x, 2, min))
  sweep(x, 2, apply(x, 2, max), "/")
}

myData <- read.table("~/Dropbox (MIT)/ZhanZhao_Jinhua/4. Car Pride/Analysis/Revolution/Shanghai_Data_new(v0).dat", header=TRUE)
pride1 <- 6 - myData$pride1
pride2 <- 6 - myData$pride2
pride3 <- 6 - myData$pride3
image1 <- 6 - myData$image1
image2 <- 6 - myData$image2
image3 <- 6 - myData$image3

ID <- myData$Id
weight <- myData$weight
age <- myData$age
youth <- as.numeric(myData$age < 35)
senior <- as.numeric(myData$age >59)
carownership <- as.numeric(myData$carownership > 1)
education <- as.numeric(myData$education > 2)
location <- myData$location
inner_city <- as.numeric(myData$location == 1)
outer_city <- as.numeric(myData$location == 3)
male <- as.numeric(myData$gender == 1)
local <- as.numeric(myData$residence == 1)
employment <- as.numeric(myData$employment == 1)
income <- myData$income
low_income <- as.numeric(myData$income < 5)
high_income <- as.numeric(myData$income > 7)
living_time <- as.numeric(myData$living_time > 3)
subway_walk <- as.numeric(myData$sub_walk < 3)
bus_walk <- as.numeric(myData$bus_walk < 3)
pt_access <- myData$pt_live
commute_dist <- myData$commute_dist
short_dist <- as.numeric(myData$commute_dist < 4)
long_dist <- as.numeric(myData$commute_dist > 5)

olsreg1 <- glm(carownership ~ youth + senior + male + education + inner_city + outer_city + local + employment + 
                 low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, 
               #weights = weight, 
               family = "binomial")

carownership_fitted <- fitted(olsreg1)
carownership_imputed <- round(carownership_fitted, digits = 0)

prideData <- data.frame(pride1, pride2, pride3, image2, image3, 
                        youth, senior, male, education, inner_city, outer_city, local, employment, 
                        low_income, high_income, #carownership_fitted, carownership_imputed,
                        weight, ID)

surveyDesign <- svydesign(id=~ID, weights=~weight, data=prideData)


cfa1 <- "
    ##measurement model
    pride = ~ pride1 + pride2 + pride3 + image2 + image3
    "; 
cfa1.fit <- cfa(cfa1, prideData, estimator="MLR", mimic="Mplus", std.lv=TRUE)
summary(cfa1.fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

cfa1.wtfit <- lavaan.survey(cfa1.fit, surveyDesign, estimator="ML")
summary(cfa1.wtfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

cfa2 <- "
    ##measurement model
    personal =~ pride1 + pride2 + pride3
    social =~ image2 + image3
    "; 
cfa2.fit <- cfa(cfa2, prideData, estimator="MLR", mimic="Mplus", std.lv = TRUE)
summary(cfa2.fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

cfa2.wtfit <- lavaan.survey(cfa2.fit, surveyDesign, estimator="ML")
summary(cfa2.wtfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fa.parallel(prideData)
efa1.fit <- fa(prideData, nfactors = 1, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")
fa.diagram(efa1.fit)
efa2.fit <- fa(prideData, nfactors = 2, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")
fa.diagram(efa2.fit)
efa3.fit <- fa(prideData, nfactors = 3, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")
fa.diagram(efa3.fit)



#SEM+2SLS

sls1 <- "
  ##measurement model
  pride = ~ pride1 + pride2 + pride3 + image2 + image3
  ##regression
  pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership_imputed
"; 
sls1.fit <- sem(sls1, prideData, estimator="MLR", mimic="Mplus", std.lv = TRUE)
summary(sls1.fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

sls1.wtfit <- lavaan.survey(sls1.fit, surveyDesign, estimator="ML")
summary(sls1.wtfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

sls2 <- "
  ##measurement model
  personal =~ pride1 + pride2 + pride3
  social =~ image2 + image3
  ##regression
  personal ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership_imputed
  social ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership_imputed
"; 
sls2.fit <- sem(sls2, slsData, estimator="MLR", mimic="Mplus", std.lv = TRUE)
summary(sls2.fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)

sls2.wtfit <- lavaan.survey(sls2.fit, surveyDesign, estimator="ML")
summary(sls2.wtfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#### cfa scores distribution
scores <- cbind(predict(cfa1.fit), predict(cfa2.fit))
scores <- normalize(scores)
scores_co <- subset(scores, myData$carownership > 1)

carowners <- subset(myData, myData$carownership > 1)
local_lic <- as.numeric(carowners$car_license < 4)

t.test(scores_co[,1]~local_lic)
t.test(scores_co[,2]~local_lic)
t.test(scores_co[,3]~local_lic)

myData2 <- read.csv("~/Dropbox (MIT)/Backup/MPlus/Shanghai_Actual_CarType.csv", header=TRUE)
class <- as.character(nrow(myData2))
for (i in 1:nrow(myData2)) {
  if (myData2$car_class2[i] == 1) {
    class[i] = "Small Car"
  }
  else if (myData2$car_class2[i] == 2) {
    class[i] = "Medium Car"
  }
  else if (myData2$car_class2[i] == 3) {
    class[i] = "Large Car"
  }
  else if (myData2$car_class2[i] == 4) {
    class[i] = "Luxury Car"
  }
  else {
    class[i] = "MPV/CUV/SUV"
  }
}
class = factor(class)
class = factor(class, levels = c("Small Car", "Medium Car", "Large Car", "Luxury Car", "MPV/CUV/SUV"))

pride1 <- 6 - myData2$Q67_1
pride2 <- 6 - myData2$Q67_2
pride3 <- 6 - myData2$Q67_3
image1 <- 6 - myData2$Q73_1
image2 <- 6 - myData2$Q73_2
image3 <- 6 - myData2$Q73_3
prideData <- data.frame(pride1, pride2, pride3, image2, image3)
cfa1 <- "
    ##measurement model
    pride = ~ pride1 + pride2 + pride3 + image2 + image3
    "; 
cfa1.fit2 <- cfa(cfa1, prideData, estimator="MLR", mimic="Mplus", std.lv=TRUE)

cfa2 <- "
    ##measurement model
    personal =~ pride1 + pride2 + pride3
    social =~ image2 + image3
    "; 
cfa2.fit2 <- cfa(cfa2, prideData, estimator="MLR", mimic="Mplus", std.lv = TRUE)
scores2 <- cbind(predict(cfa1.fit2), predict(cfa2.fit2))
scores2 <- normalize(scores2)
aov1 <- aov(scores2[,1]~class)
aov2 <- aov(scores2[,2]~class)
aov3 <- aov(scores2[,3]~class)
aggregate(scores2~class, FUN=mean)
