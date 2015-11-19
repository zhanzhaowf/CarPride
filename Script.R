library(psych)
library(GPArotation)
library(rattle)


# Factor analysis using binary data

reg_pride1 <- lm(myData$pride1 ~ myData$pride2 + myData$pride3 + myData$pride4 + myData$pride5 + myData$telling + myData$washing + myData$accident + myData$auto_show + myData$superiority + myData$spending + myData$cleanliness + myData$display_parking + myData$display_driving + myData$picture + myData$happiness + myData$self_extension + myData$must + myData$swagger + myData$other + myData$image1 + myData$image2 + myData$image3 + myData$image4)

myData <- read.table("Shanghai_Data_new.dat", header = TRUE)
#myData <- read.table("~/Dropbox (MIT)/ZhanZhao_Jinhua/4. Car Pride/Analysis/Revolution/Shanghai_Data_new(v0).dat")

pride1 <- 2 - myData$pride1_binary
pride2 <- 2 - myData$pride2_binary
pride3 <- 2 - myData$pride3_binary

image1 <- 2 - myData$image1_binary
image2 <- 2 - myData$image2_binary
image3 <- 2 - myData$image3_binary

auto_show <- 2 - myData$auto_show
superiority <- 2- myData$superiority
spending <- 2- myData$spending
cleanliness <- 2- myData$cleanliness
display_parking <- 2 - myData$display_parking
display_driving <- 2 - myData$display_driving
picture <- 2- myData$picture
happiness <- 2 - myData$happiness
self_extension <- 2 - myData$self_extension
must <- 2 - myData$must
swagger <- 2 - myData$swagger
other <- 2 - myData$other


dataAllFa <- data.frame(pride1, pride2, pride3, image1, image2, image3, auto_show, superiority, spending, cleanliness, display_parking, display_driving, picture, happiness, self_extension, must, swagger, other)
resultAllFa5 <- fa(dataAllFa, nfactors = 5, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")

dataAllFa <- data.frame(pride1, pride2, pride3, image1, image2, image3, superiority, spending, display_parking, display_driving, happiness, self_extension, must, swagger, other)
resultAllFa5 <- fa(dataAllFa, nfactors = 5, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")

dataAllFa <- data.frame(image1, image2, image3, superiority, self_extension)
resultAllFa3 <- fa(dataAllFa, nfactors = 3, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")
resultAllFa3

fa.parallel(dataAllFa)
fa.diagram(resultAllFa5)


# Modeling car pride using binary data

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

logit_pride1 <- glm(pride1 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_pride2 <- glm(pride2 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_pride3 <- glm(pride3 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_image1 <- glm(image1 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_image2 <- glm(image2 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_image3 <- glm(image3 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_self_extension <- glm(self_extension ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_superiority <- glm(superiority ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_swagger <- glm(swagger ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_other <- glm(other ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_happiness <- glm(happiness ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_spending <- glm(spending ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_must <- glm(must ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_display_parking <- glm(display_parking ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_display_driving <- glm(display_driving ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_auto_show <- glm(auto_show ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_cleanliness <- glm(cleanliness ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
logit_picture <- glm(picture ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")

summary(logit_pride1)


# Modeling car pride using original data (with multiple indicators)

pride1 <- 6 - myData$pride1
pride2 <- 6 - myData$pride2
pride3 <- 6 - myData$pride3
image1 <- 6 - myData$image1
image2 <- 6 - myData$image2
image3 <- 6 - myData$image3

lm_pride1 <- lm(pride1 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_pride2 <- lm(pride2 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_pride3 <- lm(pride3 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_image1 <- lm(image1 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_image2 <- lm(image2 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_image3 <- lm(image3 ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)

# Modeling car pride using original data (with one integrated indicator)

pride1 <- (5 - myData$pride1)/4
pride2 <- (5 - myData$pride2)/4
pride3 <- (5 - myData$pride3)/4
image1 <- (5 - myData$image1)/4
image2 <- (5 - myData$image2)/4
image3 <- (5 - myData$image3)/4

auto_show <- 2 - myData$auto_show
superiority <- 2- myData$superiority
spending <- 2- myData$spending
cleanliness <- 2- myData$cleanliness
display_parking <- 2 - myData$display_parking
display_driving <- 2 - myData$display_driving
picture <- 2- myData$picture
happiness <- 2 - myData$happiness
self_extension <- 2 - myData$self_extension
must <- 2 - myData$must
swagger <- 2 - myData$swagger
other <- 2 - myData$other

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3 + superiority + display_parking + display_driving + happiness + self_extension + swagger + other)/13

lm_pride <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)


# Cluster Analysis

dataAllCa <- data.frame(pride1, pride2, pride3, image1, image2, image3, auto_show, superiority, spending, cleanliness, display_parking, display_driving, picture, happiness, self_extension, must, swagger, other)
d <- dist(dataAllCa, method = "euclidean")
fit <- hclust(d, method = "ward")
plot(fit)

groups <- cutree(fit, k=2)

table(groups, pride2)
table(groups, self_extension)
table(groups, image1)
table(groups, image2)
table(groups, image3)
table(groups, superiority)
table(groups, pride3)
table(groups, happiness)
table(groups, pride1)
table(groups, swagger)
table(groups, other)
table(groups, spending)
table(groups, must)
table(groups, display_parking)
table(groups, display_driving)
table(groups, auto_show)
table(groups, cleanliness)
table(groups, picture)

table(groups)
table(groups, youth)
table(groups, senior)
table(groups, male)
table(groups,education)
table(groups, inner_city)
table(groups, outer_city)
table(groups, local)
table(groups, employment)
table(groups, low_income)
table(groups, high_income)
table(groups, subway_walk)
table(groups, bus_walk)
table(groups, short_dist)
table(groups, long_dist)
table(groups, carownership)

write.table(groups, "groups.txt", sep="\t")

# Pride and Car Ownership

myData2 <- subset(myData, myData$carownership > 1)

pride1 <- (5 - myData2$pride1)/4
pride2 <- (5 - myData2$pride2)/4
pride3 <- (5 - myData2$pride3)/4
image1 <- (5 - myData2$image1)/4
image2 <- (5 - myData2$image2)/4
image3 <- (5 - myData2$image3)/4

auto_show <- 2 - myData2$auto_show
superiority <- 2- myData2$superiority
spending <- 2- myData2$spending
cleanliness <- 2- myData2$cleanliness
display_parking <- 2 - myData2$display_parking
display_driving <- 2 - myData2$display_driving
picture <- 2- myData2$picture
happiness <- 2 - myData2$happiness
self_extension <- 2 - myData2$self_extension
must <- 2 - myData2$must
swagger <- 2 - myData2$swagger
other <- 2 - myData2$other

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3 + superiority + display_parking + display_driving + happiness + self_extension + swagger + other)/13

mode_car <- as.numeric(myData2$commute_mode < 3)
transit <- as.numeric(myData2$commute_mode == 3)
non_motor <- as.numeric(myData2$commute_mode == 4 | myData$commute_mode == 5)
mode_other <- as.numeric(myData2$commute_mode > 5)
drive_freq <- 9 - myData2$drive_freq
passenger_freq <- 9- myData2$passenger_freq
car_time <- 2012 - myData2$car_year
car_price <- myData2$car_price/1000
car_license <- as.numeric(myData2$car_license < 4)
car_vkt <- myData2$car_vkt
drive_time <- myData2$drive_time
ncars <- myData2$carownership

lic_respect <- (5 - myData2$non_local1)/4
lic_status <- (5 - myData2$non_local2)/4
lic_image <- (5 - myData2$non_local3)/4
lic_judging <- (5 - myData2$non_local4)/4
lic_looking <- (5 - myData2$non_local5)/4
lic_identity <- (5 - myData2$non_local6)/4
lic_pride <- (5 - myData2$non_local7)/4

license_pride <- (lic_respect + lic_status + lic_judging +lic_looking + lic_identity + lic_pride)/6

car_data <- data.frame(pride, license_pride, car_time, car_price, car_license, drive_time)


# Pride and Car Use

myData <- read.table("Shanghai_Data_new.dat", header = TRUE)
myData2 <- subset(myData, myData$carownership > 1)

pride1 <- (5 - myData2$pride1)/4
pride2 <- (5 - myData2$pride2)/4
pride3 <- (5 - myData2$pride3)/4
image1 <- (5 - myData2$image1)/4
image2 <- (5 - myData2$image2)/4
image3 <- (5 - myData2$image3)/4

auto_show <- 2 - myData2$auto_show
superiority <- 2- myData2$superiority
spending <- 2- myData2$spending
cleanliness <- 2- myData2$cleanliness
display_parking <- 2 - myData2$display_parking
display_driving <- 2 - myData2$display_driving
picture <- 2- myData2$picture
happiness <- 2 - myData2$happiness
self_extension <- 2 - myData2$self_extension
must <- 2 - myData2$must
swagger <- 2 - myData2$swagger
other <- 2 - myData2$other

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3 + superiority + display_parking + display_driving + happiness + self_extension + swagger + other)/13

mode_car <- as.numeric(myData2$commute_mode < 3)
transit <- as.numeric(myData2$commute_mode == 3)
non_motor <- as.numeric(myData2$commute_mode == 4 | myData2$commute_mode == 5)
mode_other <- as.numeric(myData2$commute_mode > 5)
drive_freq <- 9 - myData2$drive_freq
passenger_freq <- 9- myData2$passenger_freq
car_time <- 2012 - myData2$car_year
car_price <- myData2$car_price/1000
car_license <- as.numeric(myData2$car_license < 4)
car_vkt <- myData2$car_vkt
drive_time <- myData2$drive_time
ncars <- myData2$carownership

daily_use <- as.numeric(myData2$drive_freq < 3 | myData2$passenger_freq < 3)
long_vkt <- as.numeric(myData2$car_vkt < 5)
driver <- as.numeric(myData2$drive_time > 0)

weight <- myData2$weight
age <- myData2$age
youth <- as.numeric(myData2$age < 35)
senior <- as.numeric(myData2$age >59)
carownership <- as.numeric(myData2$carownership > 1)
education <- as.numeric(myData2$education > 2)
location <- myData2$location
inner_city <- as.numeric(myData2$location == 1)
outer_city <- as.numeric(myData2$location == 3)
male <- as.numeric(myData2$gender == 1)
local <- as.numeric(myData2$residence == 1)
employment <- as.numeric(myData2$employment == 1)
income <- myData2$income
low_income <- as.numeric(myData2$income < 5)
high_income <- as.numeric(myData2$income > 7)
living_time <- as.numeric(myData2$living_time > 3)
subway_walk <- as.numeric(myData2$sub_walk < 3)
bus_walk <- as.numeric(myData2$bus_walk < 3)
pt_access <- myData2$pt_live
commute_dist <- myData2$commute_dist
short_dist <- as.numeric(myData2$commute_dist < 4)
long_dist <- as.numeric(myData2$commute_dist > 5)

subway_dist <- as.numeric(myData2$sub_dist < 3)
bus_dist <- as.numeric(myData2$bus_dist < 3)

logit_mode_car <- glm(mode_car ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")
logit_daily_use <- glm(daily_use ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")
lm_car_vkt <- lm(car_vkt ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight)

weekly_bus <- as.numeric(myData2$bus_freq <= 4)
weekly_subway <- as.numeric(myData2$subway_freq <= 4)
weekly_bike <- as.numeric(myData2$bike_freq <= 4)
weekly_walk <- as.numeric(myData2$walk_freq <= 4)

logit_bus <- glm(weekly_bus ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")
logit_subway <- glm(weekly_subway ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")
logit_bike <- glm(weekly_bike ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")
logit_walk <- glm(weekly_walk ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")

myData2 <- subset(myData2, myData2$car_share >= 0)

car_share <- myData2$car_share

lm_car_share <- lm(car_share ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight)


# Effect of Car Use on Car Pride

logit_mode_car <- glm(mode_car ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
mode_car_fitted <- fitted(logit_mode_car)
mode_car_imputed <- round(mode_car_fitted, digits = 0)
lm_pride1 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + mode_car_imputed, weights = weight)

logit_daily_use <- glm(daily_use ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")
daily_use_fitted <- fitted(logit_daily_use)
daily_use_imputed <- round(daily_use_fitted, digits = 0)
lm_pride2 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + daily_use_imputed, weights = weight)

lm_car_vkt <- lm(car_vkt ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
car_vkt_fitted <- fitted(lm_car_vkt)
lm_pride3 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + car_vkt_fitted, weights = weight)

lm_car_share <- lm(car_share ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
car_share_fitted <- fitted(lm_car_share)
lm_pride4 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + car_share_fitted, weights = weight)


# Pride and Car License

lic_respect <- (5 - myData$non_local1)/4
lic_status <- (5 - myData$non_local2)/4
lic_image <- (5 - myData$non_local3)/4
lic_judging <- (5 - myData$non_local4)/4
lic_looking <- (5 - myData$non_local5)/4
lic_identity <- (5 - myData$non_local6)/4
lic_pride <- (5 - myData$non_local7)/4

license_pride <- (lic_respect + lic_status + lic_judging +lic_looking + lic_identity + lic_pride)/6


lic_respect <- (5 - myData2$non_local1)/4
lic_status <- (5 - myData2$non_local2)/4
lic_image <- (5 - myData2$non_local3)/4
lic_judging <- (5 - myData2$non_local4)/4
lic_looking <- (5 - myData2$non_local5)/4
lic_identity <- (5 - myData2$non_local6)/4
lic_pride <- (5 - myData2$non_local7)/4

license_pride <- (lic_respect + lic_status + lic_judging +lic_looking + lic_identity + lic_pride)/6


# for presentation

myData <- read.table("Shanghai_Data_new.dat", header = TRUE)


pride1 <- 2 - myData$pride1_binary
self_esteem <- 2 - myData$pride2_binary
achievement <- 2 - myData$pride3_binary

image_fit <- 2 - myData$image1_binary
status <- 2 - myData$image2_binary
social_image <- 2 - myData$image3_binary

auto_show <- 2 - myData$auto_show
superiority <- 2- myData$superiority
spending <- 2- myData$spending
cleanliness <- 2- myData$cleanliness
display_parking <- 2 - myData$display_parking
display_driving <- 2 - myData$display_driving
picture <- 2- myData$picture
happiness <- 2 - myData$happiness
self_extension <- 2 - myData$self_extension
must <- 2 - myData$must
pride2 <- 2 - myData$swagger
other <- 2 - myData$other


dataAllFa <- data.frame(pride1, self_esteem, achievement, image_fit, status, social_image, auto_show, superiority, spending, cleanliness, display_parking, display_driving, picture, happiness, self_extension, must, pride2, other)
resultAllFa5 <- fa(dataAllFa, nfactors = 5, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")

dataAllFa <- data.frame(pride1, self_esteem, achievement, image_fit, status, social_image, superiority, display_parking, display_driving, happiness, self_extension, pride2, other)
resultAllFa5 <- fa(dataAllFa, nfactors = 5, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")

dataAllFa <- data.frame(self_esteem, image_fit, status, social_image, superiority, self_extension, display_parking, display_driving)
resultAllFa4 <- fa(dataAllFa, nfactors = 4, scores = "Bartlett", rotate = "oblimin", max.iter=1000, fm = "pa")
resultAllFa4

fa.parallel(dataAllFa)
fa.diagram(resultAllFa5)


# Correlation Matrix
dataAllFa <- data.frame(pride2, self_extension, image1, image2, image3, display_parking, display_driving, superiority, pride3, happiness, pride1, swagger, other, spending, must, auto_show, cleanliness, picture)


# Effect of car ownership on car pride (using instrumental variables)

pride1 <- (5 - myData$pride1)/4
pride2 <- (5 - myData$pride2)/4
pride3 <- (5 - myData$pride3)/4
image1 <- (5 - myData$image1)/4
image2 <- (5 - myData$image2)/4
image3 <- (5 - myData$image3)/4

auto_show <- 2 - myData$auto_show
superiority <- 2- myData$superiority
spending <- 2- myData$spending
cleanliness <- 2- myData$cleanliness
display_parking <- 2 - myData$display_parking
display_driving <- 2 - myData$display_driving
picture <- 2- myData$picture
happiness <- 2 - myData$happiness
self_extension <- 2 - myData$self_extension
must <- 2 - myData$must
swagger <- 2 - myData$swagger
other <- 2 - myData$other

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3 + superiority + display_parking + display_driving + happiness + self_extension + swagger + other)/13

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

olsreg <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership, weights = weight)

olsreg1 <- glm(carownership ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight, family = "binomial")

carownership_fitted <- fitted(olsreg1)
carownership_imputed <- round(carownership_fitted, digits = 0)

olsreg2 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership_imputed, weights = weight)


# Effect of car pride on car purchase plan (for noncar owners only)

myData <- read.table("Shanghai_Data_new.dat", header = TRUE)
myData3 <- subset(myData, myData$carownership == 1)
pride <- myData3$pride
car_plan <- as.numeric(myData3$car_plan < 3)

weight <- myData3$weight
youth <- as.numeric(myData3$age < 35)
senior <- as.numeric(myData3$age >59)
education <- as.numeric(myData3$education > 2)
inner_city <- as.numeric(myData3$location == 1)
outer_city <- as.numeric(myData3$location == 3)
male <- as.numeric(myData3$gender == 1)
local <- as.numeric(myData3$residence == 1)
employment <- as.numeric(myData3$employment == 1)
low_income <- as.numeric(myData3$income < 5)
high_income <- as.numeric(myData3$income > 7)
subway_walk <- as.numeric(myData3$sub_walk < 3)
bus_walk <- as.numeric(myData3$bus_walk < 3)
short_dist <- as.numeric(myData3$commute_dist < 4)
long_dist <- as.numeric(myData3$commute_dist > 5)

olsreg <- glm(car_plan ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + pride, weights = weight, family = "binomial")


# Policy Acceptance

acceptance <- myData$acceptance
effectiveness <- myData$effectiveness
affordability <- myData$affordability
equity <- myData$equity
implementation <- myData$implementation
lm_acceptance <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + pride, weights = weight)
lm_effectiveness <- lm(effectiveness ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + pride, weights = weight)
lm_affordability <- lm(affordability ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + pride, weights = weight)
lm_equity <- lm(equity ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + pride, weights = weight)
lm_implementation <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + pride, weights = weight)

nc_pride <- pride * as.numeric(carownership == 0)
co_pride <- pride * as.numeric(carownership == 1)
lc_pride <- pride * as.numeric(carownership == 1 & myData$car_license < 4)
nl_pride <- pride * as.numeric(carownership == 1 & myData$car_license >= 4)

lm_acceptance <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + nc_pride + lc_pride + nl_pride, weights = weight)
lm_acceptance <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + carownership + nc_pride + co_pride, weights = weight)


# Estimatation the impact of car license pride on intended car license choice

license_choice <- as.numeric(myData$license_choice == 1)
lm_license_choice <- lm(license_choice ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + license_pride, weights = weight)
summary(lm_license_choice)


# Car Dependence

depend <- (5 - myData2$dependence1)/4
alternative <- (5 - myData2$dependence2)/4
lifestyle <- (5 - myData2$dependence3)/4
habit <- (5 - myData2$dependence6)/4
dependence <- (depend + alternative + lifestyle + habit)/4

cor(data.frame(depend, alternative, lifestyle, habit))

lm_depend <- lm(depend ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_alternative <- lm(alternative ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_lifestyle <- lm(lifestyle ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_habit <- lm(habit ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)
lm_dependence <- lm(dependence ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist, weights = weight)

lm_dependence <- lm(dependence ~ youth + senior + male + education + inner_city + outer_city + local + employment + low_income + high_income + subway_walk + bus_walk + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight)

dataAllCa <- data.frame(pride, dependence)
dataAllCa <- data.frame(pride1, pride2, pride3, image1, image2, image3, auto_show, superiority, spending, cleanliness, display_parking, display_driving, picture, happiness, self_extension, must, swagger, other, depend, alternative, lifestyle, habit)

d <- dist(dataAllCa, method = "euclidean")
fit <- hclust(d, method = "ward")
plot(fit)

groups <- cutree(fit, k=4)
describe.by(dataAllCa, groups)



# Comparison between Shanghai and Beijing

# Shanghai All

myData <- read.table("Shanghai_Data_new.dat", header = TRUE)

pride1 <- (5 - myData$pride1)/4
pride2 <- (5 - myData$pride2)/4
pride3 <- (5 - myData$pride3)/4
image1 <- (5 - myData$image1)/4
image2 <- (5 - myData$image2)/4
image3 <- (5 - myData$image3)/4

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3)/6

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
income <- myData$income
low_income <- as.numeric(myData$income < 5)
high_income <- as.numeric(myData$income > 7)
living_time <- as.numeric(myData$living_time > 3)
subway_dist <- as.numeric(myData$sub_dist < 3)
bus_dist <- as.numeric(myData$bus_dist < 3)
commute_dist <- myData$commute_dist
short_dist <- as.numeric(myData$commute_dist < 4)
long_dist <- as.numeric(myData$commute_dist > 5)

lm_pride <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income, weights = weight)

olsreg1 <- glm(carownership ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + subway_dist + bus_dist + short_dist + long_dist, weights = weight, family = "binomial")
carownership_fitted <- fitted(olsreg1)
carownership_imputed <- round(carownership_fitted, digits = 0)
olsreg2 <- lm(pride ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + carownership_imputed, weights = weight)

acceptance <- myData$acceptance
lm_acceptance <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + carownership + pride, weights = weight)


# Shanghai car owners

myData2 <- subset(myData, myData$carownership > 1)
myData2 <- subset(myData2, myData2$car_year >= 0)
myData2 <- subset(myData2, myData2$car_price >= 0)

pride1 <- (5 - myData2$pride1)/4
pride2 <- (5 - myData2$pride2)/4
pride3 <- (5 - myData2$pride3)/4
image1 <- (5 - myData2$image1)/4
image2 <- (5 - myData2$image2)/4
image3 <- (5 - myData2$image3)/4

pride <- (pride1 + pride2 + pride3 + image1 + image2 + image3)/6

car_license <- as.numeric(myData2$car_license < 4)
drive_freq <- 9 - myData2$drive_freq
passenger_freq <- 9- myData2$passenger_freq
car_time <- 2012 - myData2$car_year
car_price <- myData2$car_price/1000
car_vkt <- myData2$car_vkt
drive_time <- myData2$drive_time
ncars <- myData2$carownership
car_share <- myData2$car_share
daily_use <- as.numeric(myData2$drive_freq < 3 | myData2$passenger_freq < 3)
long_vkt <- as.numeric(myData2$car_vkt < 5)
driver <- as.numeric(myData2$drive_time > 0)

weight <- myData2$weight
age <- myData2$age
youth <- as.numeric(myData2$age < 35)
senior <- as.numeric(myData2$age >59)
carownership <- as.numeric(myData2$carownership > 1)
education <- as.numeric(myData2$education > 2)
location <- myData2$location
inner_city <- as.numeric(myData2$location == 1)
outer_city <- as.numeric(myData2$location == 3)
male <- as.numeric(myData2$gender == 1)
local <- as.numeric(myData2$residence == 1)
income <- myData2$income
low_income <- as.numeric(myData2$income < 5)
high_income <- as.numeric(myData2$income > 7)
living_time <- as.numeric(myData2$living_time > 3)
subway_dist <- as.numeric(myData2$sub_dist < 3)
bus_dist <- as.numeric(myData2$bus_dist < 3)
commute_dist <- myData2$commute_dist
short_dist <- as.numeric(myData2$commute_dist < 4)
long_dist <- as.numeric(myData2$commute_dist > 5)

logit_daily_use <- glm(daily_use ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + subway_dist + bus_dist + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight, family = "binomial")

myData2 <- subset(myData2, myData2$car_vkt >= 0)
lm_car_vkt <- lm(car_vkt ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + subway_dist + bus_dist + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight)

myData2 <- subset(myData2, myData2$car_share >= 0)
lm_car_share <- lm(car_share ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + subway_dist + bus_dist + short_dist + long_dist + driver + car_time + car_price + car_license + pride, weights = weight)

# Beijing All

myData <- read.table("Beijing_Data.dat", header = TRUE)

acceptance <- ((myData$accept_1p + myData$accept_2p - myData$accept_3n - myData$accept_4n - myData$accept_5n +18)/5 - 1)/4
lm_acceptance <- lm(acceptance ~ youth + senior + male + education + inner_city + outer_city + local + low_income + high_income + carownership + pride, weights = weight)

# Beijing Car Owners

car_license <- as.numeric(myData2$car_license <= 4)


# Testing significance of the difference
t.test((5-myData2$pride1)/4,(5-myData3$pride1)/4)
t.test((5-myData2$pride2)/4,(5-myData3$pride2)/4)
t.test((5-myData2$pride3)/4,(5-myData3$pride3)/4)
t.test((5-myData2$image1)/4,(5-myData3$image1)/4)
t.test((5-myData2$image2)/4,(5-myData3$image2)/4)

library(SDMTools)
wt.sd((5-myData$pride1)/4,myData$weight)
wt.sd((5-myData2$pride1)/4,myData2$weight)
wt.sd((5-myData3$pride1)/4,myData3$weight)

wt.sd((5-myData$pride2)/4,myData$weight)
wt.sd((5-myData2$pride2)/4,myData2$weight)
wt.sd((5-myData3$pride2)/4,myData3$weight)

wt.sd((5-myData$pride3)/4,myData$weight)
wt.sd((5-myData2$pride3)/4,myData2$weight)
wt.sd((5-myData3$pride3)/4,myData3$weight)

wt.sd((5-myData$image2)/4,myData$weight)
wt.sd((5-myData2$image2)/4,myData2$weight)
wt.sd((5-myData3$image2)/4,myData3$weight)

wt.sd((5-myData$image3)/4,myData$weight)
wt.sd((5-myData2$image3)/4,myData2$weight)
wt.sd((5-myData3$image3)/4,myData3$weight)

pride1 <- (5 - myData$pride1)/4
pride2 <- (5 - myData$pride2)/4
pride3 <- (5 - myData$pride3)/4
image2 <- (5 - myData$image2)/4
image3 <- (5 - myData$image3)/4


# Car Pride and Price (by income categories)
priceData <- na.omit(myData[myData$car_price > 0,])
pride1 <- (5 - priceData$pride1)/4
pride2 <- (5 - priceData$pride2)/4
pride3 <- (5 - priceData$pride3)/4
image2 <- (5 - priceData$image2)/4
image3 <- (5 - priceData$image3)/4

price <- priceData$car_price/1000

#install.packages("lavaan")
#library(lavaan)

model <- 'personal =~ pride1 + pride2 + pride3
          social =~ image2 + image3
          pride =~ pride1 + pride2 + pride3 + image2 + image3'
fit <- cfa(model)

personal <- (pride1 + pride2 + pride3)/3
social <- (image2 + image3)/2
pride <- (pride1 + pride2 + pride3 + image2 + image3)/5

myData2$newPrice <- myData2$car_price
for (i in 1:25) {
  year <- Shanghai_CPI$year[i]
  myData2$newPrice[myData2$car_year == year] = myData2$newPrice[myData2$car_year == year] * Shanghai_CPI$CPI[25] / Shanghai_CPI$CPI[i]
}

myData2$midIncome <- myData2$income
myData2$midIncome[myData2$income == 1] = 1000
myData2$midIncome[myData2$income == 2] = 2500
myData2$midIncome[myData2$income == 3] = 3500
myData2$midIncome[myData2$income == 4] = 4500
myData2$midIncome[myData2$income == 5] = 6000
myData2$midIncome[myData2$income == 6] = 8000
myData2$midIncome[myData2$income == 7] = 12500
myData2$midIncome[myData2$income == 8] = 17500
myData2$midIncome[myData2$income == 9] = 22500
myData2$midIncome[myData2$income == 10] = 27500
myData2$midIncome[myData2$income == 11] = 35000
myData2$midIncome[myData2$income == 12] = 45000

myData2$priceRatio <- myData2$newPrice/myData2$midIncome

priceRatio <- (myData2$newPrice/myData2$midIncome)[myData2$newPrice>0]
summary(myData2$newPrice/myData2$midIncome)

cor(pride[priceData$income>7],priceRatio[priceData$income>7])
plot(myData2$newPrice[myData2$newPrice>0 & myData2$income<4],pride[priceData$income<4],xlim=c(0,1e6))

newvars <- data.frame(cbind(myData2$midIncome,myData2$newPrice,myData2$priceRatio))
write.csv(newvars, file="~/Dropbox/ZhanZhao_Jinhua/3. Zhan Thesis/Analysis/Revolution/newVars.csv")
