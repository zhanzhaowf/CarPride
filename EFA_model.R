# Import: Getting Started

myData <- read.delim("C:/Users/Tim/Dropbox/Tim and Drewry/Data and Analysis/Shanghai_fs.dat")
viewData(myData)
library(psych)

# Variable definition
pride1 <- myData$pride1_pos
pride2 <- myData$pride2_pos
pride3 <- myData$pride3_pos
pride4 <- myData$pride4_neg
pride5 <- myData$pride5_neg
brag <- myData$brag_neg
wash <- myData$wash_neg
crash <- myData$crash_pos
att1 <- myData$Q71_1_neg
att2 <- myData$Q71_2_neg
att3 <- myData$Q71_3_neg
att4 <- myData$Q71_4_neg
att5 <- myData$Q71_5_neg
att6 <- myData$Q71_6_neg
att7 <- myData$Q71_7_neg
att8 <- myData$Q71_8_neg
att9 <- myData$Q71_9_neg
att10 <- myData$Q71_10_neg
att11 <- myData$Q71_11_neg
att12 <- myData$Q71_12_neg
img1 <- myData$Q73_1_neg
img2 <- myData$Q73_2_neg
img3 <- myData$Q73_3_neg
img4<- myData$Q73_4_pos

# Subsets
data <- data.frame(pride1, pride2, pride3, pride4, pride5, brag, wash, crash, att1, att2, att3, att4, att5, att6, att7, att8, att9, att10, att11, att12, img1, img2, img3, img4)

# Correlation matrix
cormat <- cor(data)

# Parallel analysis
fa.parallel(cormat, n.obs=928, fm="wls", fa="both", main="Parallel Analysis Scree Plots", n.iter=100)

# Suggests 7 factors or 4 components
# However, Jinhua suggests iterating this a bit--so let's use 7 as a bookend value and test 4, 5, and 6 factor solutions.

# Factor Analysis
efa_scores <- fa(r=cormat, nfactors=7, residuals = TRUE, rotate="promax", scores = "regression", digits = 3, max.iter=1000, warnings=TRUE, fm="wls")
efa_scores4 <- fa(r=cormat, nfactors=4, residuals = TRUE, rotate="promax", scores = "regression", digits = 3, max.iter=1000, warnings=TRUE, fm="wls")
efa_scores5<- fa(r=cormat, nfactors=5, residuals = TRUE, rotate="promax", scores = "regression", digits = 3, max.iter=1000, warnings=TRUE, fm="wls")
efa_scores6<- fa(r=cormat, nfactors=6, residuals = TRUE, rotate="promax", scores = "regression", digits = 3, max.iter=1000, warnings=TRUE, fm="wls")

# After this, I export the efa_scores output to excel for interpretation and comparison.
# Specifically, this means pulling out the loadings, determining which indicators load on factors at >0.3,
# and interpreting the factor outputs of different EFA runs. 

#Comparing goodness-of-fit statistics is relevant too, but they're all within acceptable bounds.

# The four factor model produces the most coherent and interperable set of factors, so let's use them.


# Regression Conversion

# Okay, what's going on here is that we're creating factor scores. First we have to create new factors,
# then we use a for-loop to sum up all the weights*indicators. Apparently there's a built-in method for this
# somewhere but it honestly seems more complicated than this.

# Creating new factors
factor1 <- efa_scores4$weights[1,1]*data[1]
factor2 <- efa_scores4$weights[1,2]*data[1]
factor3 <- efa_scores4$weights[1,3]*data[1]
factor4 <- efa_scores4$weights[1,4]*data[1]
#factor5 <- efa_scores$weights[1,5]*data[1]
#factor6 <- efa_scores$weights[1,6]*data[1]
#factor7 <- efa_scores$weights[1,7]*data[1]

for(i in 2:24){
  factor1 <- factor1 + efa_scores4$weights[i,1]*data[i]
  factor2 <- factor2 + efa_scores4$weights[i,2]*data[i]
  factor3 <- factor3 + efa_scores4$weights[i,3]*data[i]
  factor4 <- factor4 + efa_scores4$weights[i,4]*data[i]
  #factor5 <- factor5 + efa_scores$weights[i,5]*data[i]
  #factor6 <- factor6 + efa_scores$weights[i,6]*data[i]
  #factor7 <- factor7 + efa_scores$weights[i,7]*data[i]
}

# Regression prep
# Next up: more variables!

# Demographics
dem_age <- myData$age
dem_gdr <- myData$gender
dem_edu <- myData$education
dem_emp <- myData$employment
dem_inc <- myData$income

# Location
loc <- myData$location
subdist <- myData$subway_dist
busdist <- myData$bus_dist
subwalk <- myData$Q14_1
buswalk <- myData$Q14_2
PT_avlive <- myData$PT_avlive
PT_avwork <- myData$PT_avwork
PT_avgo <- myData$PT_avgo
PT_avtravel <- myData$PT_avtravel
wkhm_dist <- myData$work_home_dist

# Autos
carown <- myData$owncar.
ncars <- myData$cars

# Travel DVs
kmtotal <- myData$km_total
prime_mode <- myData$Pri_mode
comm_time <- myData$trip_time
driv_frq <- myData$Q19_1
pass_frq <- myData$Q19_2
driv_frq_rv <- 9-driv_frq
pass_frq_rv <- 9-pass_frq

# New data frame
data2 <- data.frame(dem_age, dem_gdr, dem_edu, dem_emp, dem_inc, loc, subdist, busdist, subwalk, buswalk, PT_avlive, PT_avwork, PT_avgo, PT_avtravel, wkhm_dist, carown, ncars, kmtotal, prime_mode, comm_time, driv_frq, pass_frq, factor1, factor2, factor3, factor4)

# cleanup
names(data2)[23] <- "factor1"
names(data2)[24] <- "factor2"
names(data2)[25] <- "factor3"
names(data2)[26] <- "factor4"
#names(data2)[27] <- "factor5"
#names(data2)[28] <- "factor6"
#names(data2)[29] <- "factor7"

#remove("factor1", "factor2", "factor3", "factor4", "factor5", "factor6", "factor7")
remove("factor1", "factor2", "factor3", "factor4")

# Here seems like a good place to create driver/nondriver data sets.
owner_data <- subset(data2, carown > 0, select = c(dem_age, dem_gdr, dem_edu, dem_emp, dem_inc, loc, subdist, busdist, subwalk, buswalk, PT_avlive, PT_avwork, PT_avgo, PT_avtravel, wkhm_dist, ncars, kmtotal, prime_mode, comm_time, driv_frq, pass_frq, factor1, factor2, factor3, factor4))
nonowner_data <- subset(data2, carown < 1, select = c(dem_age, dem_gdr, dem_edu, dem_emp, dem_inc, loc, subdist, busdist, subwalk, buswalk, PT_avlive, PT_avwork, PT_avgo, PT_avtravel, wkhm_dist, ncars, kmtotal, prime_mode, comm_time, driv_frq, pass_frq, factor1, factor2, factor3, factor4))

# For some reason the non-owner version doesn't work with carown = 0. Weird. 

# Descriptives
summary(owner_data)
summary(nonowner_data)

# Diagnostics
pairs(~dem_age+dem_gdr+dem_edu+dem_emp+dem_inc, data=data2,
      main="Simple Scatterplot Matrix")

# MLR on demographics
reg1_o <- lm(factor1 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=owner_data)
reg2_o <- lm(factor2 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=owner_data)
reg3_o <- lm(factor3 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=owner_data)
reg4_o <- lm(factor4 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=owner_data)

reg1_no <- lm(factor1 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=nonowner_data)
reg2_no <- lm(factor2 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=nonowner_data)
reg3_no <- lm(factor3 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=nonowner_data)
reg4_no <- lm(factor4 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=nonowner_data)

#reg5 <- lm(factor5 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=data2)
#reg6 <- lm(factor6 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=data2)
#reg7 <- lm(factor7 ~ dem_age + dem_gdr + dem_edu + dem_emp + dem_inc, data=data2)


# show results
summary(reg1_o)
summary(reg2_o)
summary(reg3_o)
summary(reg4_o)
summary(reg1_no)
summary(reg2_no)
summary(reg3_no)
summary(reg4_no)
#summary(reg5)
#summary(reg6)
#summary(reg7)


# Base models

# Drivers
kmtotal_base_o <- lm(owner_data$kmtotal ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars)
driv_frq_rv_base_o <- lm(owner_data$driv_frq ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars)
pass_frq_rv_base_o <- lm(owner_data$pass_frq ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars)

# Non-drivers
driv_frq_rv_base_no <- lm(nonowner_data$driv_frq ~ nonowner_data$dem_age+nonowner_data$dem_gdr+nonowner_data$dem_edu+nonowner_data$dem_emp+nonowner_data$dem_inc+nonowner_data$loc+nonowner_data$subdist+nonowner_data$busdist+nonowner_data$subwalk+nonowner_data$buswalk+nonowner_data$PT_avlive+nonowner_data$PT_avwork+nonowner_data$PT_avgo+nonowner_data$PT_avtravel+nonowner_data$wkhm_dist+nonowner_data$ncars)
pass_frq_rv_base_no <- lm(nonowner_data$pass_frq ~ nonowner_data$dem_age+nonowner_data$dem_gdr+nonowner_data$dem_edu+nonowner_data$dem_emp+nonowner_data$dem_inc+nonowner_data$loc+nonowner_data$subdist+nonowner_data$busdist+nonowner_data$subwalk+nonowner_data$buswalk+nonowner_data$PT_avlive+nonowner_data$PT_avwork+nonowner_data$PT_avgo+nonowner_data$PT_avtravel+nonowner_data$wkhm_dist+nonowner_data$ncars)

# Reports

summary(kmtotal_base_o)
summary(driv_frq_rv_base_o)
summary(pass_frq_rv_base_o)

summary(driv_frq_rv_base_no)
summary(pass_frq_rv_base_no)

# Models with factor scores

# Drivers
kmtotal_fs_o <- lm(owner_data$kmtotal ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)
driv_frq_rv_fs_o <- lm(owner_data$driv_frq ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)
pass_frq_rv_fs_o <- lm(owner_data$pass_frq ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)

# Non-drivers
driv_frq_rv_fs_no <- lm(nonowner_data$driv_frq ~ nonowner_data$dem_age+nonowner_data$dem_gdr+nonowner_data$dem_edu+nonowner_data$dem_emp+nonowner_data$dem_inc+nonowner_data$loc+nonowner_data$subdist+nonowner_data$busdist+nonowner_data$subwalk+nonowner_data$buswalk+nonowner_data$PT_avlive+nonowner_data$PT_avwork+nonowner_data$PT_avgo+nonowner_data$PT_avtravel+nonowner_data$wkhm_dist+nonowner_data$ncars+nonowner_data$factor1+nonowner_data$factor2+nonowner_data$factor3+nonowner_data$factor4)
pass_frq_rv_fs_no <- lm(nonowner_data$pass_frq ~ nonowner_data$dem_age+nonowner_data$dem_gdr+nonowner_data$dem_edu+nonowner_data$dem_emp+nonowner_data$dem_inc+nonowner_data$loc+nonowner_data$subdist+nonowner_data$busdist+nonowner_data$subwalk+nonowner_data$buswalk+nonowner_data$PT_avlive+nonowner_data$PT_avwork+nonowner_data$PT_avgo+nonowner_data$PT_avtravel+nonowner_data$wkhm_dist+nonowner_data$ncars+nonowner_data$factor1+nonowner_data$factor2+nonowner_data$factor3+nonowner_data$factor4)

# Reports

summary(kmtotal_fs_o)
summary(driv_frq_rv_fs_o)
summary(pass_frq_rv_fs_o)

summary(driv_frq_rv_fs_no)
summary(pass_frq_rv_fs_no)


# Drivers
#kmtotal_fs <- lm(owner_data$kmtotal ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)
#driv_frq_rv_fs <- lm(owner_data$driv_frq_rv ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)
#pass_frq_rv_fs <- lm(owner_data$pass_frq_rv ~ owner_data$dem_age+owner_data$dem_gdr+owner_data$dem_edu+owner_data$dem_emp+owner_data$dem_inc+owner_data$loc+owner_data$subdist+owner_data$busdist+owner_data$subwalk+owner_data$buswalk+owner_data$PT_avlive+owner_data$PT_avwork+owner_data$PT_avgo+owner_data$PT_avtravel+owner_data$wkhm_dist+owner_data$ncars+owner_data$factor1+owner_data$factor2+owner_data$factor3+owner_data$factor4)
# Non-drivers
#kmtotal_fs <- lm(kmtotal ~ dem_age+dem_gdr+dem_edu+dem_emp+dem_inc+loc+subdist+busdist+subwalk+buswalk+PT_avlive+PT_avwork+PT_avgo+PT_avtravel+wkhm_dist+ncars+data2$factor1+data2$factor2+data2$factor3+data2$factor4+data2$factor5+data2$factor6+data2$factor7)
#driv_frq_rv_fs <- lm(driv_frq_rv ~ dem_age+dem_gdr+dem_edu+dem_emp+dem_inc+loc+subdist+busdist+subwalk+buswalk+PT_avlive+PT_avwork+PT_avgo+PT_avtravel+wkhm_dist+ncars+data2$factor1+data2$factor2+data2$factor3+data2$factor4+data2$factor5+data2$factor6+data2$factor7)
#pass_frq_rv_fs <- lm(pass_frq_rv ~ dem_age+dem_gdr+dem_edu+dem_emp+dem_inc+loc+subdist+busdist+subwalk+buswalk+PT_avlive+PT_avwork+PT_avgo+PT_avtravel+wkhm_dist+ncars+data2$factor1+data2$factor2+data2$factor3+data2$factor4+data2$factor5+data2$factor6+data2$factor7)
#summary(kmtotal_fs)
#summary(driv_frq_rv_fs)
#summary(pass_frq_rv_fs)

# Model Comparisons
anova(kmtotal_base_o, kmtotal_fs_o)
anova(driv_frq_rv_base_o, driv_frq_rv_fs_o)
anova(pass_frq_rv_base_o, pass_frq_rv_fs_o)

anova(driv_frq_rv_base_no, driv_frq_rv_fs_no)
anova(pass_frq_rv_base_no, pass_frq_rv_fs_no)
