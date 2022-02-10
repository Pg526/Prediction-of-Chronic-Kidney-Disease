### Stat630 CKD project

library(mice)
library(VIM)

ckd_raw <- read.csv("casestudydata.csv", 
                    na.strings = c("", "NA"," "))
# ckd_raw <- read.csv("casestudydata_bins.csv", 
#                     na.strings = c("", "NA"," "))
head(ckd_raw)
tail(ckd_raw)

rownames(ckd_raw) <- ckd_raw$ID
ckd_raw <- ckd_raw[,-1]



###------------------------------------
### Separate the train/ test dataset
class(ckd_raw)
out_sample <- which(is.na(ckd_raw$CKD)==1) # find row number of rows with NA in 'CKD'
ckd_out <- ckd_raw[out_sample,]   ## the ones without a disease status
ckd_in <- ckd_raw[-out_sample,]   ## the ones with a disease status



###------------------------------------
### Set convenient vectors
vars <- colnames(ckd_raw)
nums <- c("Age", "Weight", "Height", "BMI", "Waist", "SBP", "DBP", "HDL", "LDL",
          "Total.Chol")
cats <- vars[!vars %in% c(nums, "ID")] #?? maybe should exclude CKD..
ords <- c("Activity")
noms <- cats[! cats %in% ords]

bins <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", 
          "AgeAbove60", "Hisp", "Non_Hisp",
          "Educ", "Income", "Obese", "Female", "Unmarried", "Dyslipidemia",
          "PVD", "PoorVision", "Smoker", "Hypertension", "Fam.Hypertension",
          "Diabetes", "Fam.Diabetes", "Stroke", "CVD", "Fam.CVD","CHF", 
          "Anemia", "CKD")

data.frame(unclass(summary()))
data.frame(unclass(summary(mydf)), check.names = FALSE, stringsAsFactors = FALSE)

des_num <- function(df, num_var) {
  sum_stat <- summary(df$num_var)
  
}


###--------------------------------
### Descriptive Analytics

## Correlation
cor(ckd)

# show correlation by symbol,just for easy reference
symnum(x = cor(ckd), 
       corr = TRUE)

# save it as excel
write.csv(cor(ckd),"cor_in.csv")

# how many people have CKD
table(ckd_in$CKD)
table(ckd_in$CKD)/nrow(ckd_in)*100
## 0           1 
## 5536        464
## 92.266667   7.733333 


### Contingency table
table(ckd_in1$CKD, ckd_in1$Racegrp)
table(ckd$CKD, ckd$Hypertension)
table(ckd$CKD, ckd$Diabetes)
table(ckd$CKD, ckd$Stroke)
table(ckd$CKD, ckd$CVD)
table(ckd$CKD, ckd$CHF)



### Find summary statistics for numerical var
write.csv(summary(ckd[,nums]),"Num_sum.csv")

lapply(X = ckd[,nums],
       FUN = sd,na.rm=TRUE)


## Check distribution - boxplot & histogram
boxplot(ckd$Age, main = "Age")
hist(ckd$Age, main = "Age")

par(mfrow=c(1, 2))
boxplot(ckd$Weight, main = "Weight")
hist(ckd$Weight, main = "Weight")

par(mfrow=c(1, 2))
boxplot(ckd$Height, main = "Height")
hist(ckd$Height, main = "Height")

par(mfrow=c(1, 2))
boxplot(ckd$BMI, main = "BMI")
hist(ckd$BMI, main = "BMI")

par(mfrow=c(1, 2))
boxplot(ckd$Waist, main = "Waist")
hist(ckd$Waist, main = "Waist")

par(mfrow=c(1, 2))
boxplot(ckd$SBP, main = "SBP")
hist(ckd$SBP, main = "SBP")

par(mfrow=c(1, 2))
boxplot(ckd$DBP, main = "DBP")
hist(ckd$DBP, main = "DBP")

par(mfrow=c(1, 2))
boxplot(ckd$HDL, main = "HDL")
hist(ckd$HDL, main = "HDL")

par(mfrow=c(1, 2))
boxplot(ckd$LDL, main = "LDL")
hist(ckd$LDL, main = "LDL")

par(mfrow=c(1, 2))
boxplot(ckd$Total.Chol, main = "Total.Chol")
hist(ckd$Total.Chol, main = "Total.Chol")


## Find Summary stat for categorical 
ct <- lapply(X = ckd[,cats],
             FUN = table)

write.csv(ct, "con_tab.csv")

summary(ckd[,cats])



###------------------------------------
## Study missing data
row_na <- rownames(ckd_in)[apply(ckd_in, 1, anyNA)]
  # 1865 rows with na

# total no. of na in whole data set
sum(is.na(ckd_in)) # 3336 null values in the whole ckd_in

# no. of na in each row (obs)
rowSums(is.na(ckd_in))

# find out how many na & unique value each col has
sapply(ckd_in, function(x) sum(is.na(x)))
sapply(imp1, function(x) length(unique(x)))

# find out how many na each col has and show percentage
colSums(is.na(ckd_in))/6000*100
  # 5%+ missing: 
    # unmarried - 5%
    # Income - 13.2%
    # PoorVision - 6.27%
    # (Fam.CVD - 4.72%)

# add a new col to show how many na in each row
ckd_in['na_count'] <- rowSums(is.na(ckd_in))

# how many obs have X na
table(ckd_in$na_count)
## 0    1      2    3    4    5    6    7    8    9   11 
## 4135 1209  325  139   66   36   45   23   15    6    1 
  # We have to impute for those with one missing value. Will give us 5344 rows
  # Can try to handle 2 missing value too

# with one missing value
m1 <- ckd_in[ckd_in$na_count == 1,]
summary(m1)
lapply(X = ckd_in[,cats],
       FUN = table)
  # missing values mainly come from: 
    # Income (521), Unmarried (227), PoorVision (155), Fam.CVD (178)
    # Eudc (4), Insured (8), Waist (30), DBP (29), Activity (4), 
    # Hypertension (36), CVD (3), CHF (11), Anemia (2), CareSource (1)

## look at correlation - find highest cor with the missing value col
# Income [missing not at random] - Educ (0.311)
# Unmarried [missing not at random] - Income (-0.25)
# Fam.CVD [Missing Completely at Random] - Fam.Hypertension (0.783)
# PoorVision - no correlation!
# DBP [Missing Completely at Random] - SBP (0.352)
# Hypertension - SBP (0.613)
# Waist - Weight (0.878); BMI (0.878)
# CHF - CVD (0.319)
# Insured - CareSourcenoplace (-0.366)
# Eudc - Income (0.311)
# Activity - Age (-0.157)
# Anemia - No correlation!

# with two missing value
m2 <- ckd_in[ckd_in$na_count == 2,]

# remove row with null value in PoorVision because it has no cor w/ other var
pv_na <- rownames(m1[is.na(m1$PoorVision),])
m1.1 <- m1[m1$PoorVision %in% c(1,0),]
# remove row with null value in Anemia because it has no cor w/ other var
anemia_na <- rownames(m1[is.na(m1$Anemia),])
m1.1 <- m1.1[m1.1$Anemia %in% c(1,0),]


# no-missing-data data set
ckd_nona <- na.omit(ckd_in) # remove ALL row with at least 1 NA
dim(ckd_nona) # only 4135 rows left in ckd_in


###-----------------------------
## Imputation

## Impute data on no na and only one/ two na
idata <- rbind(ckd_nona, m1, m2)
idata <- idata[,-40]

# factorize categorical var
idata[, cats] <- lapply(X = idata[,cats],
                        FUN = factor)

# turn numeric data into numeric??


# explore missing data with mice package
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/ 
# md.pattern: Display missing-data patterns
# only works for a not too big data set
md.pattern(idata, rotate.names = TRUE)
md.pattern(ckd_in)

# Histogram  heatmap of missing data 
# only works for a not too big data set
library(VIM)
aggr_plot <- aggr(ckd_in, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(ckd_in), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# Show distribution - grouped data into with & w/o missing data of the other var
# for numeric var only
marginplot(ckd_in[c("Weight", "SBP")]) # VIM package 
  # make sense for continuous var only
  # red plot on Y = distribution of Y when X is missing
  # blue plot on Y =  dis of Y when X is not missing 
  # Ideal situation is the two boxplots look similar meaning that the data 
  # are missing at random. 

# obvious difference 
marginplot(ckd_in[c("Age", "Weight")])
  # avg age for missing weight
marginplot(ckd_in[c("BMI", "HDL")])
  # avg BMI for missing HDL, Total.Chol
marginplot(ckd_in[c("Age", "Total.Chol")])


## impute using mice - apply one method to whole data set
tempData <- mice(data = idata,
                 m = 5, # how many round of imputation, and then take average
                 method = 'pmm', # many options, 'method(mice)' to check options
                 maxit = 50,
                 seed = 123)

tempData$imp$Income # check imputed data of variable Income
methods(mice)

ckd_imp <- complete(tempData, 1) # the number indicates which attempt to take


## impute using mice - specify method for diff var
# https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
init <- mice(idata, maxit = 0)
meth <- init$method
predM <- init$predictorMatrix

# remove variable(s) as predictor but still will be imputed
predM[, c("CKD")] <- 0

# predM[, c("CKD","AgeBelow40","AgeBetween40_50","AgeBetween50_60",
#           "AgeAbove60", "Hispa","Non_Hispa")] <- 0

# skip var for imputation but keep for prediction
# meth[c("CKD", "na_count")] = ""

# specify imputation method
# https://www.rdocumentation.org/packages/mice/versions/3.13.0/topics/mice

# Bayesian linear regression (for numeric var)
meth[c("Age", "Weight", "Height", "BMI", "Waist", "SBP", "DBP","HDL", 
       "LDL", "Total.Chol")] <- "norm" 

# logistic regression (for binary var)
meth[c("Female", "Educ", "Unmarried","Income", "Insured", "Obese",
       "Dyslipidemia", "PVD", "Smoker", "Hypertension", "Fam.Hypertension",
       "Diabetes", "Fam.Diabetes", "Stroke", "CVD", "Fam.CVD", 
       "CHF")] <- "logreg" 

# Polytomous logistic regression (for unordered var)
meth[c("Racegrp", "CareSource")] <- "polyreg"

# Proportional odds model (for ordered var)
meth[c("Activity")] <- "polr"


# Impute
set.seed(123)

imputed <- mice(idata,
                method = meth,
                predictorMatrix = predM,
                m = 5,
                seed = 123)

imp1 <- complete(imputed, 1)
imp2 <- complete(imputed, 2)
imp3 <- complete(imputed, 3)
imp4 <- complete(imputed, 4)
imp5 <- complete(imputed, 5)

write.csv(imp1, "imputed_ckd1.csv", row.names = FALSE)
write.csv(imp2, "imputed_ckd2.csv", row.names = FALSE)
write.csv(imp3, "imputed_ckd3.csv", row.names = FALSE)
write.csv(imp4, "imputed_ckd4.csv", row.names = FALSE)
write.csv(imp5, "imputed_ckd5.csv", row.names = FALSE)


write.csv(summary(ckd_nona), 'ckd_nona_summary.csv')
write.csv(summary(imp1), 'imp1_summary.csv')
write.csv(summary(imp2), 'imp2_summary.csv')
write.csv(summary(imp3), 'imp3_summary.csv')
write.csv(summary(imp4), 'imp4_summary.csv')
write.csv(summary(imp5), 'imp5_summary.csv')


# Study imputed data
lapply(X = ckd_nona[,cats],
       FUN = table)

lapply(X = imp1[,cats],
       FUN = table)

# Check missing data count again
sapply(imp1, function(x) sum(is.na(x)))
sapply(imp2, function(x) sum(is.na(x)))
sapply(imp3, function(x) sum(is.na(x)))
sapply(imp4, function(x) sum(is.na(x)))
sapply(imp5, function(x) sum(is.na(x)))



## Inspecting the distribution of original and imputed data
xyplot(imputed, Income ~ Educ+Unmarried+CKD, pch=18, cex=1)

# the distributions of the variables as individual points (for numeric var only)
# red: imputed; blue: original
stripplot(imputed, pch = 20, cex = 1.2)


install.packages("sm")
library(sm)
# https://datascienceplus.com/compare-distribution-by-density-plots/

densityplot(imputed)
compare.density(imputed, var = "Height")



###----------------------------------- 
### create dummy variables

ckd_dum <- model.matrix(~-1+Racegrp+CareSource,data=imp1) 
summary(ckd_dum)

## remove one col for each var
# ckd_dum <- ckd_dum[,-c(4,8)] # remove white & CareSourceOther

## combine data sets to include dummy var
data <- cbind(imp1[, -c(3,7)], ckd_dum )

data <- as.data.frame(scale(data))



###----------------------------------- 
## Logistic Regression
library(car)
library(caret) # partition train/ test data

# load data
imp1 <- read.csv("imputed_ckd1.csv")
imp2 <- read.csv("imputed_ckd2.csv")
imp3 <- read.csv("imputed_ckd3.csv")
imp4 <- read.csv("imputed_ckd4.csv")
imp5 <- read.csv("imputed_ckd5.csv")


# set Age bins & Hisp bin
library(dplyr)

# imp1
v1 <- imp1 %>% select(Age, Racegrp)
breaks <- c(0, 40, 50, 60, 100)
tags <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", "AgeAbove60")
age_bins <- cut(v1$Age,
                breaks = breaks,
                include.lower = TRUE, # include lowest break value?
                right = FALSE, # intervals should be closed on the right (and open on the left)
                labels = tags)
imp1$Age_bin <- age_bins
imp1$Hisp <- ifelse(imp1$Racegrp == "hispa",1,0)

# Check whether the number match
nrow(imp1[imp1$Age < 40,])
nrow(imp1[imp1$Age_bin == "AgeBelow40",])
nrow(imp1[60 <= imp1$Age,])
nrow(imp1[imp1$Age_bin == "AgeAbove60",])
nrow(imp1[imp1$Racegrp == "hispa",])
nrow(imp1[imp1$Hisp == 1,])
 
# imp2
v2 <- imp2 %>% select(Age, Racegrp)
breaks <- c(0, 40, 50, 60, 100)
tags <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", "AgeAbove60")
age_bins <- cut(v2$Age,
                breaks = breaks,
                include.lower = TRUE, # include lowest break value?
                right = FALSE, # intervals should be closed on the right (and open on the left)
                labels = tags)
imp2$Age_bin <- age_bins
imp2$Hisp <- ifelse(imp2$Racegrp == "hispa",1,0)

# Check whether the number match
nrow(imp2[imp2$Age < 40,])
nrow(imp2[imp2$Age_bin == "AgeBelow40",])
nrow(imp2[60 <= imp2$Age,])
nrow(imp2[imp2$Age_bin == "AgeAbove60",])
nrow(imp2[imp2$Racegrp == "hispa",])
nrow(imp2[imp2$Hisp == 1,])

# imp3
v3 <- imp3 %>% select(Age, Racegrp)
breaks <- c(0, 40, 50, 60, 100)
tags <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", "AgeAbove60")
age_bins <- cut(v3$Age,
                 breaks = breaks,
                 include.lower = TRUE, # include lowest break value?
                 right = FALSE, # intervals should be closed on the right (and open on the left)
                 labels = tags)
imp3$Age_bin <- age_bins
imp3$Hisp <- ifelse(imp3$Racegrp == "hispa",1,0)

# Check whether the number match
nrow(imp3[imp3$Age < 40,])
nrow(imp3[imp3$Age_bin == "AgeBelow40",])
nrow(imp3[60 <= imp3$Age,])
nrow(imp3[imp3$Age_bin == "AgeAbove60",])
nrow(imp3[imp3$Racegrp == "hispa",])
nrow(imp3[imp3$Hisp == 1,])


# imp4
v4 <- imp4 %>% select(Age, Racegrp)
breaks <- c(0, 40, 50, 60, 100)
tags <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", "AgeAbove60")
age_bins <- cut(v4$Age,
                breaks = breaks,
                include.lower = TRUE, # include lowest break value?
                right = FALSE, # intervals should be closed on the right (and open on the left)
                labels = tags)
imp4$Age_bin <- age_bins
imp4$Hisp <- ifelse(imp4$Racegrp == "hispa",1,0)

# Check whether the number match
nrow(imp4[imp4$Age < 40,])
nrow(imp4[imp4$Age_bin == "AgeBelow40",])
nrow(imp4[60 <= imp4$Age,])
nrow(imp4[imp4$Age_bin == "AgeAbove60",])
nrow(imp4[imp4$Racegrp == "hispa",])
nrow(imp4[imp4$Hisp == 1,])


# imp5
v5 <- imp5 %>% select(Age, Racegrp)
breaks <- c(0, 40, 50, 60, 100)
tags <- c("AgeBelow40", "AgeBetween40_50", "AgeBetween50_60", "AgeAbove60")
age_bins <- cut(v5$Age,
                breaks = breaks,
                include.lower = TRUE, # include lowest break value?
                right = FALSE, # intervals should be closed on the right (and open on the left)
                labels = tags)
imp5$Age_bin <- age_bins
imp5$Hisp <- ifelse(imp5$Racegrp == "hispa",1,0)

# Check whether the number match
nrow(imp5[imp5$Age < 40,])
nrow(imp5[imp5$Age_bin == "AgeBelow40",])
nrow(imp5[60 <= imp5$Age,])
nrow(imp5[imp5$Age_bin == "AgeAbove60",])
nrow(imp5[imp5$Racegrp == "hispa",])
nrow(imp5[imp5$Hisp == 1,])


cats1 <- c(cats, "Hisp", "Age_bin")
imp1[, cats1] <- lapply(X = imp1[,cats1],
                        FUN = factor)

imp2[, cats1] <- lapply(X = imp2[,cats1],
                        FUN = factor)

imp3[, cats1] <- lapply(X = imp3[,cats1],
                        FUN = factor)

imp4[, cats1] <- lapply(X = imp4[,cats1],
                        FUN = factor)

imp5[, cats1] <- lapply(X = imp5[,cats1],
                        FUN = factor)


# Partition Train/ Test data sets
set.seed(123)
sub <- createDataPartition(y = imp1$CKD, # target variable (preserve this var )
                           p = 0.80, # % in training
                           list = FALSE) # list: indexing
imp1_train <- imp1[sub, ] # create train dataframe
imp1_test <- imp1[-sub, ] # create test dataframe

# imp2
sub2 <- createDataPartition(y = imp2$CKD,
                        p = 0.8,
                        list = FALSE)
imp2_train <- imp2[sub,]
imp2_test <- imp2[-sub,]

# imp3
sub3 <- createDataPartition(y = imp3$CKD,
                        p = 0.8,
                        list = FALSE)
imp3_train <- imp3[sub,]
imp3_test <- imp3[-sub,]

# imp4
sub4 <- createDataPartition(y = imp4$CKD,
                            p = 0.8,
                            list = FALSE)
imp4_train <- imp4[sub,]
imp4_test <- imp4[-sub,]

# imp5
sub5 <- createDataPartition(y = imp5$CKD,
                            p = 0.8,
                            list = FALSE)
imp5_train <- imp5[sub,]
imp5_test <- imp5[-sub,]


write.csv(imp1_train, "imp1_train.csv", row.names = FALSE)
write.csv(imp1_test, "imp1_test.csv", row.names = FALSE)

write.csv(imp2_train, "imp2_train.csv", row.names = FALSE)
write.csv(imp2_test, "imp2_test.csv", row.names = FALSE)

write.csv(imp3_train, "imp3_train.csv", row.names = FALSE)
write.csv(imp3_test, "imp3_test.csv", row.names = FALSE)

write.csv(imp4_train, "imp4_train.csv", row.names = FALSE)
write.csv(imp4_test, "imp4_test.csv", row.names = FALSE)

write.csv(imp5_train, "imp5_train.csv", row.names = FALSE)
write.csv(imp5_test, "imp5_test.csv", row.names = FALSE)


## factorize the categorical variables
imp4_train <- read.csv('imp4_train.csv')
imp4_test <- read.csv('imp4_test.csv')

is.factor(imp4_train$CKD)

imp4_train$Age_bin <- as.factor(imp4_train$Age_bin)

cats1 <- c(cats, "Hisp", "Age_bin")
imp4_train[, cats] <- lapply(X = imp4_train[,cats],
                        FUN = factor)

imp4_test[, cats1] <- lapply(X = imp4_test[,cats1],
                             FUN = factor)

# standardize data - center & scale
std_tr <- preProcess(x = imp4,
                       method = c('center', 'scale'))
train_std <- predict(object = std_tr,
                          newdata = imp4)



## full model (w/o Total.Chol because of alias)
data <- imp1_train_std
data <- imp4_train[,-c(1,3)] # remove age & racegrp

dim(data)
m_full <- glm(CKD~., # `.` means all variables - full model
          family = "binomial",
          data = data[,-16]) # Total.Chol is alias with HDL+LDL
summary(m_full)
sum <- summary(m_full)

vif(m_full)
confint(m_full)

## remove BMI
m1 <- glm(CKD ~ Female+Educ+Unmarried+Income+
            CareSource+Insured+Weight+Height+Obese+Waist+SBP+
            DBP+HDL+LDL+Dyslipidemia+PVD+Activity+PoorVision+
            Smoker+Hypertension+Fam.Hypertension+Diabetes+
            Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia+
            Age_bin+Hisp,
          family = "binomial",
          data = data[,-16])
summary(m1)
vif(m1) # weight & waist

# test out whether BMI is a better predictor - NO!
m1.1 <- glm(CKD ~ Female+Educ+Unmarried+Income+
            CareSource+Insured+Obese+Waist+SBP+ # +Weight+Height
            DBP+HDL+LDL+Dyslipidemia+PVD+Activity+PoorVision+
            Smoker+Hypertension+Fam.Hypertension+Diabetes+
            Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia+BMI+
            Age_bin+Hisp,
          family = "binomial",
          data = data[,-16])
summary(m1.1)
vif(m1.1) # BMI & waist

# remove waist
m1.2 <- glm(CKD ~ Female+Educ+Unmarried+Income+
            CareSource+Insured+Weight+Height+Obese+SBP+
            DBP+HDL+LDL+Dyslipidemia+PVD+Activity+PoorVision+
            Smoker+Hypertension+Fam.Hypertension+Diabetes+
            Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia+
            Age_bin+Hisp,
          family = "binomial",
          data = data[,-16])
summary(m1.2)
vif(m1.2)


## Backward elimination
m2b <- step(m_full,
           direction="backward")

## explore the model
formula(m2b)
# CKD ~ Age + Female + Racegrp + Unmarried + Weight + Height + 
#   Waist + HDL + LDL + PVD + Hypertension + Diabetes + CVD + 
#   Fam.CVD + CHF + Anemia
# CKD ~ Unmarried + Insured + BMI + SBP + DBP + HDL + LDL + PVD + 
#   Activity + PoorVision + Hypertension + Diabetes + CVD + CHF + 
#   Anemia + Age_bin + Hisp
summary(m2b)

# confidence interval
#  what's the diff btw the two?
  # https://stackoverflow.com/questions/64920712/what-is-the-difference-between-confint-and-confint-default-when-calculating#:~:text=confint%20is%20a%20%22generic%22%20function,confint%20dispatches%20the%20function%20confint.&text=default%20will%20force%20the%20use%20of%20the%20%22default%22%20method.
  # confint.default assumes normality, which may not be correct in log reg. 
#confint.default(m3b)
confint(m2b)

# forward selection
m_empty <- glm(CKD ~ 1,
               family = "binomial",
               data = data[,-16])

m2f <- step(m_empty,
            direction = "forward",
            scope = list(lower = m_empty,
                         upper = m_full))

# explore the model
summary(m2f)
formula(m2f)
# CKD ~ Age + Diabetes + CVD + Hypertension + HDL + Racegrp + Anemia + 
#   PVD + LDL + CHF + Unmarried + Fam.CVD + Height + Female
# CKD ~ Age_bin + Hypertension + Hisp + CVD + PVD + Anemia + HDL + 
#   DBP + Unmarried + Insured + Diabetes + CHF + Weight + PoorVision + 
#   LDL

confint(m2f)

# enhance m2b/ m2f (m2b and m2f are the same)
m2_enh <- glm(formula = CKD ~ Age_bin + Hypertension + Hisp + CVD + PVD + 
                Anemia + HDL + DBP + Activity + Unmarried + Insured + Diabetes + 
                BMI + CHF, 
              family = "binomial", 
              data = data[, -16])
summary(m2_enh)


## Prateek's sister model
m_ptk <- glm(CKD ~ Female + Weight + Height + Obese + Waist + SBP + DBP + HDL + 
               LDL + Dyslipidemia + PVD + Activity + Smoker + Hypertension + 
               Diabetes + Stroke + CVD + Age_bin + Hisp,
             family = "binomial",
             data = data[, -16])
summary(m_ptk)

## remove highest p-value one by one until all sig
m_ptk2 <- glm(CKD ~ DBP + HDL + 
               PVD + Activity + Hypertension + 
               Diabetes + CVD + Age_bin + Hisp,
             family = "binomial",
             data = data[, -16])
summary(m_ptk2)


## hand pick model
m_hp <- glm(CKD ~ SBP + DBP +  
              LDL + PVD + Activity + 
              Hypertension + Diabetes + Fam.Diabetes + 
              Stroke + CVD + CHF + Anemia + Age_bin + Hisp,
            family = "binomial",
            data = data[,-16])
summary(m_hp)


m_hp2 <- glm(CKD ~ DBP +  
              LDL + PVD + Activity + 
              Hypertension + Diabetes +
              CVD + CHF + Anemia + Age_bin + Hisp,
            family = "binomial",
            data = data[,-16])
summary(m_hp2)


## Step 5 - Hypotehsis test of model, Compare 2 models, Definition 5-3
# Difference in Deviances (D)
D <- with(m_full, null.deviance - deviance)
D

# Degree of Freedom
df <- with(m_full, df.null - df.residual)
df

## chi-sq test - pvalue of difference
p <- with(m_full, pchisq(null.deviance - deviance,
                 df.null - df.residual,
                 lower.tail = FALSE))
p



# print everything together
cbind(model_name = 'm_full',
      Diff_in_deviance = D,
      Model_deviance = d,
      D.F. = df,
      p_val_chisq = p)


## Step 6 - Predict probabilities and Odds Ratios of New Data
# predict using test data set

test_data <- imp4_test[,-c(1,3)]

phat=predict(m1, newdata = test_data, type = "response")
summary(phat)

## odds ratios
OR <- phat/(1-phat)
summary(OR)

# OR > 1 means that the chance of having disease > the chance not having
# i.e., phat > 0.5
OR[OR > 1]
length(OR[OR > 1])


## Step 7 - Predict and Plot actual VS prediction data
# test model on data set used to build model
phat_ori <- predict(m_full,
                    newdata = data,
                    type = "response")
summary(phat_ori)
plot(data$Age, data$CKD, pch = 16, xlab = "Age", ylab = "CKD")
plot(imp4_train$Age, imp4_train$CKD, xlab = "Age", ylab = "CKD", pch = 16)
points(imp4_train$Age, phat_ori , col="blue")


# plot prediction on test data set
plot(imp4_test$Age, imp4_test$CKD, pch = 16, xlab = "Age", ylab = "CKD")
points(imp4_test$Age, phat , col="blue")


## Use my own functions 
logreg_measure(m_hp2)
find_threshold(phat, imp4_test)



## Step 8 - Classification
# choose your desirable threshold

classify <- ifelse(phat > 0.07, 1, 0)
summary(classify)

# confusion matrix
conf <- c_accuracy(test_data$CKD, classify)
round(conf,4)


  ## calculate performance matrix for threshold
  classify_chosen <- ifelse(phat_name > profit_threshold/100, 1, 0)
  conf <- c_accuracy(test_data_name$CKD, classify_chosen)
  print("### Contingency Matrices (Profit based threshold) ###")
  print(conf)
  
  
  ## find threshold using ROC
  library(pROC)
  ROC_curve_test <- roc(test_data_name$CKD~phat_name,
                        percent = TRUE,
                        plot = TRUE)
  ROC_threshold <- coords(ROC_curve_test, "best", ret = "threshold")
  
  
  print(cbind("Max-Profit ##" = max_profit,
              "Profit-Per-Head ##" = per_head_profit,
              "Threshold-(Profit) ##" = profit_threshold,
              "Threshold-(ROC) ##" = as.numeric(ROC_threshold)*100))

  
  ## calculate performance matrix for threshold from ROC
  ROC_threshold <- as.numeric(ROC_threshold)
  ROC_t <- round(ROC_threshold*100,0)

  
  classify_chosen2 <- ifelse(phat_name > ROC_t/100, 1, 0)
  conf2 <- c_accuracy(test_data_name$CKD, classify_chosen2)
  print("### Contingency Matrices (ROC based threshold) ###")
  print(c("ROC based threshold estimated at (%): ",ROC_t))
  print(ROC_curve_test$auc)
  print(conf2)


find_threshold(phat, imp4_test)


## ROC Curve of the model
library(pROC)

ROC_curve <- roc(data$CKD~phat_ori,
                 percent = TRUE,
                 plot = TRUE)
ROC_curve

ROC_curve_test <- roc(imp4_test$CKD~phat,
                      percent = TRUE,
                      plot = TRUE)
ROC_curve_test

ROC_threshold <- coords(ROC_curve_test, "best", ret = "threshold")
ROC_threshold


## confusion matrix function (courtesy to Prof. Matthew Schnedier)
c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}




