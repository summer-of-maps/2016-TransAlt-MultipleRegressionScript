# Name: Transportation Alternatives Regression Script
# Author: Parker Ziegler, Azavea, Summer of Maps Fellow
# Date: June 27, 2016
# Purpose: To create a scalable script for analyzing data on traffic
# crashes in New York City.
# Elements: The script reads in data as a .csv, creates scatterplots and box 
# plots for each variable, generates covariance and correlation matrices
# with accompanying graphics, performs OLS regression and diagnostics,
# and concludes by running a negative binomial regression with diagnostics.

# set the working directory
setwd("~/Desktop/TransAlt_Final/Deliverables/Deliverable2_StatisticalAnalysis")

# read in the data and subset to fit logical criteria
dat <- read.csv("Regression_Data.csv", header=TRUE)
  
# check to ensure that the data is being read properly
head(ctdata)

# after installing the packages below with install,packages(), load the libraries

install.packages("vcd")
install.packages("gmodels")
install.packages("MASS")
install.packages("ggplot2")
install.packages("lattice")
install.packages("xtable")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("car")
install.packages("gvlma")
install.packages("Cairo")
install.packages("dplyr")
install.packages("gamlss")

library(vcd)
library(gmodels)
library(MASS)
library(ggplot2)
library(lattice)
library(xtable)
library(Hmisc)
library(corrplot)
library(car)
library(gvlma)
library(Cairo)
library(gamlss)
library(dplyr)

# filter the data to remove erroneous results
ctdata <- dplyr::filter(dat, TOTAL_POP > 500, TOTAL_POP < 25000, POP_PH < 100, MED_INCOME > 0, POV_RATE < 100, EMPLOYED > 0)

# view the data in the viewer
View(ctdata)

# obtain summary statistics on the data
summary(ctdata)

# check the data structure; R will likely interpret this data as a data.frame
str(ctdata)

# create scatterplots of the data
par(mfrow=c(4,4))
scatterplot(CRASH_AREA ~ TOTAL_POP, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Total Population", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ POV_RATE, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Poverty Rate (%)", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ FAM_P_RATE, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Family Poverty Rate (%)", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ UNEMP_RATE, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Unemployment Rate (%)", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ EMPLOYED, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Employment Rate (%)", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ MED_INCOME, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Median Income ($)", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ NUM_DEVS, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Number of Public Housing Developments", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ NUM_UNITS, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Number of Public Housing Units", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ POP_PH, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="% Population Living in Public Housing", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ PERC_CAR, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="% Commute by Car", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ PERC_PUB, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="% Commute by Public Transportation", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ PERC_BIKE, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="% Commute by Bike", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ PERC_WALK, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="% Commute by Walking", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ LANE_AREA, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Bike Lane Mileage per Sq. Mi.", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ DIS_2_CP, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Mean Distance to Central Park", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ DIS_2_BIKE, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Mean Distance to Bike Lane", ylab = "Crashes per Sq. Mi.")
scatterplot(CRASH_AREA ~ POP_DENS, data = ctdata, spread = FALSE, smoother.args = list(lty=2), pch = 1, xlab="Population Desnity", ylab = "Crashes per Sq. Mi.")

# create a covariance matrix and a correlation matrix
options(digits=2)
cov(ctdata, use="pairwise.complete.obs", method=c("pearson", "kendall", "spearman"))
cormatrix <- cor(ctdata[c(16:37, 56)], use="pairwise.complete.obs", method=c("pearson", "kendall", "spearman"))

# function for formatting correlation matrix into a table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# coerce your data.frame into a matrix
ctdata_matrix <- as.matrix(ctdata[c(16:37, 56)])

# create a variable that stores the pearson coefficient for significant correlation
corsig <- rcorr(ctdata_matrix, type="pearson")

# place the correlation and significance values into a table
corsigtable <- flattenCorrMatrix(corsig$r, corsig$P)

# write a .csv to store the data
write.csv(corsigtable, "160801_Correlation_Significance_Table.csv")

# generate a correlation plot to help visualize the strength and direction of correlation
corrplot(cormatrix, type="upper", order="hclust", tl.col="black", tl.srt=45)

# part 1 of the script is complete.
# part 2 of the script involves creating a regression model from the data

# try a regression model with some of the candidate variables
fit <- lm(CRASH_AREA ~ MED_INCOME + PERC_CAR + POP_PH + POV_RATE + DIS_2_CP, data = ctdata, na.action = na.omit)
summary(fit)

# plot the model to examine it
plot(fit)

# check the variance inflation factors on the variables (redundancy)
vif(fit)

# check for normality using a Q-Q plot and a Shapiro-Wilks test
qqPlot(fit, labels=row.names(ctdata$City.Council.District), id.method="identify", simulate=TRUE, main="Q-Q Plot")
shapiro.test(ctdata$CRASH_AREA)
shapiro.test(ctdata$CRASH_POP)

#check for lineraity using a component plus residuals plot
crPlots(fit)

# check for homoscedasticity using ncvTest and spreadLevelPlot
ncvTest(fit)
spreadLevelPlot(fit)

# check the global suitability of the model using the gvlma function
gvmodel <- gvlma(fit)
summary(gvmodel)

# if the model passes these diagnostics, it likely fits the requirements
# for OLS regression. if it does not, rerun the model with new variables
# or consider a different type of regression model.

# another method involves trying best subsets regression. this involves
# automated iterative model building.

# install packages
install.packages("leaps")
library (leaps)

# run the best subsets algorithm
attach(ctdata)
leaps <- regsubsets(ctdata$CRASH_AREA ~ ctdata$EMPLOYED + ctdata$MED_INCOME + ctdata$PERC_PUB + ctdata$PERC_BIKE + ctdata$POP_PH + ctdata$LANES_AREA + ctdata$POV_RATE, data=ctdata, nbest=10)
summary(leaps)

# try a stepwise regression for similar results. this is an exploratory
# process that can be helpful in discerning which candidate variables
# may provide the best fit.
regression <- lm(ctdata$CRASH_AREA ~ ctdata$POV_RATE + ctdata$FAM_P_RATE + ctdata$UNEMP_RATE + ctdata$EMPLOYED + ctdata$MED_INCOME + ctdata$NUM_DEVS + ctdata$NUM_UNITS + ctdata$POP_PH + ctdata$PERC_CAR + ctdata$PERC_PUB + ctdata$PERC_BIKE + ctdata$PERC_WALK + ctdata$PERC_BACH + ctdata$LANES_AREA + ctdata$DIS_2_CP + ctdata$DIS_2_LANE)
step <- stepAIC(regression, direction="both")
step$anova

# the outcome data does not follow a normal distribution, meaning that OLS
# regression may not be the appropriate model. instead, try a regression model
# that is better suited to count data.

# start by plotting the data
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
attach(ctdata)
hist(ctdata$CRASH_AREA, xlab = "Crashes per Square Mile", main = "Distribution of Normalized Crash Count", breaks = 60)
hist(ctdata$NUM_CRASH, xlab = "Number of Crashes", main = "Distribution of Crash Count", breaks = 60)

# now, fit a generalized linear model of the Poisson family (appropriate for
# count data)

fit <- glm(ctdata$NUM_CRASH ~ ctdata$EMPLOYED + ctdata$PERC_PUB + ctdata$PERC_BIKE + ctdata$BIKE_LANES, data=ctdata, family=poisson())
summary(fit)

# check for overdispersion
deviance(fit)/df.residual(fit)

# overdispersion is present - this means we require a different type
# of generalized linear model (GLM). use the GAMLSS package to check
# which family best fits your response variable.

# start with poisson for reference
histDist(ctdata$NUM_CRASH, family=PO)

# try negative binomial
histDist(ctdata$NUM_CRASH, family=NBI)

# try zero-inflated poisson
histDist(ctdata$NUM_CRASH, family=ZIP)

# try normal
histDist(ctdata$NUM_CRASH, family=NO)

# based on the histograms, the negative binomial family best fits the
# distribution of the data. Now, try iterative model making. Compare
# against the null regression.
fit.nbbest <- glm.nb(NUM_CRASH ~ Z_MED_INCOME + Z_POP_DENS + Z_POV_RATE + Z_DIS_2_CP + offset(TRACT_AREA), data=ctdata)
fit.nbnew <- glm.nb(NUM_CRASH ~ Z_MED_INCOME + Z_POP_DENS + Z_POV_RATE + Z_PERC_CAR + offset(TRACT_AREA), data=ctdata)
fit.nbnull <- glm.nb(NUM_CRASH ~ 1, data=ctdata)

# obtain the BIC of all tests, along with coefficients. the lowest BIC
# represents the best fit. use a chisq test to measure overall model
# significance. obtain plots of the models.
BIC(fit.nbbest, fit.nbnew, fit.nbnull)
summary(fit.nbbest)
pchisq(summary(fit.nbbest)$deviance, summary(fit.nbnew)$df.residual)
par(mfrow=c(2,2))
plot(fit.nbbest)

# the script is complete.

