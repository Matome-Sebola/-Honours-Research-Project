#Install Packages
install.packages("evd")
library(evd)
install.packages("extRemes")
library(extRemes)
install.packages("fExtremes")
library(fExtremes)
install.packages("ggpubr")
library(ggpubr)
install.packages("trend")
library(trend)
install.packages("tseries")
library(tseries)
install.packages("fBasics")
library(fBasics)
install.packages("nortest")
library(nortest)
install.packages("moments")
library(moments)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("Kendall")
library(Kendall)
install.packages("installr")
library(installr)
install.packages("POT")
library(POT)
install.packages("ismev")
library(ismev)
install.packages("e1071")
library(e1071)  
install.packages("eva")
library(eva)
install.packages("extremefit")
library(extremefit)
install.packages("xts")
library(xts)

#Read dataset
library(readxl)
rainfall <- read_excel("C:/Users/MR ML SEBOLA/Desktop/MAX_RAINFALL1.xlsx")
View(rainfall)
attach(rainfall)

#Convert exponential values to decimals
options(scipen = 999)

#Descriptive Statistics
summary(rainfall$Observations)
basicStats(rainfall$Observations)

# Exploratory data analysis
plot(rainfall$Observations,
     ylab = "Maximum monthly rainfall (mm)",
     xlab = "No. of observations", main = "")


hist(rainfall$Observations,
     xlab = "Maximum monthly rainfall (mm)",
     col = "blue", main = "")


#Test for normality
jarqueberaTest(rainfall$Observations)

#Test for stationarity using KPSS AND ADF
adf.test(rainfall$Observations)
kpss.test(rainfall$Observations)
pp.test(rainfall$Observations)

#Test for independence
MannKendall(rainfall$Observations)
mk.test(rainfall$Observations)

#Goodness of fit
ad.test(rainfall$Observations)
ks.test(rainfall$Observations,"pgpg")

#threshold selection
#mean residual life plot
mrlplot(rainfall$Observations, main = "")
#threshold choice plot
tcplot(rainfall$Observations)

#fitting the gpd to the all exceedances
exceedances<-fevd(rainfall$Observations, threshold = 50, type = "GP")
print(exceedances)
ci(exceedances, type = "parameter")
# Fitting the model GPD
excee <- gpd.fit(rainfall$Observations, 50)
# Model diagnostic
gpd.diag(excee)
#Profile log likelihood
gpd.profxi(excee, xlow = -0.5, xup = 0.5, conf = 0.95)


#fitting exponential model to all exceedances
exp_exceedances<-fevd(rainfall$Observations, threshold = 50, type = "Exponential")
print(exp_exceedances)
#likelihood ratio test
lr.test(exceedances,exp_exceedances)
#return levels for all exceedances
return.level(exceedances, return.period=c(10,20,40,50,100,200,500))
#confidence interval for return level estimates
ci(exceedances, return.period=c(2,5,10,20,40,50,100,200,500))

#extremal index
extremalindex(rainfall$Observations, threshold = 50, run.length = 2)
extremalindex(rainfall$Observations, threshold = 50, run.length = 3)
extremalindex(rainfall$Observations, threshold = 50, run.length = 4)
extremalindex(rainfall$Observations, threshold = 50, run.length = 6)









#Read dataset
library(readxl)
wind <- read_excel("C:/Users/MR ML SEBOLA/Desktop/MAX_WINDSPEED1.xlsx")
View(wind)
attach(wind)

#Convert exponential values to decimals
options(scipen = 999)

#Descriptive Statistics
summary(wind$Observations)
basicStats(wind$Observations)

# Exploratory data analysis
plot(wind$Observations,
     ylab = "Monthly maximum windspeed (mm)",
     xlab = "No. of observations")

hist(wind$Observations,
     main = "Monthly maximum wind speed (mm)",
     xlab = "Monthly windspeed(mm)",
     col = "blue")




expo<-fevd(declustered_data, type="Exponential")
attach(rainfall)
library(evd)
library(ggpubr)
library(extRemes)
library(trend)
declusted_data<-decluster(Observations, threshold = 50)
plot(declusted_data)
plot(declusted_data, xlab = "Years", ylab = "Monthly Rainfall (mm)", col = "red")
gpd<-fevd(declusted_data,threshold = 50, method = "MLE", type = "GP")
print(gpd)
expo<-fevd(declusted_data,threshold = 50, method = "MLE", type = "Exponential")
print(expo)
lr.test(expo,gpd)
plot(expo)
return.level.fevd.mle(expo, return.period = c(10, 20, 40, 50, 100), do.ci = TRUE)
