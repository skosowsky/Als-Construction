#-------------------------------------#
#            Seasonal Models          #
#                                     #
#            Dr Aric LaBarr           #
#-------------------------------------#

# Needed Libraries for Analysis #
library(foreign)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)


# Saving File Locations and Uploading SAS File #
sashome <- "C:/Program Files/SASHome/SASFoundation/9.4/"
timehome <- "E:/Courses/Time Series/Data"

Construction <- read.ssd(file.path(timehome), "Constr", sascmd=file.path(sashome, "sas.exe"))
NOAA.Texas <- read.ssd(file.path(timehome), "Txnoaa", sascmd=file.path(sashome, "sas.exe"))


# Drilling Down into the Needed Data Sets for Easy Variable Naming and Creating Time Series Data Objects #
attach(Construction)
names(Construction)

Contracts <- ts(CONTRCTS, start=1983, frequency=12)

plot(Contracts)


# Building a Model with Seasonal Dummy Variables #
Month <- rep(0, length(Contracts))
Month <- Month + 1:12
M <- factor(Month)
M <- relevel(M, ref="12")

Season.Lin <- lm(Contracts ~ M)
summary(Season.Lin)

M.Matrix <- model.matrix(~M)

Season.ARIMA <- Arima(Contracts, xreg=M.Matrix[,2:12], method="ML")
summary(Season.ARIMA)


# Forecasting with Seasonal Dummy Variables #
For.Month <- rep(0, 24)
For.Month[1:2] <- For.Month[1:2] + 11:12
For.Month[3:24] <- For.Month[3:24] + 1:12
For.M <- factor(For.Month)
For.M <- relevel(For.M, ref="12")

Con.Forecast <- forecast(Season.ARIMA, h=24, xreg=model.matrix(~For.M)[,2:12], method="ML")
plot(Con.Forecast)

# Forecasting with Seasonal Dummy Variables AND Trend #
Month <- rep(0, length(Contracts))
Month <- Month + 1:12
M <- factor(Month)
M <- relevel(M, ref="12")

Season.Lin <- lm(Contracts ~ M)
summary(Season.Lin)

M.Matrix <- model.matrix(~M)

Season.ARIMA.T <- Arima(Contracts, xreg=M.Matrix[,2:12], include.drift=TRUE, method="ML") # Option in Arima Function to Include Drift Automatically #
summary(Season.ARIMA)

Drift <- 1:length(Contracts)
Season.ARIMA.T <- Arima(Contracts, xreg=cbind(M.Matrix[,2:12],Drift), method="ML") # Adding Drift Manually #
summary(Season.ARIMA)

For.Drift <- (length(Contracts)+1):(length(Contracts) + 24)
Con.Forecast2 <- forecast(Season.ARIMA.T, h=24, xreg=cbind(model.matrix(~For.M)[,2:12],For.Drift))
plot(Con.Forecast2)

Quad.Drift <- Drift*Drift
Season.ARIMA.QT <- Arima(Contracts, xreg=cbind(M.Matrix[,2:12],Drift,Quad.Drift), method="ML") # Adding Quadratic Drift #
summary(Season.ARIMA.QT)

For.Quad.Drift <- For.Drift*For.Drift
Con.Forecast3 <- forecast(Season.ARIMA.QT, h=24, xreg=cbind(model.matrix(~For.M)[,2:12],For.Drift, For.Quad.Drift))
plot(Con.Forecast3)


# Building a Seasonal ARIMA Model #
detach(Construction)
attach(NOAA.Texas)
names(NOAA.Texas)

Temperature <- ts(TEMPERAT[1189:1369], start=1994, frequency=12)

nsdiffs(Temperature) # Says No Seasonal Difference Needed, but SAS uses Dickey-Fuller Test and Says One Seasonal Difference #

ndiffs(diff(Temperature, lag=12))

Acf(diff(Temperature, lag=12))
Pacf(diff(Temperature, lag=12))

S.ARIMA <- Arima(diff(Temperature, lag=12), order=c(0,0,1), seasonal=c(0,0,1), method="ML")
summary(S.ARIMA)

S.ARIMA2 <- Arima(diff(Temperature, lag=12), order=c(0,0,13), method="ML")
summary(S.ARIMA2)


# Building a Model with Trigonometric Functions #
detach(NOAA.Texas)
attach(Construction)
names(Construction)

Contracts <- ts(CONTRCTS, start=1983, frequency=12)

X <- data.frame(matrix(0,nrow=length(Contracts),ncol=10))
for(i in 1:length(Contracts)){
  X[i,1] <- cos(2*pi*1*i/12)
  X[i,2] <- sin(2*pi*1*i/12)
  X[i,3] <- cos(2*pi*2*i/12)
  X[i,4] <- sin(2*pi*2*i/12)
  X[i,5] <- cos(2*pi*3*i/12)
  X[i,6] <- sin(2*pi*3*i/12)
  X[i,7] <- cos(2*pi*4*i/12)
  X[i,8] <- sin(2*pi*4*i/12)
  X[i,9] <- cos(2*pi*5*i/12)
  X[i,10] <- sin(2*pi*5*i/12)
}

Trig.Model <- Arima(Contracts, order=c(0,0,0), method="ML", xreg=X)
summary(Trig.Model)

Trig.Model2 <- Arima(Contracts, order=c(0,0,0), method="ML", xreg=X[,c(1,2,4,10)])
summary(Trig.Model2)

For.X <- data.frame(matrix(0,nrow=24,ncol=10))
for(i in 1:24){
  For.X[i,1] <- cos(2*pi*1*(length(Contracts)+i-1)/12)
  For.X[i,2] <- sin(2*pi*1*(length(Contracts)+i-1)/12)
  For.X[i,3] <- cos(2*pi*2*(length(Contracts)+i-1)/12)
  For.X[i,4] <- sin(2*pi*2*(length(Contracts)+i-1)/12)
  For.X[i,5] <- cos(2*pi*3*(length(Contracts)+i-1)/12)
  For.X[i,6] <- sin(2*pi*3*(length(Contracts)+i-1)/12)
  For.X[i,7] <- cos(2*pi*4*(length(Contracts)+i-1)/12)
  For.X[i,8] <- sin(2*pi*4*(length(Contracts)+i-1)/12)
  For.X[i,9] <- cos(2*pi*5*(length(Contracts)+i-1)/12)
  For.X[i,10] <- sin(2*pi*5*(length(Contracts)+i-1)/12)
}

Con.Trig.Forecast <- forecast(Trig.Model, h=24, xreg=For.X)
plot(Con.Trig.Forecast)

# Building a Model with Trigonometric and Drift Functions #
Trig.Model3 <- Arima(Contracts,order=c(0,0,0), method="ML", xreg=cbind(X,Drift,Quad.Drift))
summary(Trig.Model3)

Con.Trig.Forecast <- forecast(Trig.Model3, h=24, xreg=cbind(For.X,For.Drift, For.Quad.Drift))
plot(Con.Trig.Forecast)
