#ARIMA Model

library(readxl)
Bitcoin_dataset_updated <- read_excel("E:/sem 3-2/Applied econometrics/Assignment 3/Bitcoin_dataset_updated.xlsx", sheet = "Sheet1")

library(dplyr)
library(lubridate)
Bitcoin_dataset_updated$week <- floor_date(Bitcoin_dataset_updated$Date, "week") 
x<-ddply(Bitcoin_dataset_updated, .(week), function(z) mean(z$price))             #to convert the data into weekly averages.
View(x)

write.csv(x, "E:/sem 3-2/Applied econometrics/Assignment 3/weekly.csv",row.names = FALSE)
write.csv(Bitcoin_dataset_updated, "E:/sem 3-2/Applied econometrics/Assignment 3/bitcoin.csv",row.names = FALSE)

weekly <- read_excel("E:/sem 3-2/Applied econometrics/Assignment 3/weekly.xlsx")
View(weekly)
rm(x)
Bitcoin.ts <- ts(weekly$`avg price`, frequency = 52, start = c(2015,1))         # considered values only from 2015 first week above values are deleted in excel manually before loading the weekly df.
Bitcoin.ts

plot.ts(Bitcoin.ts)
plot.ts(log(Bitcoin.ts))
Bitcoin.tsdiff1 = diff(Bitcoin.ts, differences = 1)
plot.ts(Bitcoin.tsdiff1)
Bitcoin.tsdiff2 = diff(Bitcoin.tsdiff1, differences = 1)
plot.ts(Bitcoin.tsdiff2)

library(tseries)
library(aTSA)

adf.test(Bitcoin.ts, alternative="stationary")
adf.test(Bitcoin.tsdiff1, alternative="stationary")
adf.test(Bitcoin.tsdiff2, alternative="stationary")

stationary.test(Bitcoin.ts)
stationary.test(Bitcoin.ts, method = "pp")
stationary.test(Bitcoin.ts, method = "kpss")
stationary.test(Bitcoin.tsdiff1)
stationary.test(Bitcoin.tsdiff1, method = "pp")
stationary.test(Bitcoin.tsdiff2)

acf(Bitcoin.tsdiff1, lag.max = 50)
pacf(Bitcoin.tsdiff1, lag.max = 50)
library(forecast)
library(urca)
auto.arima(Bitcoin.tsdiff1)
auto.arima(Bitcoin.ts)
bitcoin.tsarima <- arima(Bitcoin.ts, order = c(1,1,0))
View(bitcoin.tsarima)
summary(bitcoin.tsarima)

Bitcoin.tsforecasts <- forecast(bitcoin.tsarima, h = 10, level = c(95))
Bitcoin.tsforecasts
plot(Bitcoin.tsforecasts)

acf(Bitcoin.tsforecasts$residuals, lag.max=50)
Box.test(Bitcoin.tsforecasts$residuals, lag=50, type="Ljung-Box")
Box.test(Bitcoin.tsforecasts$residuals, lag=50, type="Box-Pierce")
plot.ts(Bitcoin.tsforecasts$residuals)
hist(Bitcoin.tsforecasts$residuals, breaks = 50)


#ARDL Model

weekly2 <- read_excel("E:/sem 3-2/Applied econometrics/Assignment 3/weekly2.xlsx")
View(weekly2)
Bitcoin2.ts <- ts(weekly2, frequency = 52, start = c(2015,1))

plot.ts(Bitcoin2.ts[,1])
plot.ts(Bitcoin2.ts[,2])
plot.ts(Bitcoin2.ts[,3])
plot.ts(Bitcoin2.ts[,4])

x = diff(Bitcoin2.ts[,3], differences = 1)
y = diff(Bitcoin2.ts[,4], differences = 1)
plot.ts(x)
plot.ts(y)

stationary.test(Bitcoin2.ts[,2])
stationary.test(Bitcoin2.ts[,3])
stationary.test(Bitcoin2.ts[,4])
stationary.test(x)
stationary.test(y)

Bitcoin2.ts.tab <- cbind(Bitcoin2.ts,diff(Bitcoin2.ts[,2]),diff(Bitcoin2.ts[,3]),diff(Bitcoin2.ts[,4]))
View(Bitcoin2.ts.tab)

library(dynlm)
library(knitr)
library(broom)
Bitcoin2.tsdyn1 <- dynlm(d(price)~L(d(price),1)+d(transactions)+d(SP),data = Bitcoin2.ts)
Bitcoin2.tsdyn2 <- dynlm(L(price,1)~price+L(price,-1)+L(d(transactions),1)+L(d(SP),1),data = Bitcoin2.ts)
Bitcoin2.tsdyn3 <- dynlm(d(price)~L(d(price),1)+L(d(transactions),0:1)+L(d(SP),0),data = Bitcoin2.ts)
Bitcoin2.tsdyn4 <- dynlm(d(price)~L(d(price),1)+L(d(transactions),0:2)+L(d(SP),0),data = Bitcoin2.ts)
Bitcoin2.tsdyn5 <- dynlm(d(price)~L(d(price),1)+L(d(transactions),0:2)+L(d(SP),0:2),data = Bitcoin2.ts)
Bitcoin2.tsdyn6 <- dynlm(d(price)~L(d(price),1)+L(d(transactions),0:3)+L(d(SP),0:3),data = Bitcoin2.ts)

kable(tidy(summary(Bitcoin2.tsdyn1)), digits=4, caption="The Bitcoin auto regressive distributed lag model1")
kable(tidy(summary(Bitcoin2.tsdyn2)), digits=4, caption="The Bitcoin auto regressive distributed lag model2")
kable(tidy(summary(Bitcoin2.tsdyn3)), digits=4, caption="The Bitcoin auto regressive distributed lag model3")
kable(tidy(summary(Bitcoin2.tsdyn4)), digits=4, caption="The Bitcoin auto regressive distributed lag model4")
kable(tidy(summary(Bitcoin2.tsdyn5)), digits=4, caption="The Bitcoin auto regressive distributed lag model5")
kable(tidy(summary(Bitcoin2.tsdyn6)), digits=4, caption="The Bitcoin auto regressive distributed lag model6")

glL1 <- glance(Bitcoin2.tsdyn1)[c("r.squared","statistic","AIC","BIC")]
glL3 <- glance(Bitcoin2.tsdyn3)[c("r.squared","statistic","AIC","BIC")]
glL4 <- glance(Bitcoin2.tsdyn4)[c("r.squared","statistic","AIC","BIC")]

tabl <- rbind(glL1, as.numeric(glL3), as.numeric(glL4))
kable(tabl, caption="Goodness-of-fit statistics for Bitcoin-ARDL models")

ehat <- resid(Bitcoin2.tsdyn4)
acf(ehat,lag.max = 50)

res1 <- resid(Bitcoin2.tsdyn4)
res2 <- lag(resid(Bitcoin2.tsdyn4),-1)
plot(res1,res2)
abline(v=mean(res1, na.rm = TRUE), lty=2)
abline(h=mean(res2, na.rm = TRUE), lty=2)

res3 <- lag(resid(Bitcoin2.tsdyn4),-2)
plot(res1,res3)
abline(v=mean(res1, na.rm = TRUE), lty=2)
abline(h=mean(res3, na.rm = TRUE), lty=2)

library(lmtest)
a <- bgtest(Bitcoin2.tsdyn4, order=1, type="F", fill=0)
b <- bgtest(Bitcoin2.tsdyn4, order=1, type="F", fill=NA)
c <- bgtest(Bitcoin2.tsdyn4, order=4, type="Chisq", fill=0)
d <- bgtest(Bitcoin2.tsdyn4, order=4, type="Chisq", fill=NA)

dfr <- data.frame(rbind(a[c(1,2,4)], b[c(1,2,4)], c[c(1,2,4)], d[c(1,2,4)]))
dfr <- cbind(c("1, F, 0", "1, F, NA", "4, Chisq, 0", "4, Chisq, NA"), dfr)
names(dfr)<-c("Method", "Statistic", "Parameters", "p-Value")
kable(dfr, caption="Breusch-Godfrey test for the Bitcoin-ARDL model no 4")

library(dLagM)
auto.arima(weekly2$transactions)
transactions.ts <- ts(weekly2$transactions)
transactions.arima <- arima(transactions.ts,order = c(0,1,2))
transactions_forecast <- forecast::forecast(transactions.arima, h=5, level = c(95))
transactions_forecast
auto.arima(weekly2$SP)
SP.ts <- ts(weekly2$SP)
SP.arima <- arima(SP.ts,order = c(0,1,1))
SP_forecast <- forecast::forecast(SP.arima, h=5, level = c(95))
SP_forecast

#ARDL Forecast using dLagM package  #we cannot use this for a dynlm object, so I have re-estimated the same model using ardlDlm(), this is supported by the dLagM::forecast() function.

diff.ts <- cbind(diff(weekly2$price,1),diff(weekly2$transactions,1),diff(weekly2$SP))   #diff of all three variables.  
View(diff.ts)

rem.p = list(X2 = c(3) , X3 = c(1,2,3))   # p denotes the x lags(for all x variables) and q denotes the AR part, so to remove some select lags we can use this remove parameter.
rem.q = c(2)                             # I have given some buffer values to avoid dY.t-1 getting removed and similar for x variables as well.
remove = list(p = rem.p , q = rem.q)
view(remove)
model.ardlDlm  = ardlDlm(formula = X1 ~ X2 + X3, data = data.frame(diff.ts) ,   #data should be a dataframe not anyother vector type
                         p = 3 , q = 2 , 
                         remove = remove)                            #as there is no parameter to directly calculate differences I have given the differences data itself as the variables.

x.new =  matrix(c(-3866.7, 32.7, -1505.9, 0,0,0), ncol = 3, nrow = 2)     #these are the differences of predicted values from ARIMA.
dLagM::forecast(model = model.ardlDlm , x = x.new , h = 4,interval = TRUE, nSim = 100)
