library(readxl)
library(forecast)
covid_us <- read_excel("E:/DataFromBuddy/christopher/covid_us.xlsx", sheet = "uk")
attach(covid_us)
view(covid_us)
library(ggplot2)
library(scales)
#geom_smooth(method = "lm",se = FALSE, col = "red")
ggplot(data = covid_us) +
  aes(x = as.Date(date), y = new_cases) + geom_line()+
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("2 month"))
attach(covid_us)
library(zoo)
# Change the data to zoo type
covid_us_us <- covid_us[,c(1,2)]
covid_us_us
dat_demo <- zoo(covid_us$new_cases, seq(from = as.Date("2020-01-31"), to = as.Date("2021-03-21"), by = 1))
class(dat_demo)
summary(dat_demo)
plot(dat_demo)
# Make the data stationary, by differencing the data.
stationary_data <- diff(dat_demo)
plot(stationary_data)
library(tseries)
# To check if it's stationary we conduct a quantitative test. We use the Augmented Dickey-Fuller Test.
# H_0 = The null hypothesis for this test is that there is a unit root.
# H_A = The alternative hypothesis is that the time series is stationary (or trend-stationary).
adf.test(as.matrix(stationary_data)) #-- ?
# We select a significance level of 0.05 and since our p-value is 0.01 and smaller then 0.05, we come to the conclusion to reject the null hypothesis. In other words our data is stationary.
# We use the Auto Correlation Graph
# First we have a look at our acf graph when our data isn't stationary.
acf(dat_demo)
# Here we see that our values are all exceeding the blue line. The goal is to have the values under the blue line and they should be inverted as well.
# To select the p and q values we select the number before the first inverted line.
acf(stationary_data)
pacf(stationary_data)
plot(stationary_data)
# arima has a auto.arima function which gives us the ideal arima model based on our data.
arima_funct <- auto.arima(stationary_data)
arima_funct
# lets use the auto.arima function to forecast 3 months
forecast1 <- forecast(arima_funct, h=90)
additional_cases <- round(sum(forecast1$upper[,2]),0)
additional_cases
total_number_of_cases <- round(sum(covid_us_us$new_cases)+additional_cases,0)
total_number_of_cases
plot(forecast1)
accuracy(forecast1)
delta <- (forecast1$lower[,1]+forecast1$upper[,1])/2
delta
predicted_30 <- data.frame(date=seq(from=18708,by=1,length=90),new_cases=delta)
predicted_30
original <- data.frame(date=as.numeric(covid_us_us$date),new_cases=c(0,stationary_data))
original
z<- rbind(original,predicted_30)
z$date <- as.Date(z$date)
forecast1_data<- as.data.frame(forecast1)
forecast1_data<- data.frame(date=seq(from=18708,by=1,length=90),forecast1_data, y = delta)
forecast1_data$date <- as.Date(forecast1_data$date)
graph<- ggplot(data=z,aes(x=date,y=new_cases),show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_line(data=original,aes(x=as.Date(date),y=new_cases))+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.95, ymax =Hi.95), inherit.aes = FALSE,fill = "lightsteelblue2")+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.80, ymax =Hi.80), inherit.aes = FALSE,fill = "lightsteelblue3")+
  geom_line(data=forecast1_data,aes(x=date,y=y),size=1,color='purple')+
  ggtitle("Forecasts from ARIMA(3,0,2) with zero mean")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x="Date",y="New Casess")
graph

