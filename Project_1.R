Time series analysis on Singapur labor participation rate
#INTRODUCTION - labor force participants in singapore -work with data containing a trend 
#project2-deal with seasonality - model comparison(cross validation, ARIMA based model)
#project3-scraping and analyzing stock data - analyze irregular data 

##Main R functions in analysis 
###R base
####decompose() stl() arima() holtwinters() acf() pacf() plot() ts()
###Forecast package

##required packages ------
library(forecast)
library(ggplot2)
library(quantmod)
library(xts)
#function forecast+standard model(ARIMA,Seasonale decomposition, exponential smoothing, simple models) = forecasting
##ARIMA model - auto.arima(time_series)
##model comparison - accuracy() tsCV()
##data prep - na.locf()

#Unemployement rate
#labor force participation rate

#step1 import the data by scanning and press enter twice no timestamps
singapur=scan()
print(singapur)
view(si)
#convert to time series 
singapur = ts(data = singapur,start=1980)

plot(singapur,ylab = "Labour Force in Singapore")
#mission statement
##create linear trend model with holt() with and without damping parameter ------ HOLT linear model 
holttrend 

##compare plots of all three models 
###fitted value for models against each other 
###simple exponential smoothing: ses()
###holt's linear trend model: holt()+damped
  ###Y=l+h*b
###holt-winters seasonal method: hw()
###automated exponential smoothing: ets()

holttrend = holt(singapur, h= 5) 
summary(holttrend)
plot(holttrend)
###with damped argument to limit the curve from going to infinity 
holt()+damped 
#phi auto generated 
holtdamped = holt(singapur, h=15, damped=T)
plot(holt(singapur, h=15, damped=T)) ##choose the best curve for you to show a curve that is more round 
summary(holt(singapur, h=15, damped=T))

plot(holt(singapur, h=15, damped=T, phi = 0.8)) ##wider 

##ARIMA model ------ get the best fit and forecast 
##Arima() ------- manual parameter selection
##auto.arima() ------- automatic parameter selection
singapurarima <- auto.arima(singapur)
summary(singapurarima)
plot(forecast(singapurarima, h=5))
###do not recommend h=55 because the time span is too long which make the prediction less accurate
plot(forecast(singapurarima, h=55))

##Exact calculation of arima parameters to produce different arima model 
auto.arima(singapur, stepwise = F, approximation = F)

#use ggplot and forecast to visualize the result 
##simple visualizations with R base
###overview plot - models 
holttrend <- holt(singapur, h=10)
holtdamped <- holt(singapur, h=10, damped = T)
arimafore <- forecast(auto.arima(singapur), h=10)
###use functions autoplot() + autolayer()
autoplot(singapur) + 
  forecast::autolayer(holttrend$mean,series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean,series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") + ##need autolayer for each model 
  xlab("year") +
  ylab("Labour Force Participation Rate Age 25-54") +
  guides(colour = guide_legend(title="Forecast Method")) +
  theme(legend.position = c(0.8,0.2))+   ##where to put the legend 
  ggtitle("Singapur") +  ##add the title 
  theme(plot.title = element_text(family = "Times", hjust=0.5, color = "blue", face = "bold", size=15)) 
  
#in sample forecast vs actual data -----
##what is fitted data? 
holttrend <- holt(singapur, h=10)
holtdamped <- holt(singapur, h=10, damped = T)
arimafore <- forecast(auto.arima(singapur), h=10)

autoplot(singapur) + 
  forecast::autolayer(holttrend$fitted,series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$fitted,series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$fitted, series = "ARIMA") + ##need autolayer for each model 
  xlab("year") +
  ylab("Labour Force Participation Rate Age 25-54") +
  guides(colour = guide_legend(title="Forecast Method")) +
  theme(legend.position = c(0.8,0.2))+   ##where to put the legend 
  ggtitle("Singapur") +  ##add the title 
  theme(plot.title = element_text(family = "Times", hjust=0.5, color = "blue", face = "bold", size=15)) 

