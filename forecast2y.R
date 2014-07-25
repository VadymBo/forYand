
library(forecast)

## download data and reorder values
images<-read.csv("YaImages.csv", sep=';', header =F)
images<-images[nrow(images):1,]

weather<-read.csv("YaWeather.csv", sep=';', header =F)
weather<-weather[nrow(weather):1,]



## function tp make forecast
prognoz<-function(data, level=95)
{
  ## args
  ##  data - vector of input values
  ##  level - confidence intevals
  ##
  
  ## returns:
  ##    progn - list of timesries with input, forecast, confidence values
  ##    also build a plot
  ##
  
  progn<-list()
    
  t.series<-ts(data = data,start = c(2008,5) ,frequency = 12)
  model<-stlf(x = t.series,h = 24,method = "arima", level=level)
  
  progn$data= t.series
  progn$pred = model$mean
  progn$lower = ts(model$lower,start = c(2014,5), frequency=12)
  progn$upper = ts(model$upper,start = c(2014,5), frequency=12)
  
  plot(progn$data, xlim=c(2008,2017), ylim=c(0, max(progn$upper)), ylab= "unique users per month", col="grey", lwd=3) +
    lines(progn$lower, col="red", lwd=1)+
    lines(progn$pred, col="blue", lwd=2)+
    lines(progn$upper, col="red", lwd=1)
    legend(x = 2008,y=max(progn$upper), legend = c("data","forecast", "95% confidence int"), col=c('grey','blue','red'), lwd=c(3,2,1))
  
  progn
}

im.forecast=prognoz(images[,2])
we.forecast=prognoz(weather[,2])


