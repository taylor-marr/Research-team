# Forecasting examples 

install.packages("forecast")
library(forecast)

getwd()
sales <- read.csv("./../../PRTeam Analysis/Nielson Project/demand-institute-cities-mls-data-monthly.csv")

bay_area = sales[sales$Place.Id %in% c(17151, 17420, 13654) & sales$State == "California",]
head(bay_area)

img <- readPNG("./Redfin-Logo-Web.png", FALSE)
my_g <- grobTree(rasterGrob(img, interpolate=TRUE, x=1, hjust=1))

require(graphics)
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
plot(gnp) # using 'plot.ts' for time-series plot

plot(gnp_fc) <- forecast(gnp)
gnp_fc$mean

fit <- HoltWinters(WWWusage,gamma=FALSE)
plot(forecast(fit))

## Seasonal Holt-Winters
(m <- HoltWinters(co2))
plot(m)
plot(fitted(m))

(m <- HoltWinters(AirPassengers, seasonal = "mult"))
plot(m)

## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = FALSE)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)

bay_area <- bay_area[bay_area$City == "San Francisco",]

demand <- ts(bay_area[,10], start = c(2010, 1), frequency = 12)
plot(demand)

hw <- HoltWinters(demand)
plot(hw)

forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)

library(ggplot2)
# install.packages("reshape")
library(reshape)


HWplot<-function(ts_object,  n.ahead=12,  CI=.95,  error.ribbon='red', line.size=1){
    
    hw_object<-HoltWinters(ts_object)
    
    forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
    
    
    for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
    
    fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
    
    actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
    
    
    graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
    graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
    graphset[is.na(graphset$dev),  ]$dev<-0
    
    graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
    
    
    graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
    
    p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) + geom_vline(xintercept = max(actual_values$time),  lty=2) + ylab('Value') + scale_colour_redfin() +theme_redfin()
    return(p)
    
}

HWplot(demand) + ggtitle("San Francisco Median Sale Prices of Homes") + ylab("")
+ geom_bar()
    
(gg <- ggplotly(p))

q<-ggplot(bay_area, aes(x = as.Date(Month), y = `Median.Sale.Price`/1000, color = City)) +
    geom_smooth(span = 0.5) +
    ylab("Med Sale Price in Thousands") +
    xlab("") +
    ggtitle(paste(bay_area$State[1])) +
    theme_redfin() +
    scale_colour_redfin()

grid.arrange(arrangeGrob(q),my_g, heights=c(9,.35))

q1<-ggplot(demand, aes(x = time, y = `Median.Sale.Price`/1000, color = City)) +
    geom_point() +
    geom_line(size =1) +
    ylab("Med Sale Price in Thousands") +
    xlab("") +
    ggtitle(paste(bay_area$State[1])) +
    theme_redfin() +
    scale_colour_redfin()
grid.arrange(arrangeGrob(q1),my_g, heights=c(9,.35))

q2<-ggplot(bay_area, aes(x = as.Date(Month), y = `Homes.Sold`, color = City)) +
    geom_line(size = 1.2) +
    geom_point() + 
    geom_point(size = .6, color = "white") + 
    ggtitle(paste(bay_area$State[1])) +
    theme_redfin() +
    scale_colour_redfin()
grid.arrange(arrangeGrob(q2),my_g, heights=c(9,.35))

q3<-ggplot(bay_area, aes(x = as.Date(Month), y = `Median.Days.On.Market`, color = City)) +
    geom_line() +
    geom_point(size= 1.2) + 
    ggtitle(paste(bay_area$State[1])) +
    theme_redfin() +
    scale_colour_redfin()
grid.arrange(arrangeGrob(q3),my_g, heights=c(9,.35))

q4<-ggplot(bay_area, aes(x = as.Date(Month), y = `Inventory`, color = City)) +
    geom_point() +
    geom_bar(aes(y=`Homes.Sold`))
    ggtitle(paste(bay_area$State[1])) +
    theme_redfin() 
    # scale_colour_redfin()
grid.arrange(arrangeGrob(q4),my_g, heights=c(9,.35))
