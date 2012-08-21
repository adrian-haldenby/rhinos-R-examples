#enter the path to your data file here
csv_path <- "~/BrowserWeekly.csv"

#import the data to datafram
data<-read.csv(csv_path, header=TRUE,stringsAsFactors= FALSE)

#convert to weekly time series
data.browser.ts <-ts(data=data[,1], frequency = 52, start=c(2010,1))
data.mobile.ts <-ts(data=data[,2], frequency = 52, start=c(2010,1))

#apply multiplicative holt winters
data.browser.hw <- HoltWinters(data.browser.ts,seasonal="multi")
data.mobile.hw <- HoltWinters(data.mobile.ts, seasonal="multi")

#predict 2 years ahead
data.browser.predict <- predict(data.browser.hw, n.ahead=104)
data.mobile.predict <- predict(data.mobile.hw, n.ahead=104)

#add the predictions together to get total prediction
data.total.ts <- data.browser.ts+data.mobile.ts
data.total.predict <- data.browser.predict+data.mobile.predict 

#use time series plot
ts.plot( data.mobile.ts, data.mobile.predict, data.browser.ts, data.browser.predict, data.total.ts, data.total.predict, lty=c(1:2), col=c(3,3,2,2,4,4),ylab="Visits", main="Mobile & Big Browser Forcasts")
#add legend
legend("topleft",legend=c("Mobile","Mobile Forcasted","Big Browser","Big Browser Forcasted", "Total Visits", "Total Visits Forcasted"),lty=c(1:2),col=c(3,3,2,2,4,4))