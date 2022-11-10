#apple aapl
#visa v
#walmart wmt
#exxon mobil xom
#johnson & johnson jnj

library(quantmod)
#Stock data from Yahoo Finance
getSymbols("AAPL",src="yahoo",from="2019-01-01")
getSymbols("V",src="yahoo",from="2019-01-01")
getSymbols("WMT",src="yahoo",from="2019-01-01")
getSymbols("XOM",src="yahoo",from="2019-01-01")
getSymbols("JNJ",src="yahoo",from="2019-01-01")

AAPL=as.data.frame(AAPL)
V=as.data.frame(V)
WMT=as.data.frame(WMT)
XOM=as.data.frame(XOM)
JNJ=as.data.frame(JNJ)

head(AAPL,2);tail(AAPL,2)
head(V,2);tail(V,2)
head(WMT,2);tail(WMT,2)
head(XOM,2);tail(XOM,2)
head(JNJ,2);tail(JNJ,2)

dim(AAPL);dim(V);dim(WMT);dim(XOM);dim(JNJ)

#Bitcoin data from Nasdaq
BTC_data = read.csv("C:/Users/Joseph/Desktop/ECNS 491/HistoricalData_1668019166502.csv")
head(BTC_data,2);tail(BTC_data,2)
dim(BTC_data)

add.julian=function(df0){
  df0$Date=row.names(df0)
  df0$jday=as.numeric(julian(as.Date(df0$Date,"%Y-%m-%d")))
  df0
}
#Create new DF of only closing price and date for Apple
tmp=as.data.frame(AAPL); tail(tmp)
tmp$Date=row.names(tmp); tmp=add.julian(tmp); tmp$Apple=tmp$AAPL.Close 
row.names(tmp)=NULL; APPLE=tmp[,c('jday','Apple')]
head(APPLE,3); tail(APPLE,3)

#Create new DF of only closing price and date for Visa
tmp=as.data.frame(V); tail(tmp)
tmp$Date=row.names(tmp); tmp=add.julian(tmp); tmp$Visa=tmp$V.Close 
row.names(tmp)=NULL; Visa=tmp[,c('jday','Visa')]
head(Visa,3); tail(Visa,3)

#Create new DF of only closing price and date for Walmart
tmp=as.data.frame(WMT); tail(tmp)
tmp$Date=row.names(tmp); tmp=add.julian(tmp); tmp$Walmart=tmp$WMT.Close 
row.names(tmp)=NULL; Walmart=tmp[,c('jday','Walmart')]
head(Walmart,3); tail(Walmart,3)

#Create new DF of only closing price and date for Exxon Mobil
tmp=as.data.frame(XOM); tail(tmp)
tmp$Date=row.names(tmp); tmp=add.julian(tmp); tmp$Exxon=tmp$XOM.Close 
row.names(tmp)=NULL; Exxon=tmp[,c('jday','Exxon')]
head(Exxon,3); tail(Exxon,3)

#Create new DF of only closing price and date for Johnson & Johnson
tmp=as.data.frame(JNJ); tail(tmp)
tmp$Date=row.names(tmp); tmp=add.julian(tmp); tmp$Johnson=tmp$JNJ.Close 
row.names(tmp)=NULL; Johnson=tmp[,c('jday','Johnson')]
head(Johnson,3); tail(Johnson,3)


library(lubridate)
#Create new DF of only closing price and for Bitcoin
#convert date column to type date and then change date format to y-m-d
BTC_data$newdate = strptime(as.character.Date(BTC_data$Date), "%m/%d/%Y")
head(BTC_data,3)
BTC_data$newdate = as.Date.character(BTC_data$newdate)

#Create new DF of only closing price and for Bitcoin
tmp=as.data.frame(BTC_data); tail(tmp)
tmp$Date=BTC_data$newdate; row.names(tmp) = BTC_data$newdate; tmp=add.julian(tmp); tmp$Bitcoin=tmp$Bitcoin.Close.Last 
row.names(tmp)=NULL; Bitcoin=tmp[,c('jday','Close.Last')]
Bitcoin$Bitcoin = Bitcoin$Close.Last
Bitcoin = Bitcoin[,c('jday','Bitcoin')]
head(Bitcoin,3); tail(Bitcoin,3)

#merge all data frames by julian day

dfA = merge(Walmart, Bitcoin)
dfB = merge(dfA, Exxon)
dfC = merge(dfB, Johnson)
dfD = merge(dfC, APPLE)
df1 = merge(dfD, Visa)
summary(df1)

#first difference data to find difference in daily closing prices
apple_fd = diff(df1$Apple)
jnj_fd = diff(df1$Johnson)
visa_fd = diff(df1$Visa)
walmart_fd = diff(df1$Walmart)
exxon_fd = diff(df1$Exxon)
bitcoin_fd = diff(df1$Bitcoin)

julian_day = df1$jday[2:879]
df2 = as.data.frame(cbind(julian_day, apple_fd, jnj_fd, visa_fd, walmart_fd, exxon_fd, bitcoin_fd))
summary(df2)

df2 = subset(df2, julian_day>18050)

df2$bitcoin_fd_scaled = (df2$bitcoin_fd)/750 #scale down bitcoin price changes to fit same scale as stocks

plot(df2$julian_day ,df2$bitcoin_fd_scaled,type = 'l',col=1,main = 'Daily Price Changes',xlab = 'julian days',ylab = 'price in $USD')
lines(df2$julian_day ,df2$apple_fd,type='l',col=2)
lines(df2$julian_day ,df2$bitcoin_fd_scaled)
legend('topleft',legend=c('Bitcoin','Apple'),fill=c(1,2))

plot(df2$julian_day ,df2$bitcoin_fd_scaled,type = 'l',col=1,main = 'Daily Price Changes',xlab = 'julian days',ylab = 'price in $USD')
lines(df2$julian_day ,df2$jnj_fd,type='l',col=2)
lines(df2$julian_day ,df2$bitcoin_fd_scaled)
legend('topleft',legend=c('Bitcoin','Johnson & Johnson'),fill=c(1,2))

plot(df2$julian_day ,df2$bitcoin_fd_scaled,type = 'l',col=1,main = 'Daily Price Changes',xlab = 'julian days',ylab = 'price in $USD')
lines(df2$julian_day ,df2$visa_fd,type='l',col=2)
lines(df2$julian_day ,df2$bitcoin_fd_scaled)
legend('topleft',legend=c('Bitcoin','Visa'),fill=c(1,2))

plot(df2$julian_day ,df2$bitcoin_fd_scaled,type = 'l',col=1,main = 'Daily Price Changes',xlab = 'julian days',ylab = 'price in $USD')
lines(df2$julian_day ,df2$walmart_fd,type='l',col=2)
lines(df2$julian_day ,df2$bitcoin_fd_scaled)
legend('topleft',legend=c('Bitcoin','Walmart'),fill=c(1,2))

plot(df2$julian_day ,df2$bitcoin_fd_scaled,type = 'l',col=1,main = 'Daily Price Changes',xlab = 'julian days',ylab = 'price in $USD')
lines(df2$julian_day ,df2$exxon_fd,type='l',col=2)
lines(df2$julian_day ,df2$bitcoin_fd_scaled)
legend('topleft',legend=c('Bitcoin','Exxon Mobil'),fill=c(1,2))

plot(df1$jday, df1$Apple, type='l',col=1,main='Stock Prices', xlab ='Julian Days', ylab='price in  in $USD',asp=2)
lines(df1$jday,df1$Walmart,type='l', col=2)
lines(df1$jday, df1$Exxon,type='l',col=3)
lines(df1$jday, df1$Visa,type='l',col=4)
lines(df1$jday, df1$Johnson,type='l', col=5)
legend('topleft',legend=c('Apple','Walmart','Exxon','Visa', 'Johnson & Johnson'),fil=c(1,2,3,4,5))

#test correlations between individual stocks and Bitcoin
cor(df2$bitcoin_fd,df2$apple_fd)
cor(df2$bitcoin_fd,df2$jnj_fd)
cor(df2$bitcoin_fd,df2$visa_fd)
cor(df2$bitcoin_fd,df2$walmart_fd)
cor(df2$bitcoin_fd,df2$exxon_fd)

#highest correlation between Bitcoin and Walmart, lowest between Bitcoin and Johnson & Johnson