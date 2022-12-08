# 11 sectors of S&P 500
# Information Technology - XLK
# Health Care - XLV
# Financials - XLF
# Consumer Discretionary - XLY
# Communication Services - XLC
# Industrials - XLI
# Consumer Staples - XLP
# Energy - XLE
# Utilities - XLU
# Real Estate - XLRE
# Materials - XLB

# Get data from Yahoo Finance since January 1, 2019
library(quantmod)
getSymbols("XLK",src="yahoo",from="2019-05-03")
getSymbols("XLV",src="yahoo",from="2019-05-03")
getSymbols("XLF",src="yahoo",from="2019-05-03")
getSymbols("XLY",src="yahoo",from="2019-05-03")
getSymbols("XLC",src="yahoo",from="2019-05-03")
getSymbols("XLI",src="yahoo",from="2019-05-03")
getSymbols("XLP",src="yahoo",from="2019-05-03")
getSymbols("XLE",src="yahoo",from="2019-05-03")
getSymbols("XLU",src="yahoo",from="2019-05-03")
getSymbols("XLRE",src="yahoo",from="2019-05-03")
getSymbols("XLB",src="yahoo",from="2019-05-03")

# Create data frames
information_technology = as.data.frame(XLK)
health_care = as.data.frame(XLV)
financials = as.data.frame(XLF)
consumer_discretionary = as.data.frame(XLY)
communication_services = as.data.frame(XLC)
industrials = as.data.frame(XLI)
consumer_staples = as.data.frame(XLP)
energy = as.data.frame(XLE)
utilities = as.data.frame(XLU)
real_estate = as.data.frame(XLRE)
materials = as.data.frame(XLB)

# Convert row name to date column and only keep closing price

# XLK - Information Technology
tmp = as.data.frame(information_technology)
tmp$Date = row.names(tmp);tail(tmp)
tmp$XLK_information_technology = tmp$XLK.Close;tail(tmp)
row.names(tmp) = NULL
Stocks = tmp[,c('Date','XLK_information_technology')]
tail(Stocks)

# add 10 other etf's to stock dataframe
Stocks$XLV_health_care = health_care$XLV.Close
Stocks$XLF_financials = financials$XLF.Close
Stocks$XLY_consumer_discretionary = consumer_discretionary$XLY.Close
Stocks$XLC_communication_services = communication_services$XLC.Close
Stocks$XLI_industrials = industrials$XLI.Close
Stocks$XLP_consumer_staples = consumer_staples$XLP.Close
Stocks$XLE_energy = energy$XLE.Close
Stocks$XLU_utilities = utilities$XLU.Close
Stocks$XLRE_real_estate = real_estate$XLRE.Close
Stocks$XLB_materials = materials$XLB.Close

head(Stocks,3)
dim(Stocks)

# Load in Bitcoin Data from NASDAQ
bitcoin = read.csv("C:/Users/Joseph/Desktop/ECNS 491/Final Project/HistoricalData_1668019166502.csv")

# convert bitcoin date to y-m-d
bitcoin$newdate = strptime(as.character.Date(bitcoin$Date), "%m/%d/%Y")
head(bitcoin,3)
bitcoin$Date = as.character(bitcoin$newdate);head(bitcoin,3)

# Only keep date and close price for bitcoin
bitcoin = bitcoin[,c('Date','Close.Last')];head(bitcoin,3)

# merge stock and bitcoin data frames
nrow(Stocks)
nrow(bitcoin)

# new data frame should same number of rows at stock data as bitcoin trades 7 days a week
library(dplyr)
df = Stocks |> left_join(bitcoin, by=('Date'))
df$Date = as.Date(df$Date) # convert to date object
colnames(df)[13] = "Bitcoin"
nrow(df) == nrow(Stocks) #True

# unit test for duplicate entries
dups_stocks = Stocks |> count(Date) |> filter(n>1)
stopifnot(nrow(dups_stocks)==0)

dups_bitcoin = bitcoin |> count(Date) |> filter(n>1)
stopifnot(nrow(dups_bitcoin)==0)

# data visualization
df |> as_tibble()
library(ggplot2)
p = ggplot(df, aes(x=Date, y=XLK_information_technology,col='XLK: Information Technology')) + geom_line() +
  geom_line(aes(x=Date,y = XLV_health_care,col = "XLV: Health Care"))+
  geom_line(aes(x=Date,y=XLF_financials,col='XLF: Financials'))+
  geom_line(aes(x=Date,y=XLY_consumer_discretionary,col='XLY: Consumer Discretionary')) +
  geom_line(aes(x=Date,y=XLC_communication_services,col='XLC: Communication Services')) +
  geom_line(aes(x=Date,y=XLI_industrials,col='XLI: Industrials')) +
  geom_line(aes(x=Date,y=XLP_consumer_staples,col='XLP: Consumer Staples')) +
  geom_line(aes(x=Date,y=XLE_energy,col='XLE: Energy')) +
  geom_line(aes(x=Date,y=XLU_utilities,col='XLU: Utilities')) +
  geom_line(aes(x=Date,y=XLRE_real_estate,col='XLRE: Real Estate')) +
  geom_line(aes(x=Date,y=XLB_materials,col='XLB: Materials')) +
  ylab('Price')

# grid of individual graphs
library(gtable)
library(grid)
library(gridExtra)
library(lattice)
p1 = ggplot(df, aes(x=Date, y=XLK_information_technology,col='XLK: Information Technology')) + geom_line(col=1)
p2 = ggplot(df,aes(x=Date,y = XLV_health_care,col = "XLV: Health Care"))+ geom_line(col=2)
p3 = ggplot(df,aes(x=Date,y=XLF_financials,col='XLF: Financials'))+ geom_line(col=3)
p4 = ggplot(df,aes(x=Date,y=XLY_consumer_discretionary,col='XLY: Consumer Discretionary'))+ geom_line(col=4)
p5 = ggplot(df,aes(x=Date,y=XLC_communication_services,col='XLC: Communication Services'))+ geom_line(col=5)
p6 = ggplot(df,aes(x=Date,y=XLI_industrials,col='XLI: Industrials'))+ geom_line(col=6)
p7 = ggplot(df,aes(x=Date,y=XLP_consumer_staples,col='XLP: Consumer Staples'))+ geom_line(col=7)
p8 = ggplot(df,aes(x=Date,y=XLE_energy,col='XLE: Energy'))+ geom_line(col=8)
p9 = ggplot(df,aes(x=Date,y=XLU_utilities,col='XLU: Utilities'))+ geom_line(col=9)
p10 = ggplot(df,aes(x=Date,y=XLRE_real_estate,col='XLRE: Real Estate'))+ geom_line(col=10)
p11 = ggplot(df,aes(x=Date,y=XLB_materials,col='XLB: Materials'))+ geom_line(col=11)
p12 = ggplot(df,aes(x=Date,y=Bitcoin,col='Bitcoin'))+ geom_line(col=12)


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,nrow=4,top="Time Series Plots of Each Sector ETF and Bitcoin")


df2 = df

XLK_fd = diff(df$XLK_information_technology)
XLV_fd = diff(df$XLV_health_care)
XLF_fd = diff(df$XLF_financials)
XLY_fd = diff(df$XLY_consumer_discretionary)
XLC_fd = diff(df$XLC_communication_services)
XLI_fd = diff(df$XLI_industrials)
XLP_fd = diff(df$XLP_consumer_staples)
XLE_fd = diff(df$XLE_energy)
XLU_fd = diff(df$XLU_utilities)
XLRE_fd = diff(df$XLRE_real_estate)
XLB_fd = diff(df$XLB_materials)
Bitcoin_fd = diff(df$Bitcoin)
df2 = as.data.frame(cbind(XLV_fd, XLK_fd,XLF_fd,XLY_fd,XLC_fd,XLI_fd,XLP_fd,XLE_fd,XLU_fd,XLRE_fd,XLB_fd,Bitcoin_fd))
df2$XLV_fd = XLV_fd
df2[is.na(df2)] = 0
#correlation table
cor(df2)

#scatter plots of daily price changes
BitcoinvsXLK = ggplot(df2, aes(x=Bitcoin_fd, y = XLK_fd)) + geom_point(col=1) + geom_smooth(method = 'lm')
BitcoinvsXLK

BitcoinvsXLV = ggplot(df2, aes(x=Bitcoin_fd, y=XLV_fd)) + geom_point(col=2) + geom_smooth(method = 'lm')
BitcoinvsXLV

BitcoinvsXLF = ggplot(df2, aes(x=Bitcoin_fd, y=XLF_fd)) + geom_point(col=3) + geom_smooth(method = 'lm')
BitcoinvsXLF

BitcoinvsXLY = ggplot(df2, aes(x=Bitcoin_fd, y = XLY_fd)) + geom_point(col=4) + geom_smooth(method = 'lm')
BitcoinvsXLY

BitcoinvsXLC = ggplot(df2, aes(x=Bitcoin_fd, y=XLC_fd)) + geom_point(col=5) + geom_smooth(method = 'lm')
BitcoinvsXLC

BitcoinvsXLI = ggplot(df2, aes(x=Bitcoin_fd, y=XLI_fd)) + geom_point(col=6) + geom_smooth(method = 'lm')
BitcoinvsXLI

BitcoinvsXLP = ggplot(df2, aes(x=Bitcoin_fd, y = XLP_fd)) + geom_point(col=7) + geom_smooth(method = 'lm')
BitcoinvsXLP

BitcoinvsXLE = ggplot(df2, aes(x=Bitcoin_fd, y=XLE_fd)) + geom_point(col=8) + geom_smooth(method = 'lm')
BitcoinvsXLE

BitcoinvsXLU = ggplot(df2, aes(x=Bitcoin_fd, y=XLU_fd)) + geom_point(col=9) + geom_smooth(method = 'lm')
BitcoinvsXLU

BitcoinvsXLRE = ggplot(df2, aes(x=Bitcoin_fd, y = XLRE_fd)) + geom_point(col=10) + geom_smooth(method = 'lm')
BitcoinvsXLRE

BitcoinvsXLB = ggplot(df2, aes(x=Bitcoin_fd, y = XLB_fd)) + geom_point(col=11) + geom_smooth(method = 'lm')
BitcoinvsXLB

grid.arrange(BitcoinvsXLP,BitcoinvsXLB,BitcoinvsXLC,BitcoinvsXLE,BitcoinvsXLF,BitcoinvsXLI,BitcoinvsXLK,BitcoinvsXLRE,BitcoinvsXLU,BitcoinvsXLV,BitcoinvsXLY,nrow=4,top="Scatter Plots of Daily Price Changes")

#correlation plot
library(corrplot)
corrplot(cor(df2))

histXLK = ggplot(df2, aes(x=XLK_fd, fill = "XLK"))+geom_density(col = 1)+scale_fill_manual(values = 1)
histXLK
histXLV = ggplot(df2, aes(x=XLK_fd, fill = "XLV"))+geom_density(col = 2)+scale_fill_manual(values = 2)
histXLV
histXLF = ggplot(df2, aes(x=XLF_fd, fill = "XLF"))+geom_density(col = 3)+scale_fill_manual(values = 3)
histXLF
histXLY = ggplot(df2, aes(x=XLK_fd, fill = "XLY"))+geom_density(col = 4)+scale_fill_manual(values = 4)
histXLY
histXLC = ggplot(df2, aes(x=XLC_fd, fill = "XLC"))+geom_density(col = 5)+scale_fill_manual(values = 5)
histXLC
histXLI = ggplot(df2, aes(x=XLI_fd, fill = "XLI"))+geom_density(col = 6)+scale_fill_manual(values = 6)
histXLI
histXLP = ggplot(df2, aes(x=XLP_fd, fill = "XLP"))+geom_density(col = 7)+scale_fill_manual(values = 7)
histXLP
histXLE = ggplot(df2, aes(x=XLE_fd, fill = "XLE"))+geom_density(col = 8)+scale_fill_manual(values = 8)
histXLE
histXLU = ggplot(df2, aes(x=XLK_fd, fill = "XLU"))+geom_density(col = 9)+scale_fill_manual(values = 9)
histXLU
histXLRE = ggplot(df2, aes(x=XLK_fd, fill = "XLRE"))+geom_density(col = 10)+scale_fill_manual(values = 10)
histXLRE
histXLB = ggplot(df2, aes(x=XLK_fd, fill = "XLB"))+geom_density(col = 11)+scale_fill_manual(values = 11)
histXLB
histBitcoin = ggplot(df2, aes(x=Bitcoin_fd, fill = "XLV"))+geom_density(col = 12)+scale_fill_manual(values = 12)
histBitcoin

grid.arrange(histBitcoin,histXLB,histXLC,histXLE,histXLF,histXLI,histXLK,histXLP,histXLRE,histXLU,histXLV,histXLY,nrow=4,top="Density Plots of Daily Price Changes")
