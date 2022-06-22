library(ggplot2)
library(lubridate)
library(FinTS)
btc_data <- read.csv("C:/Users/39324/Downloads/BTC-USD (2).csv", header = TRUE)
tnx_data <- read.csv("C:/Users/39324/Downloads/^TNX (1).csv", header = TRUE)

#sistemo prendo solo adj.price  date
btc_data <- btc_data[,c("Date","Adj.Close")]
btc_data
y <- c(!(weekdays(as.Date(btc_data$Date)) %in% c('sabato','domenica')) )
btc_data<- btc_data[y,]

tnx_data <- tnx_data[,c("Date","Adj.Close")]
tnx_data
f <- c(!(weekdays(as.Date(tnx_data$Date)) %in% c('sabato','domenica')) )
tnx_data<- tnx_data[f,]
tnx_data<-subset(tnx_data, tnx_data$Adj.Close!="null")
tnx_data$Adj.Close <- as.numeric(tnx_data$Adj.Close)
str(tnx_data)

#converto i dati nella colonna Date in oggetto data di entrambi i database e rappresento la serie storica
btc_data$Date<- ymd(btc_data$Date)
tnx_data$Date<- ymd(tnx_data$Date)

par(mfrow=c(2,1))
plot(btc_data,type="l",col=4, main="Grafico temporale della serie dei prezzi di chiusura di Bitcoin 
     da 4 Aprile 2020 al 01 Marzo 2022")
plot(tnx_data,type="l",col=4, main="Grafico temporale della serie dei prezzi di chiusura del rendimento a 10 anni Statunitense
     da 4 Aprile 2020 al 01 Marzo 2022")




#calcolo la prima differenza logaritmica sui ritorni di entrambi gli asset
m <-  diff(log(btc_data$Adj.Close), lag=1)
btc_data <- btc_data[-1,]
btc_data$Adj.Close <- m
names(btc_data)[2] <- "returns"

btc_data

z <-  diff(log(tnx_data$Adj.Close), lag=1)
tnx_data <- tnx_data[-1,]
tnx_data$Adj.Close <- z
names(tnx_data)[2] <- "returns"

tnx_data

#rapprenseto la prima differenza logaritmica di entrambi gli asset 

par(mfrow=c(2,1))

plot(btc_data$returns,type="l",col=4, main = "prima differenza della serie logaritmica naturale sui rendimenti di bitcoin ")
plot(tnx_data$returns,type="l",col=4, main = "prima differenza della serie logaritmica naturale sui rendimenti del Treasury a 10 anni  ")




#calcolo le statistiche univariate
descriptive_stat_btc <- FinTS.stats(btc_data$return)
descriptive_stat_tnx <- FinTS.stats(tnx_data$return)

descriptive_stat_btc
descriptive_stat_tnx

#jaq
jarque.bera.test(btc_data$return)

jarque.bera.test(tnx_data$return)

#rappresento la densità
dens_btc <- density(btc_data$return)
dens_Tnx <- density(tnx_data$return)

par(mfrow=c(2,1))
hist(btc_data$return,prob=T, breaks = 50, ylim=c(-1,max(dens_btc$y)),xlab="Btc returns" , main="istogramma e density line  della distibuzione dei rendimenti di Btc")
lines(density(btc_data$return), col="red")

hist(tnx_data$return,prob=T,breaks = 50,ylim=c(-1,max(dens_Tnx$y)),xlab="Tnx returns", main="istogramma e density line  della distibuzione dei rendimenti di Tnx")
lines(density(tnx_data$return),col="red")

#rappresento i qq plot
par(mfrow=c(2,1))
qqnorm(btc_data$return, main = "qq Bitcoin")
qqline(btc_data$return,col="red")

qqnorm(tnx_data$return,  main = "qq Tnx")
qqline(tnx_data$return,col="red")


#acf
par(mfrow=c(2,1))
acf(btc_data$return, main=" acf btc returns")
acf(tnx_data$returns, main=" acf tnx returns")
raw <- spectrum(btc_data$returns)
smooth <-  spectrum(btc_data$returns,spans=c(25,5,25),main="Smoothed periodogram",ylim=c(1e-5,2e-4))

#adf
library(tseries)

adf.test(btc_data$returns)
adf.test(tnx_data$returns)

#pearson correlation

corr_dataset <- merge(btc_data, tnx_data, by = "Date")
corr_dataset <- corr_dataset[,2:3]
names(corr_dataset) <- c("btc_returns","tnx_returns")
correlation <- cor(corr_dataset, method="pearson")


#granger causality
install.packages("lmtest")
library("lmtest")
grangertest(corr_dataset$btc_returns~corr_dataset$tnx_returns, order=3)
