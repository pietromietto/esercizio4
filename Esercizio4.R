# MSF 2020, Homework n. 3
# Data loading


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load the dataset 
data.indeces <- read.csv("International_Indexes_CSV.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
dates        <- as.Date(data.indeces$Date, format = "%d/%m/%Y")
n            <- nrow(data.indeces)
p            <- ncol(data.indeces)
indeces      <- data.indeces[,2:p]
head(date)
head(indeces)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Build the dataset 
StockInd <- data.frame(
  day    = dates[-1], 
  indeces[-1,],
  log(indeces)[-1,],
  log(indeces[2:n,]) - log(indeces[1:(n-1),])
)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Build the dataset of log-returns
StockIndLogRet <- data.frame(
  day = dates[-1], 
  log(indeces[2:n,]) - log(indeces[1:(n-1),])
)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Build the dataset of log-returns (percentage)
StockIndLogRet100 <- data.frame(
  day = dates[-1], 
  (log(indeces[2:n,]) - log(indeces[1:(n-1),])) * 100
)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
# Save in RData format
save(StockInd, file = "InternStockInd.RData")
save(StockIndLogRet, file = "InternStockIndLogRet.RData")
save(StockIndLogRet100, file = "InternStockIndLogRet100.RData")



### ESERCIZIO 4                      ###
### METODI STATISTICI PER LA FINANZA ###
### GRUPPO 8                         ###

# seleziono colonna 8 e la rendo serie storica: JAPDOWA
japdowa<-zoo(indeces[,"JAPDOWA"], dates)
head(japdowa)
tail(japdowa)

# Punto 1 
# calcolo i log dei prezzi 
log_japdowa<-log(japdowa)
head(log_japdowa)
tail(log_japdowa)
#grafico dei log dei prezzi
a<-as.timeSeries(log_japdowa)
seriesPlot(a, title=F)
title("Log dei prezzi giornalieri di JAPDOWA", 
      sub="Serie da 2002-12-31 a 2019-04-05", cex.sub=0.75)
# statistiche più importanti dei prezzi
summary(log_japdowa)
round(basicStats(log_japdowa), 3)
# calcolo l'ACF  PACF
par(mfrow=c(2,1))
acf(a)
pacf(a)
par(mfrow=c(1,1))

#prova













