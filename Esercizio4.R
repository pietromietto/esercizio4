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
















