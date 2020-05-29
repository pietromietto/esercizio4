### MSF_Homework 3 ###
### ESERCIZIO 4    ###

# carico librerie utili per l'analisi

library(ggplot2)
library(forecast)
library(fBasics)
library(FinTS)
library(fGarch)
library(lmtest)
library(rugarch)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load the dataset 
data.indeces <- read.csv("International_Indexes_CSV.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
dates<- as.Date(data.indeces$Date, format = "%d/%m/%Y")
n<- nrow(data.indeces)
p<- ncol(data.indeces)
indeces<- data.indeces[,2:p]
head(dates)
head(indeces)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# seleziono colonna 8 e la rendo serie storica: JAPDOWA
japdowa<-indeces[,"JAPDOWA"]
# costruzione del data frame
jap.data <- data.frame(
  day = dates[-1],
  value = japdowa[-1], 
  lvalue = log(japdowa)[-1],
  lret = log(japdowa[2:n]) - log(japdowa[1:(n-1)]),
  lret.sq = (log(japdowa[2:n]) - log(japdowa[1:(n-1)]))^2,
  lret.abs = abs(log(japdowa[2:n]) - log(japdowa[1:(n-1)]))
)  

save(jap.data, file = "JAPDOWA.RData")
head(jap.data)
attach(jap.data)

# Punto 1

head(lvalue)
tail(lvalue)
# grafico dei log dei prezzi
ggplot(data = jap.data, aes(x = day, y = lvalue)) + geom_line(color="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(date_labels = "%d %b %Y") + 
  labs(title = "Prezzi logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")

# statistiche più importanti dei prezzi
summary(lvalue)
round(basicStats(lvalue), 4)

# calcolo l'ACF  PACF
require(gridExtra)
plot1<-ggAcf(lvalue, type = c("correlation")) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lvalue) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)

# Punto 2 

# analisi dei rendimenti logaritmici 
ggplot(data = jap.data, aes(x = day, y = lret)) + geom_line(color="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + 
  labs(title = "Rendimenti logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")

# calcolo l'ACF  PACF
plot1<-ggAcf(lret,type = c("correlation"), lag.max = 100) + 
  labs(title = "ACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lret, lag.max = 100) + 
  labs(title = "PACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)

# modelliamo l'evoluzione temporale in media della serie tramite un modello ARMA
# usiamo auto.arima per una ricerca del modello di tipo step-wise
auto.arima(lret)

# proviamo ad adattare un modello ARMA(1,1) sui rendimenti logaritmici--> (ARIMA(1,1,1) su jap.data$value)
model.m<-Arima(lret,order=c(1,0,1), include.drift=T,include.mean=T)
coeftest(model.m)

# l'intercetta e il drift non sono significativi 
model.m<-arima(lret,order=c(1,0,1), include.mean=F)
coeftest(model.m)
# componenti ar e ma sono invece altamente significative

# calcolo ACF e PACF sui residui
plot1<-ggAcf(residuals(model.m),type = c("correlation"), lag.max = 100) +
  labs(title = "ACF residui ARMA", x = "lag", y = "ACF")
plot2<-ggPacf(residuals(model.m), lag.max = 100) + 
  labs(title = "PACF residui ARMA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)

# calcolo indice di asimmetria emprico
(sk<-mean(lret^3)/(sd(lret))^3)

# calcolo indice di curtosi
(k<-mean(lret^4)/(var(lret))^2)

# calcolo test di normalita'
ksnormTest(lret)
jarqueberaTest(lret)
dagoTest(lret)

# considero i residui del mio modello
res.m<-residuals(model.m)
res.m<-as.numeric(res.m)
jap.data <- cbind(jap.data, res.m)
head(jap.data)

# rappresento i residui del mio modello ARMA
ggplot(data = jap.data, aes(x = day, y = res.m)) + geom_line(color="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(date_labels = "%d %b %Y") + 
  labs(title = "Residui ARMA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")


res.m.sq<-res.m^2 # residui al quadrato
res.m.abs<-abs(res.m) # valore assoluto dei residui
jap.data <- cbind(jap.data, res.m.sq, res.m.abs)
# analisi grafica per verificare eteroschedasticita'
# rappresento residui al quadrato e valore assoluto dei residui 
plot1<-ggplot(data = jap.data, aes(x = day, y = res.m.sq)) + geom_line(color="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(date_labels = "%d %b %Y") + 
  labs(title = "Residui ARMA al quadrato", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
plot2<-ggplot(data = jap.data, aes(x = day, y = res.m.abs)) + geom_line(color="steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(date_labels = "%d %b %Y") + 
  labs(title = "Residui ARMA in valore assoluto", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
grid.arrange(plot1, plot2, nrow=2)

# rappresento ACF e PACF 
plot1<-ggAcf(res.m.sq,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.m.sq, lag.max =100) + 
  labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
plot3<-ggAcf(res.m.abs,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF residui in valore assoluto", x = "lag", y = "ACF")
plot4<-ggPacf(res.m.abs, lag.max =100) + 
  labs(title = "PACF dei residui in valore assoluto", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)


# test per la presenza della componente eteroschedastica
# sulla serie
ArchTest(lret)
# sui residui
ArchTest(res.m)

# test che verifica la parte in media stimata
AutocorTest(res.m) 
AutocorTest(res.m.sq) 
AutocorTest(res.m.abs) 

# Punto 3 
#proviamo a trovare un'opportuna distribuzione di probabilita' per lret
class(lret)
lret.ss<-as.timeSeries(lret)
# verifichiamo se può assumere una distribuzione Normale in modo grafico
densityPlot(lret.ss)
ggplot(data = jap.data, aes(sample = lret)) + stat_qq(col="steelblue") + 
  stat_qq_line(col=2) + theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 12)) + 
  labs(title = "qqplot per i rendimenti logaritmici JAPDOWA ", x = "theoretical quantile", y = "sample quantile")

# adottiamo altre distribuzioni
(fit.norm<-nFit(lret.ss)) #Normale
(fit.std<-stdFit(lret.ss)) #t di Student
(fit.ged<-gedFit(lret.ss)) #GED
(fit.sstd<-sstdFit(lret.ss)) #t-Student asimmetrica
(fit.sged<-sgedFit(lret.ss)) #GED asimmetrica
(fit.nig<-nigFit(lret.ss,trace=F)) #NIG

N<-length(lret.ss)
# calcoliamo i valori del criterio di Akaike per ogni distribuzione
# sceglieremo la distribuzione che minimizza tale criterio
(aic.std<-2*fit.std$objective + 2*3*N)
(aic.ged<-2*fit.ged$objective + 2*3*N)
(aic.sstd<-2*fit.sstd$minimum + 2*4*N)
(aic.sged<-2*fit.sged$objective + 2*4*N)
(aic.nig<-2*fit.nig@fit$objective + 2*4*N)
which.min(c(aic.std,aic.ged,aic.sstd,aic.sged,aic.nig)) 
# viene preferito il secondo modello inserito, ossia una distribuzione ged
fit.ged

# densita' empirica a confronto con quella teorica
ggplot(data=jap.data, aes(x = lret)) + geom_histogram(aes(y=..density..),bins = 100, col = "white", fill = "steelblue") +
  geom_density(col=3,lwd=1) + theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 16), axis.text.y = element_text(size = 16), title = element_text(size = 20)) + 
  labs(title = "Confronto tra densità empirica  e teorica (ged)", x = "rendimenti", y = "Frequenza") + 
  stat_function(fun=dged,args=list(mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),col=2,alpha=0.7,lwd=1)
# confronto ora la funzione di ripartizione empirica con la vera funzione di ripartizione
ggplot(data=jap.data, aes(x=sort(lret))) + 
  geom_point(aes(x=sort(lret),y=(1:N/N)), col="steelblue", alpha = 1.4)+
  stat_function(fun=pged,args=list(mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),col=2,alpha=0.7,lwd=1)+
  labs(title="Confronto tra CDF empirica e teorica (ged)", x="rendimenti", y="frequenze cumulate ") +
  theme(axis.text.x=element_text(angle=0, hjust=1, size=9), axis.text.y=element_text(size = 9), 
        title = element_text(size=15))
# Punto 4
#verificare l’ipotesi della presenza di asimmetria della distribuzione Skew Student–t

########################
fit.t<-garchFit(~garch(1,1), data=lret, cond.dist="sstd",trace=F,include.mean=T)
summary(fit.t)
res<-fit.t@residuals
sigma2<-fit.t@sigma.t
res.std<-res/sigma2
res.std2<-res.std^2
#######################

# calcolo l'ACF e PACF
plot1<-ggAcf(res.std,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF dei residui standardizzati", x = "lag", y = "ACF")
plot2<-ggPacf(res.std, lag.max=100) + 
  labs(title = "PACF dei residui standardizzati", x = "lag", y = "PACF")
plot3<-ggAcf(res.std2,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF dei residui al quadrato", x = "lag", y = "ACF")
plot4<-ggPacf(res.std2, lag.max=100) + 
  labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

a<-res.std2[-1] #tolgo la prima osservazione
b<-res[-length(res)] #tolgo l'ultima osservazione
length(a)
length(b)
# applichiamo un criterio di diagnostica 
cor(res[-1]^2,b) # negativa, quindi c'e' asimmetria

# applichiamo anche dei test di simmetria (Engle-Ng)
# generiamo la variabile dummy
d=numeric(length(b)) 
for(i in 1:length(b)){
  if (b[i]<0) d[i]=1
}
# Sign test
fit.lm=lm(a~1+d) 
summary(fit.lm)

D=d*b
# Size-sign test
fit.lm=lm(a~1+D) 
summary(fit.lm)

# Test congiunto
fit.lm=lm(a~1+d+D) 
summary(fit.lm)

# Punto 5 

# Punto 6 

# stimiamo un processo GARCH-M sui log-rendimenti ipotizzando, come visto al punto precedente,
# assumiamo che processo generatore dei dati provengano da una ged 
spec.gm2<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1), include.mean=F, archm=T,archpow=2),
                     distribution.model="ged", fixed.pars = list(omega=0))
fit.gm2<-ugarchfit(spec.gm2,data=lret)
show(fit.gm2)

spec.gm1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,1),include.mean=F, archm=T, archpow=1),
                     distribution.model="ged", fixed.pars = list(omega=0))
fit.gm1<-ugarchfit(spec.gm1,data=lret)
show(fit.gm1)
# grafico tra errori al quadrato e valori osservati
plot(fit.gm1,which=7)

# Punto 7

# adattiamo un modello ARMA(1,1), senza intercetta
plot1<-ggAcf(res.m.sq,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.m.sq, lag.max =100) + 
  labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
plot3<-ggAcf(res.m.abs,type = c("correlation"), lag.max = 100) +
  labs(title = "ACF residui in valore assoluto", x = "lag", y = "ACF")
plot4<-ggPacf(res.m.abs, lag.max =100) + 
  labs(title = "PACF dei residui in valore assoluto", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

# modelliamo la varianza

# ARMA-GARCH con errori Normali
# ARMA(1,1)-ARCH(10)
spec1.n<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(10,0)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
fit1.n<-ugarchfit(spec1.n,lret)
show(fit1.n)
# ARMA(1,1)-GARCH(2,1)
spec2.n<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
fit2.n<-ugarchfit(spec2.n,lret)
show(fit2.n)
# ARMA(1,1)-GARCH(1,1)
spec3.n<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm", fixed.pars = list(omega=0))
fit3.n<-ugarchfit(spec3.n,lret)
show(fit3.n)

# calcolo criteri di adattamento e li confronto
infocriteria(fit1.n)
infocriteria(fit2.n)
infocriteria(fit3.n)
# scelgo l'ARMA(1,1)-GARCH(2,1)

# rappresento grafici dell'ACF e PACF dei residui standardizzati
par(mfrow=c(2,1))
plot1<-plot(fit2.n, which=10)
plot2<-plot(fit2.n, which=11)
par(mfrow=c(1,1))
# rappresento norm-qqplot 
plot(fit2.n, which=9)

############################

# ARMA-GARCH con errori Student-t
# ARMA(1,1)-GARCH(2,1)
spec1.t<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit1.t<-ugarchfit(spec1.t,lret)
show(fit1.t)
# ARMA(0,0)-GARCH(2,1)
spec2.t<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(0,0)),distribution.model="std")
fit2.t<-ugarchfit(spec2.t,lret)
show(fit2.t)
# ARMA(0,0)-GARCH(1,1)
spec3.t<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(0,0)),distribution.model="std")
fit3.t<-ugarchfit(spec3.t,lret) 
show(fit3.t)
# ARMA(0,1)-GARCH(1,1)
spec4.t<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(0,1)),distribution.model="std")
fit4.t<-ugarchfit(spec4.t,lret)
show(fit4.t)

# calcolo criteri di adattamento e li confronto
infocriteria(fit1.t)
infocriteria(fit2.t)
infocriteria(fit3.t)
infocriteria(fit4.t)
# molto simili tra loro, il migliore è ARMA(1,1)-GARCH(2,1)

# guardo ACF e PACF deiresidui standardizzati dei modelli
par(mfrow=c(2,1))
plot1<-plot(fit1.t, which=10)
plot2<-plot(fit1.t, which=11)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
plot1<-plot(fit2.t, which=10)
plot2<-plot(fit2.t, which=11)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
plot1<-plot(fit3.t, which=10)
plot2<-plot(fit3.t, which=11)
par(mfrow=c(1,1))
par(mfrow=c(2,1))
plot1<-plot(fit4.t, which=10)
plot2<-plot(fit4.t, which=11)
par(mfrow=c(1,1))
# dal confronto grafico scelgo ARMA(0,1)-GARCH(1,1)
# rappresento std-qqplot
plot(fit4.t, which=9)

# confronto finale tra quello nromale e quello Student-t
AIC.n<-infocriteria(fit2.n)[1]
AIC.t<-infocriteria(fit4.t)[1]
which.min(c(AIC.n,AIC.t))
# migliore sembrerebbe quello che assume distr. Student-t

# Punto 8

# previsioni col modello selezionato al punto 7
# elimino le ultime 20 oss
lret.2<-lret[-(4224:4244)]
fit.st2<-ugarchfit(spec4.t, lret.2)
prev<-ugarchforecast(fit4.t, n.ahead=20, n.roll=0)
vol.prev<-sigma(prev)
# faccio le previsioni 20 passi in avanti
plot(prev,which=1)
plot(prev,which=3)
# creo data frame per ggplot
# faccio le previsioni 20 passi in avanti
prev1<-data.frame(
  giorno<-day[(n-19):n],
  prev.garch<-prev@forecast$seriesFor,
  lim.inf.garch<-prev@forecast$seriesFor-2*prev@forecast$sigmaFor,
  lim.sup.garch<-prev@forecast$seriesFor+2*prev@forecast$sigmaFor
)
ggplot(data = jap.data[c(4200:n),], aes(x=day, y=lret)) + geom_line(color="steelblue", alpha=0.7) +
  geom_line(data=prev1, aes(x=giorno, y=prev.garch), col="springgreen4", alpha=0.7) + 
  geom_line(data=prev1, aes(x=giorno, y=lim.inf.garch), col="navyblue", lty=3) +
  geom_line(data=prev1, aes(x=giorno, y=lim.sup.garch), col="navyblue", lty=3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(date_labels="%d %b %Y") + 
  labs(title = "Intervalli di previsione", x = "time", y = "index")

rm(list=ls())



















