library(fImport)
install.packages("backtest",dependecies=T)
library(backtest)
library(zoo)
library(FinTS)
library(forecast)
library(ggplot2)
library(fGarch)
library(lmtest)
library(rugarch)


data<-fredSeries("DEXUSEU",from="1999-01-04",to="2020-05-01")
data<-as.zoo(data)
dates<-as.Date(index(data))
dex<-as.numeric(data)
n<-length(dex)
dexuseu<- data.frame(
  day = dates[-1],
  value = dex[-1], 
  lvalue = log(dex)[-1],
  lret = log(dex[2:n]) - log(dex[1:(n-1)]),
  lret.sq = (log(dex[2:n]) - log(dex[1:(n-1)]))^2,
  lret.abs = abs(log(dex[2:n]) - log(dex[1:(n-1)])))  

attach(dexuseu)

##Punto 1. Media condizionata
ggplot(data=dexuseu, aes(x=day, y=value)) + geom_line(color="deeppink2") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels="%d %b %Y") + labs(title = "DEXUSEU", subtitle="Serie da 04-01-1999 a 01-05-2020", x = "time", y = "index")
#Dal grafico si nota una evidente non stazionarietà della serie considerata. Presenta un trend crescente dal 2002 al 2008, mentre dopo 
#la crisi del 2008 il trend diventa pressocchè decrescente.
require(gridExtra)
plot1<-ggAcf(value, type = c("correlation"),lag.max=100) +labs(title = "ACF", x = "lag", y = "ACF")
plot2<-ggPacf(value,lag.max=100) + labs(title = "PACF", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#L'ACF conferma la presenza di dipendenza temporale e la necessità di differenziare la serie.
ggplot(data=dexuseu, aes(x=day, y=lret)) + geom_line(color="deeppink2") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels="%d %b %Y") + labs(title = "Log-returns di DEXUSEU", subtitle="Serie da 04-01-1999 a 01-05-2020", x = "time", y = "index")
# statistiche più importanti dei log rendimenti
summary(lret)
round(basicStats(lret), 4)
#possiamo notare che il valore atteso del rendimento è zero.
#la serie risulta essere leggermente asimmetrica a destra, inoltre le code della distribuzione sono poco più pesanti della Normale.
#possiamo verificarlo anche guardando al grafico quantile-quantile:
qqnormPlot(lret)
#Analizziamo il correlogramma:
require(gridExtra)
plot1<-ggAcf(lret, type = c("correlation"),lag.max=1000) +labs(title = "ACF dei log-returns", x = "lag", y = "ACF")
plot2<-ggPacf(lret,lag.max=1000) + labs(title = "PACF dei log-returns", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#Si notano alcuni ritardi significativi che si protraggono a lungo nel tempo e che non sembrano seguire un particolare
#pattern che ci possa far pensare ad una stagionalità.
#Quest'effetto è dovuto con tutta probabilità all'informazione non modellata circa l'eteroschedasticità, nonostante 
#stiamo considerando la serie al grado 1.




#Punto 2. Varianza condizionata
#Dal grafico dei log-returns del DEXUSEU è evidente il fenomeno del volatility clustering, segno della presenza di eteroschedasticità.
#valutiamo quindi i log-rendimenti al quadrato e i in valore assoluto:
ggplot(data=dexuseu, aes(x=day, y=lret.sq)) + geom_line(color="deeppink2") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels="%d %b %Y") + labs(title = "Log-returns^2", subtitle="Serie da 04-01-1971 a 01-05-2020", x = "time", y = "index")

plot1<-ggAcf(lret.sq, type = c("correlation"),lag.max=1000) +labs(title = "ACF dei log-returns^2", x = "lag", y = "ACF")
plot2<-ggPacf(lret.sq,lag.max=1000) + labs(title = "PACF dei log-returns^2", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#si nota una forte dipendenza nella volatilità dei rendimenti
AutocorTest(lret.sq)
ArchTest(lret.sq)

ggplot(data=dexuseu, aes(x=day, y=lret.abs)) + geom_line(color="deeppink2") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels="%d %b %Y") + labs(title = "abs(Log-returns)", subtitle="Serie da 04-01-1971 a 01-05-2020", x = "time", y = "index")
plot1<-ggAcf(lret.abs, type = c("correlation"),lag.max=1000) +labs(title = "ACF dei abs(log-returns)", x = "lag", y = "ACF")
plot2<-ggPacf(lret.abs,lag.max=1000) + labs(title = "PACF dei abs(log-returns)", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#anche valutando il valore assoluto, il correlogramma mostra dipendenza.
AutocorTest(lret.abs) 
ArchTest(lret.abs)
#è evidente la presenza di effetti ARCH sia sulla serie dei log-rendimenti al quadrato che in valore assoluto.



#Punto 3.
skewness(lret)
kurtosis(lret)
#come accennato al punto precedente, la serie presenta una leggera asimmetria a destra.
#l'eccesso di curtosi positivo inoltre indica una distribuzione leptocurtica.
#possiamo verificare la deviazione dalla Normale guardando al grafico quantile-quantile e ad alcuni test di normalita':
qqnormPlot(lret)
#i dati si discostano dalla bisettrice in particolare sulla coda destra.
ksnormTest(lret)
jarqueberaTest(lret)
dagoTest(lret)
#proviamo ad adattare un modello sulla distribuzione:
(fit.norm<-nFit(lret))
(fit.std<-stdFit(lret)) #t di Student
(fit.ged<-gedFit(lret)) #GED
(fit.sstd<-sstdFit(lret)) #t-Student asimmetrica
(fit.sged<-sgedFit(lret)) #GED asimmetrica
(fit.nig<-nigFit(lret,trace=F)) #NIG
N<-length(lret)
#calcoliamo i valori del criterio di Akaike per ogni distribuzione.
(aic.norm<-2*fit.norm@fit$minimum + 2*2/N)
(aic.std<-2*fit.std$objective + 2*3*N)
(aic.ged<-2*fit.ged$objective + 2*3*N)
(aic.sstd<-2*fit.sstd$minimum + 2*4*N)
(aic.sged<-2*fit.sged$objective + 2*4*N)
(aic.nig<-2*fit.nig@fit$objective + 2*4*N)
which.min(c(aic.norm,aic.std,aic.ged,aic.sstd,aic.sged,aic.nig))
#viene preferito il secondo modello inserito, ossia una distribuzione student-t
#densita' empirica a confronto con quella teorica:
ggplot(data=dexuseu, aes(x=lret)) + geom_histogram(aes(y=..density..),bins = 100, col="white", fill="steelblue") +
  geom_density(col=3,lwd=1) + theme(axis.text.x=element_text(angle=0, hjust=1, size=16), axis.text.y=element_text(size = 16), title = element_text(size=20)) +
  labs(title="Confronto tra densita' empirica  e teorica (std)", x="rendimenti", y="Frequenza") +
  stat_function(fun=dstd,args=list(mean=fit.std$par[1],sd=fit.std$par[2],nu=fit.std$par[3]),col=2,alpha=0.7,lwd=1)
#confronto ora la funzione di ripartizione empirica con la vera funzione di ripartizione:
plot(sort(lret),(1:N/N),main="Probabilita'",col="steelblue4")
curve(pstd(x,mean=fit.std$par[1],sd=fit.std$par[2],nu=fit.std$par[3]),add=T,col=2,lwd=2)




#Punto 4.
################arima(0,0)
spec1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=T),
                  distribution.model="std")
fit1<-ugarchfit(spec1,data=lret)
show(fit1) 
spec1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=F),
                  distribution.model="std")
fit1<-ugarchfit(spec1,data=lret)
show(fit1)
spec1<-ugarchspec(variance.model=list(model="sGARCH",submodel="GARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=T),
                  distribution.model="std")
fit1<-ugarchfit(spec1,data=lret)
show(fit1) #alpha1+beta1 =~ 1 
spec2<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=T),
                  distribution.model="std",fixed.pars=list(omega=0))
fit2<-ugarchfit(spec2,data=lret)
show(fit2)
spec3<-ugarchspec(variance.model=list(model="iGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=T),
                  distribution.model="std")
fit3<-ugarchfit(spec3,data=lret)
show(fit3)
spec4<-ugarchspec(variance.model=list(model="iGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=T),
                  distribution.model="std")
fit4<-ugarchfit(spec4,data=lret)
show(fit4)
which.min(c(infocriteria(fit1)[1],infocriteria(fit2)[1],infocriteria(fit3)[1],infocriteria(fit4)[1]))
which.min(c(infocriteria(fit1)[2],infocriteria(fit2)[2],infocriteria(fit3)[2],infocriteria(fit4)[2]))
which.min(c(infocriteria(fit1)[3],infocriteria(fit2)[3],infocriteria(fit3)[3],infocriteria(fit4)[3]))
which.min(c(infocriteria(fit1)[4],infocriteria(fit2)[4],infocriteria(fit3)[4],infocriteria(fit4)[4]))


plot(fit1,which=9)
#l'adattamento della std sui residui, come gia' visto, e' molto buono.
plot(fit1,which=10)
plot(fit1,which=11)
#dalle funzioni di ACF sui residui standardizzati e sui residui standardizzati al quadrato confermano che 
#non c'è traccia di dipendenza.
persistence(fit1)
halflife(fit1)
#l'halflife sul modello sGARCH(2,1) e' molto elevato: dopo 643 giorni la varianza ritorna alla sua media
persistence(fit4)
halflife(fit5)
#ovviamente l'halflife per l'igarch e' log(1) = -Inf
plot(sigma(fit4))
plot(fit4,which="all")



#Punto 5.
lret2<-lret[-((n-49):n)]
fit1.2<-ugarchfit(spec1,lret2)
fit4.2<-ugarchfit(spec4,lret2)
n2<-length(lret2)
prev1<-ugarchforecast(fit1.2,n.ahead=50,n.roll=0)
prev4<-ugarchforecast(fit4.2,n.ahead=50,n.roll=0)
plot(prev1@forecast$seriesFor,type="l")
plot(prev1@forecast$sigmaFor,type="l")
plot(prev4@forecast$seriesFor,type="l")
plot(prev4@forecast$sigmaFor,type="l")
plot(prev1,which=1)
plot(prev1,which=3)
plot(prev4,which=1)
plot(prev4,which=3)



#Punto 6.
#Calcoliamo i limiti inferiore e superiore dell'intervallo di confidenza al livello del 5%
lim.inf1<-prev1@forecast$seriesFor-2*prev1@forecast$sigmaFor
lim.sup1<-prev1@forecast$seriesFor+2*prev1@forecast$sigmaFor
lim.inf4<-prev4@forecast$seriesFor-2*prev1@forecast$sigmaFor
lim.sup4<-prev4@forecast$seriesFor+2*prev1@forecast$sigmaFor

plot(lret2,xlim=c(min(index(lret2)),max(index(lret2))+100),ylim=c(min(lret2),max(lret2+.1)))
lines(max(index(lret2))+1:50,prev1@forecast$seriesFor,col=2)
lines(max(index(lret2))+1:50,lim.inf1,col=3)
lines(max(index(lret2))+1:50,lim.sup1,col=3)
plot(lret2,xlim=c(min(index(lret2)),max(index(lret2))+100),ylim=c(min(lret2),max(lret2+.1)))
lines(max(index(lret2))+1:50,prev4@forecast$seriesFor,col=2)
lines(max(index(lret2))+1:50,lim.inf4,col=3)
lines(max(index(lret2))+1:50,lim.sup4,col=3)


lret.zoo<-zoo(lret,order.by=dates)
plot.zoo(lret.zoo,xlim=c(as.Date("2020-01-01"),max(index(lret.zoo))+50))
lines(max(index(lret.zoo))+c(1:50),fitted(prev1),col=2)
lines(max(index(lret.zoo))+c(1:50),fitted(prev4),col=2)
#le previsioni sono le stesse
#vediamo gli intervalli di previsione:
lines(max(index(lret.zoo))+c(1:50),fitted(prev1)+2*sigma(prev1),col=2,lty=3)
lines(max(index(lret.zoo))+c(1:50),fitted(prev1)-2*sigma(prev1),col=2,lty=3)
lines(max(index(lret.zoo))+c(1:50),fitted(prev4)+2*sigma(prev4),col=3,lty=3)
lines(max(index(lret.zoo))+c(1:50),fitted(prev4)-2*sigma(prev4),col=3,lty=3)






#Punto 7.
alpha<-c(0.01,0.05,0.10,0.90,0.95,0.99)
rollwin<-2000
n<-length(lret)
#metodo rolling-window
alfa.q<-qnorm(1-alpha)
Var1<-rep(0,n)
Var2<-rep(0,n)
Var3<-rep(0,n)
Var4<-rep(0,n)
Var5<-rep(0,n)
Var6<-rep(0,n)
for(j in (rollwin+1):n) 
{
  mean<-mean(lret[(j-rollwin):(j-1)])
  rend.m<-lret[j-1]-mean
  mod.garch<-ugarchfit(spec=spec1,data=lret[(j-rollwin):(j-1)]-mean,solver="hybrid")
  vol<-mod.garch@fit$coef[1]+mod.garch@fit$coef[2]*(rend.m^2)+mod.garch@fit$coef[3]*(mod.garch@fit$sigma[rollwin]^2)
  Var1[j]<-mean+sqrt(vol)*alfa.q[1]
  Var2[j]<-mean+sqrt(vol)*alfa.q[2]
  Var3[j]<-mean+sqrt(vol)*alfa.q[3]
  Var4[j]<-mean+sqrt(vol)*alfa.q[4]
  Var5[j]<-mean+sqrt(vol)*alfa.q[5]
  Var6[j]<-mean+sqrt(vol)*alfa.q[6]
  cat("processing obs ",j,"\n")
}







plot(lret[-(1:rollwin)],type="l")
lines(Var1,col=2) #tutte sovrapposte
lines(Var2,col=3) #tutte sovrapposte
lines(Var3,col=4) #tutte sovrapposte
lines(Var4,col=5) #tutte sovrapposte
lines(Var5,col=6) #tutte sovrapposte
lines(Var6,col=7) #tutte sovrapposte


dexuseu<-cbind(dexuseu,Var1,Var2,Var3,Var4,Var5,Var6)
names(dexuseu)[7:12]<-c("VaR01.rw","VaR05.rw","VaR10.rw","VaR90.rw","VaR95.rw","VaR99.rw")
save(dexuseu,file="dexuseu2.RData")


ggplot()+
  geom_line(aes(x=dexuseu$day[(rollwin+1):n],y=dexuseu$lret[(rollwin+1):n],color="blue"),size=0.15,alpha=0.3) +
  geom_line(aes(x=dexuseu$day[(rollwin+1):n],y=dexuseu$Var1[(rollwin+1):n], color="red"), alpha=1.4, size=0.35) +
  geom_line(aes(x=dexuseu$day[(rollwin+1):n],y=dexuseu$Var2[(rollwin+1):n], color = " black "), alpha = 1.4 , size = 0.35) +
  theme(axis.text.x=element_text(angle = 0, hjust = 1, size = 16), axis.text.y=element_text(size = 16), title=element_text(size = 20)) +
  scale_color_manual( name = " Series ",
                      breaks = c(" blue ", " red ", " black "),
                      values = c(" blue " = " steelblue ", "red" = " red ", " black "= " black "),
                      labels = c("log -- returns ", " VaR1 ", " VaR2 ")) +
  theme(legend.title=element_text(color = " black ", size = 14),legend.text=element_text(color = " black ", size = 14)) +
  labs(title = "DEX US EU from 1999-01-04 to 2020-05-01", x = " ", y = " ") +
  scale_x_date(date_labels = "%Y %b %d")



