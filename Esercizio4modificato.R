# MSF 2020, Homework n. 3
library(ggplot2)
library(forecast)
library(fBasics)
library(FinTS)
library(fGarch)
library(lmtest)
# Data loading

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load the dataset 
data.indeces <- read.csv(file.choose(), sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
dates<- as.Date(data.indeces$Date, format = "%d/%m/%Y")
n<- nrow(data.indeces)
p<- ncol(data.indeces)
indeces<- data.indeces[,2:p]
head(dates)
head(indeces)
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


### ESERCIZIO 4                      ###
### METODI STATISTICI PER LA FINANZA ###
### GRUPPO 8                         ###

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

# Punto 1.
# analisi descrittiva della serie dei prezzi logaritmici
head(lvalue)
tail(lvalue)
# grafico dei log dei prezzi
ggplot(data = jap.data, aes(x = day, y = lvalue)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Prezzi logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
# statistiche più importanti dei prezzi
summary(lvalue)
round(basicStats(lvalue), 4)
# calcolo l'ACF  PACF
require(gridExtra)
plot1<-ggAcf(lvalue, type = c("correlation")) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lvalue) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#########per salvare i grafici: 
#########pdf("grafico.pdf"); grid.arrange(plot1,plot2); dev.off()
#dal grafico risulta evidente che la serie dei prezzi logaritmici non sia stazionaria. In particolare la lenta decrescita della funzione di autocorrelazione 
#testimonia la forte dipendenza temporale.
#da questa serie non epurata dall'effetto in media è difficile fare considerazioni sulla stazionarietà in varianza, tuttavia anche questa non sembra costante 
#nel tempo, nonostante l'applicazione del logaritmo abbia sensibilmente ridotto il range di variazione dei dati, comprimendoli attorno al trend


#Punto 2.
#analisi dei rendimenti logaritmici 
ggplot(data = jap.data, aes(x = day, y = lret)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Rendimenti logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
# il fenomeno del volatility clustering risulta essere piuttosto evidente
t.test(lret)
#la media è uguale a zero con un intervallo di confidenza del 95%.
#Questo test si basa sull'assunzione di normalità.
ksnormTest(lret)
jarqueberaTest(lret)
dagoTest(lret)
#per tutti i test, si rifiuta l'ipotesi di normalità.
#Quindi questo test non è attendibile poichè in questo caso non è rispettata la normalità.
#Proviamo a verificare la stessa cosa guardando ai parametri di un modello ARMA:
#usiamo auto.arima per una ricerca del modello di tipo step-wise
auto.arima(lret)
#proviamo ad adattare un modello ARMA(1,1) sui rendimenti logaritmici, che quindi sono già stai differenziati una volta (ARIMA(1,1,1) per jap.data$value)
model1<-arima(lret,order=c(1,0,1), include.mean=T)
coeftest(model1)
#Come ci aspettavamo, l'intercetta del modello non è significativa. 
#Infatti differenziando la serie originaria dei prezzi l'abbiamo resa stazionaria in media.
(sk<-mean(lret^3)/(sd(lret))^3)
#indice di asimmetria empirico, nei rendimenti sembra esserci una leggera asimmetria a sinistra
(k<-mean(lret^4)/(var(lret))^2)
#indice di curtosi, K>3 dunque le code di questa distribuzione sono più pesanti di quelle della normale
plot1<-ggAcf(lret,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lret, lag.max = 100) + labs(title = "PACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#dalle funzioni di ACF e PACF si notano parecche autocorrelazioni significative che si protraggono nel tempo fino a lag piuttosto lontani
AutocorTest(lret)
#Il test box-ljung non rifiuta l'ipotesi per cui i dati sono distribuiti indipendentemente.
ArchTest(lret)
#Risultano esserci effetti ARCH
#Analisi dei residui e dei residui al quadrato:
res.m1<-model1$residuals
res.m1.sq<-res.m1^2
plot(res.m1,type="l")
plot1<-ggAcf(residuals(model1),type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui", x = "lag", y = "ACF")
plot2<-ggPacf(residuals(model1), lag.max =
                100) + labs(title = "PACF dei residui", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#La componente eteroschedastica è ancora presente.
plot1<-ggAcf(res.m1.sq,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.m1.sq, lag.max = 100) + labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
grid.arrange(plot1, plot2,nrow=2)
#C'è un'evidente struttura che si protrae a lungo. Ciò può essere dovuto al fatto che non tutta l'informazione è stata catturata in media
(fit1<-garchFit(~garch(10,0),data=lret,cond.dist="norm",trace=F))
#I parametri sono tutti significativi (infatti avevamo già notato la forte persistenza dei quadrati).
#Proviamo a rendere il modello più parsimonioso adattando un GARCH:
(fit2<-garchFit(~garch(2,1),data=lret,cond.dist="norm",trace=F))
(fit3<-garchFit(~garch(1,1),data=lret,cond.dist="norm",trace=F))
(fit4<-garchFit(~garch(1,1),data=lret,cond.dist="ged",trace=F))
#Guardando alla significatività dei parametri, si adatta meglio un GARCH(1,1)
N<-length(lret)
(aic.1<-2*fit1@fit$llh + 2*3*N)
(aic.2<-2*fit2@fit$llh + 2*3*N)
(aic.3<-2*fit3@fit$llh + 2*3*N)
(aic.4<-2*fit4@fit$llh + 2*3*N)
which.min(c(aic.1,aic.2,aic.3,aic.4))


#Punto 3.
#proviamo ad adattare alla distribuzione empirica dei rendimenti, una opportuna distribuzione di probabilità.
#par(mfrow=c(1,1))
class(lret)
lret.ss<-as.timeSeries(lret)
densityPlot(lret.ss)

(fit.norm<-nFit(lret.ss)) #Normale
(fit.std<-stdFit(lret.ss)) #t di Student
(fit.ged<-gedFit(lret.ss)) #GED
(fit.sstd<-sstdFit(lret.ss)) #t-Student asimmetrica
(fit.sged<-sgedFit(lret.ss)) #GED asimmetrica
(fit.nig<-nigFit(lret.ss,trace=F)) #NIG
N<-length(lret.ss)
#calcoliamo i valori del criterio di Akaike per ogni distribuzione.
#sceglieremo la distribuzione che minimizza tale criterio.
(aic.std<-2*fit.std$objective + 2*3*N)
(aic.ged<-2*fit.ged$objective + 2*3*N)
(aic.sstd<-2*fit.sstd$minimum + 2*4*N)
(aic.sged<-2*fit.sged$objective + 2*4*N)
(aic.nig<-2*fit.nig@fit$objective + 2*4*N)

which.min(c(aic.std,aic.ged,aic.sstd,aic.sged,aic.nig)) 
#scelgo il secondo modello inserito, ossia un ged.
#rappresento la densità empirica e la confronto con quella teorica:
ggplot(data=jap.data, aes(x = lret)) + geom_histogram(aes(y=..density..),bins = 100, col = "white", fill = "steelblue", alpha = 0.5) +
  geom_density(col=3,lwd=1) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 16), axis.text.y = element_text(size = 16), title = element_text(size = 20)) + labs(title = "Histogram for DJI log-returns", x = "DJI log-returns", y = "
Frequency") + stat_function(fun=dged,args=list(mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),col=2,lwd=1)
#confronto ora la funzione di ripartizione empirica con la vera funzione di ripartizione:
plot(sort(lret),(1:N/N),main="Probabilità",col="steelblue4")
curve(pged(x,mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),add=T,col=2,lwd=2)


#Punto 4.
fit.t<-garchFit(~garch(1,1),data=lret,cond.dist="sstd",trace=F,include.mean=F)
res<-fit.t@residuals
sigma2<-fit.t@sigma.t
res.std<-res/sigma2
res.std2<-res.std^2
#plot:
plot1<-ggAcf(res.std2,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.std2, lag.max=100) + labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
a<-res.std2[-1] #tolgo la prima osservazione
b<-res[-length(res)] #tolgo l'ultima osservazione
length(a)
length(b)
#queso test di diagnostica confronta la correlazione fra i residui non standardizzati al quadrato e i residui 
#non std al grado 1 ritardati di un passo. Una correlazione negativa mi fa presupporre che ci sia un effetto di leverage
cor(res[-1]^2,b) #negativa, quindi c'è asimmetria.


#Punto 5.  ####da verificare!
library(rugarch)
sk<-fit.t@fit$par[4]
sh<-fit.t@fit$par[5]
dkurtosis(distribution="sstd",skew=sk,shape=sh)
kurt<-function(d) (3*d^2)/((d-4)*(d-2))
kurt(8)
#poichè il valore della curtosi della skew student-t è diverso da quello della curtosi teorica
#per una skew t con d gradi di libertà, risulta che i gradi di libertà della distribuzione non siano 8.


#Punto 6.
#Nel processo GARCH-M, il termine delta*sigma2t rappresenta il premio per il rischio, che è variabile nel tempo.
spec.gm<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0),include.mean=TRUE,
                                  archm=T,archpow=2),distribution.model="ged")
fit.gm=ugarchfit(spec.gm,data=lret)
show(fit.gm)
plot(fit.gm,which="all")


#Punto 7.
#ARMA-GARCH con errori Normali:
spec.n<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                    mean.model=list(armaOrder=c(1,1)),
                    distribution.model="norm")
fit.n<-ugarchfit(spec.n,lret)
show(fit.n)
#analisi dei residui:
res<-residuals(fit.n)
plot1<-ggAcf(res,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui (errori Normali)", x = "lag", y = "ACF")
plot2<-ggPacf(res, lag.max=100) + labs(title = "PACF dei residui (errori Normali)", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
AutocorTest(res)
#i residui sono incorrelati nel tempo.
AIC.n<-infocriteria(fit.n)[1]

#ARMA-GARCH con errori Student-t:
spec.st<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(1,1)),
                  distribution.model="std")
fit.st<-ugarchfit(spec.st,lret)
show(fit.st)
#analisi dei residui:
res<-residuals(fit.st)
plot1<-ggAcf(res,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui (errori Student-t)", x = "lag", y = "ACF")
plot2<-ggPacf(res, lag.max=100) + labs(title = "PACF dei residui (errori Student-t)", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
AutocorTest(res)
#i residui sono incorrelati nel tempo.
AIC.t<-infocriteria(fit.st)[1]
which.min(c(AIC.n,AIC.t))
#secondo il criterio di Akaike, è migliore il secondo modello.



#Punto 8.
#previsioni col modello selezionato:
lret.2<-lret[-(4224:4244)]
fit.st2<-ugarchfit(spec.st,lret.2)
prev<-ugarchforecast(fit.st2,n.ahead=20,n.roll=0)
vol.prev<-sigma(prev)
plot(vol.prev,type="l")
plot(prev,which=1)
plot(prev,which=3)

par(mfrow=c(1,1))
plot(lret.2,xlim=c(min(index(lret.2)),max(index(lret.2))+20),ylim=c(min(lret.2),max(lret.2+.5)))
lines(max(index(lret.2))+1:20,prev@forecast$seriesFor,col=2)
lines(max(index(lret.2))+1:20,prev@forecast$seriesFor-2*prev@forecast$sigmaFor,col=3)
lines(max(index(lret.2))+1:20,prev@forecast$seriesFor+2*prev@forecast$sigmaFor,col=3)
