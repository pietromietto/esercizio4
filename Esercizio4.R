# MSF 2020, Homework n. 3
library(ggplot2)
library(forecast)
library(fBasics)
library(FinTS)
library(fGarch)
library(lmtest)
library(rugarch)
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
#analisi descrittiva della serie dei prezzi logaritmici
head(lvalue)
tail(lvalue)
# grafico dei log dei prezzi
ggplot(data = jap.data, aes(x = day, y = lvalue)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Prezzi logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
# statistiche più importanti dei prezzi
summary(lvalue)
round(basicStats(lvalue), 4)
#la distribuzione dei prezzi è essenzialmente simmetrica e con un eccesso di curtosi rispetto alle code della normale (k=3) negativo
# calcolo l'ACF  PACF
require(gridExtra)
plot1<-ggAcf(lvalue, type = c("correlation")) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lvalue) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#########per salvare i grafici: 
#########pdf("grafico.pdf"); grid.arrange(plot1,plot2); dev.off()

#dal grafico risulta evidente che la serie dei prezzi logaritmici non sia stazionaria in media. In particolare la lenta decrescita della funzione di autocorrelazione 
#testimonia la forte dipendenza temporale.
#da questa serie non epurata dall'effetto in media è difficile fare considerazioni sulla stazionarietà in varianza, tuttavia anche questa non sembra costante 
#nel tempo, nonostante l'applicazione del logaritmo abbia sensibilmente ridotto il range di variazione dei dati, comprimendoli attorno al trend


#Punto 2.
#analisi dei rendimenti logaritmici 
ggplot(data = jap.data, aes(x = day, y = lret)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Rendimenti logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
#tramite l'applicazione dell'operatore differenza prima la serie è stata resa stazionaria in media,
#infatti dal grafico la vediamo evolvere intorno allo zero in modo costante, senza particolari trend crescenti/decrescenti
#ci aspettiamo di trovare sia drift che intercetta non significativi

plot1<-ggAcf(lret,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "ACF")
plot2<-ggPacf(lret, lag.max = 100) + labs(title = "PACF dei rendimenti logaritmici JAPDOWA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#dalle funzioni di ACF e PACF si notano parecchie autocorrelazioni esterne alle bande di confidenza in lag piuttosto lontani (difficili da catturare con un modello)
#le prime autocorrelazioni risultano molto vicine alla soglia di significatività

#andiamo a modellare l'evoluzione temporale in media della serie tramite un modello ARMA
#usiamo auto.arima per una ricerca del modello di tipo step-wise
auto.arima(lret)
#proviamo ad adattare un modello ARMA(1,1) sui rendimenti logaritmici--> (ARIMA(1,1,1) su jap.data$value)
model.m<-Arima(lret,order=c(1,0,1), include.drift=T,include.mean=T)
coeftest(model.m)
#Come previsto, l'intercetta e il drift non sono significativi 
model.m<-arima(lret,order=c(1,0,1), include.mean=F)
coeftest(model.m)
#le componenti ar e ma sono invece altamente significative
plot1<-ggAcf(residuals(model.m),type = c("correlation"), lag.max = 100) +labs(title = "ACF residui ARMA", x = "lag", y = "ACF")
plot2<-ggPacf(residuals(model.m), lag.max = 100) + labs(title = "PACF residui ARMA", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, nrow=2)
#i primi lag sono stati ripuliti dalla componente in media

(sk<-mean(lret^3)/(sd(lret))^3)
#indice di asimmetria empirico, nei rendimenti sembra esserci una leggera asimmetria a sinistra
(k<-mean(lret^4)/(var(lret))^2)
#indice di curtosi, è K>3 dunque le code della distribuzione che descrive i rendimenti sono più pesanti di quelle della Normale
#il fatto che il processo da cui sono stati generati questi dati non presenti errori gaussiani è testimoniato dai test di normalità
ksnormTest(lret)
jarqueberaTest(lret)
dagoTest(lret)
#l'ipotesi di normalità è rifiutata con forza in tutti i casi

res.m<-residuals(model.m)
jap.data <- cbind(jap.data, res.m)
head(jap.data)

ggplot(data = jap.data, aes(x = day, y = res.m)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Residui ARMA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
#come dai rendimenti logaritmici, anche dai residui del modello per la media, il fenomeno del volatility clustering risulta piuttosto evidente
res.m.sq<-res.m^2 #residui al quadrato
res.m.abs<-abs(res.m)
jap.data <- cbind(jap.data, res.m.sq, res.m.abs)
plot1<-ggplot(data = jap.data, aes(x = day, y = res.m.sq)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Residui ARMA al quadrato", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
plot2<-ggplot(data = jap.data, aes(x = day, y = res.m.abs)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Residui ARMA in valore assoluto", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
grid.arrange(plot1, plot2, nrow=2)
plot1<-ggAcf(res.m.sq,type = c("correlation"), lag.max = 100) +labs(title = "ACF residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.m.sq, lag.max =100) + labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
plot3<-ggAcf(res.m.abs,type = c("correlation"), lag.max = 100) +labs(title = "ACF residui in valore assoluto", x = "lag", y = "ACF")
plot4<-ggPacf(res.m.abs, lag.max =100) + labs(title = "PACF dei residui in valore assoluto", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
#dai grafici ma sorattutto dalle ACF si nota un'evidente struttura che si protrae a lungo. 
#oltre alle analisi grafiche anche i test confermano la presenza della componente eteroschedastica
ArchTest(lret)
ArchTest(res.m)
#sia sui residui del modello ARMA che dai rendimenti logarirmici viene rifiutata con forza l'assenza di effetti ARCH
AutocorTest(res.m) 
#Il test box-ljung non rifiuta H0 di incorrelazione dei residui del modello
#Pertanto model.m ha modellato bene la media e i residusono incorrelati
AutocorTest(res.m.sq) 
AutocorTest(res.m.abs) 
#invece viene rifiutata con forza l'ipotesi di incorrelazione dei residui al quadrato e in valore assoluto


#Punto 3.
#proviamo ad adattare alla distribuzione empirica dei rendimenti, un'opportuna distribuzione di probabilita'
class(lret)
lret.ss<-as.timeSeries(lret)
densityPlot(lret.ss)
ggplot(data = jap.data, aes(sample = lret)) + stat_qq(col="steelblue") + stat_qq_line(col=2) 
+ theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 20)) + labs(title = "qqplot per i rendimenti logaritmici JAPDOWA ", x = "theoretical quantile", y = "sample quantile")
#come visto al punto precedente la distribuzione empirica è leggermente asimmetrica a sinistra e ha code molto più pesanti della normale

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
#viene preferito il secondo modello inserito, ossia una distribuzione ged
fit.ged
#tale distribuzione è simmetrica e ha code esponenziali come la normale
#in effetti corrisponderebbe alla distribuzione normale qualora il parametro di forma fosse nu=2
#invece in questo caso viene stimato ~0.9, il che garantisce code più pesanti 

#densita' empirica a confronto con quella teorica:
ggplot(data=jap.data, aes(x = lret)) + geom_histogram(aes(y=..density..),bins = 100, col = "white", fill = "steelblue") +
  geom_density(col=3,lwd=1) + theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 16), axis.text.y = element_text(size = 16), title = element_text(size = 20)) + labs(title = "Confronto tra densità empirica  e teorica (ged)", x = "rendimenti", y = "
Frequenza") + stat_function(fun=dged,args=list(mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),col=2,alpha=0.7,lwd=1)
#confronto ora la funzione di ripartizione empirica con la vera funzione di ripartizione:
plot(sort(lret),(1:N/N),main="Probabilita'",col="steelblue4")
curve(pged(x,mean=fit.ged$par[1],sd=fit.ged$par[2],nu=fit.ged$par[3]),add=T,col=2,lwd=2)


#Punto 4.
#verificare l’ipotesi della presenza di asimmetria della distribuzione Skew Student–t
fit.sstd<-sstdFit(lret)
fit.sstd
res<-fit.sstd$estimate
sigma<-sigma(fit.sstd)
res.std<-res/sigma
res.std2<-res.std^2
########################
fit.t<-garchFit(~garch(1,1), data=lret, cond.dist="sstd",trace=F,include.mean=T)
summary(fit.t)
res<-fit.t@residuals
sigma2<-fit.t@sigma.t
res.std<-res/sigma2
res.std2<-res.std^2
#######################
#plot
plot1<-ggAcf(res.std,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui standardizzati", x = "lag", y = "ACF")
plot2<-ggPacf(res.std, lag.max=100) + labs(title = "PACF dei residui standardizzati", x = "lag", y = "PACF")
plot3<-ggAcf(res.std2,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei residui al quadrato", x = "lag", y = "ACF")
plot4<-ggPacf(res.std2, lag.max=100) + labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
a<-res.std2[-1] #tolgo la prima osservazione
b<-res[-length(res)] #tolgo l'ultima osservazione
length(a)
length(b)
#applichiamo un criterio di diagnostica che confronta la correlazione fra i residui non standardizzati al quadrato e i residui 
#non std al grado 1 ritardati di un passo. Una correlazione negativa mi fa presupporre che ci sia un effetto di leverage
cor(res[-1]^2,b) #negativa, quindi c'e' asimmetria

#applichiamo anche dei test di simmetria (Engle-Ng) sui residui standardizzati del GARCH student-t
d=numeric(length(b)) #generiamo la variabile dummy
for(i in 1:length(b)){
  if (b[i]<0) d[i]=1
}
fit.lm=lm(a~1+d) # Sign test
summary(fit.lm)
#la dummy risulta fortemente significativa, pertanto uno shock negativo in (t-1) ha un impatto maggiore di uno shock positivo
D=d*b
fit.lm=lm(a~1+D) # Size-sign test
summary(fit.lm)
#anche da questo test sembra essere confermata la presenza di asimmetria nei residui del GARCH
fit.lm=lm(a~1+d+D) # Test congiunto
summary(fit.lm)
#congiuntamente la magnitudo non sembra avere un effetto rilevante sull'asimmetria dei residui

#Punto 5.
#Verificare l’ipotesi che il numero dei gradi di liberta` della Skew Student–t sia uguale a 8.
sh<-fit.t@fit$par[6];sh
std.sh<-fit.t@fit$se.coef[6];std.sh
#costruiamo un test per verificare H0: nu=8
test<-function(nu,nu0=8,std){
  t<-(nu-nu0)/std
  2*min(pnorm(t), 1-pnorm(t))
}
test(sh,std=std.sh) #rifiuto H0, i gradi di libertà della skew-student sono diversi da 8 

ggplot(data=jap.data, aes(x = lret))+geom_density(aes( y=..density..),col = "steelblue")+
  stat_function(fun=dsstd,args=list(mean=0.0005665267,sd=0.0151280521,nu=5.96647, xi=0.9334239),col=2,alpha=0.7,lwd=1)
  
  stat_function(fun=dsstd,args=list(mean=fit.t@fit$par[1],sd=fit.t@fit$par[2],
  nu=fit.t@fit$par[6], xi=fit.t@fit$par[5]),col=2,alpha=0.7,lwd=1)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15)) + scale_color_manual(values = 
    c("steelblue",  "red"), labels = c("ν=8", "ν=ν.stima")) +
  theme(legend.text = element_text(color = "black",size = 14)) + 
  labs( x = "rendimenti", y = "Frequenza") 
help(sstd)


#################

 

#Punto 6.
#stimiamo un processo GARCH-M sui log-rendimenti ipotizzando, come visto al punto precedente, che gli errori del processo generatore dei dati provengano da una ged 
spec.gm2<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
        mean.model=list(armaOrder=c(1,1),include.mean=F,
        archm=T,archpow=2),distribution.model="ged", fixed.pars = list(omega=0))
fit.gm2<-ugarchfit(spec.gm2,data=lret)
show(fit.gm2)
#il parametro archm è detto parametro di premio al rischio. E' una misura del trade–off tra rischio e rendimento che, se positiva come in questo caso, indica una correlazione positiva tra rendimenti e rischio.
#Tale parametro, risulta fortemente significativo per g(sigma(t))=g(sigma(t))^2
#Proviamo ad adattare un GARCH-M tale per cui g(sigma(t))=g(sigma(t))
spec.gm1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(1,1),include.mean=F,
                                    archm=T,archpow=1),distribution.model="ged", fixed.pars = list(omega=0))
fit.gm1<-ugarchfit(spec.gm1,data=lret)
show(fit.gm1)
plot(fit.gm1,which=7)
#anche dal grafico possiamo notare la presenza di autocorrelazione tra la serie osservata del JAPDAWA e la stessa serie al quadrato, proxy della volatilta'


#Punto 7.
#ARMA-GARCH con errori Normali:
#come avevamo già visto nel punto2, per la media adattiamo un modello ARMA(1,1), senza intercetta.
#ci resta da modellare la varianza che, sempre al punto 2, era risultata fortemente eteroschedastica
plot1<-ggAcf(res.m.sq,type = c("correlation"), lag.max = 100) +labs(title = "ACF residui al quadrato", x = "lag", y = "ACF")
plot2<-ggPacf(res.m.sq, lag.max =100) + labs(title = "PACF dei residui al quadrato", x = "lag", y = "PACF")
plot3<-ggAcf(res.m.abs,type = c("correlation"), lag.max = 100) +labs(title = "ACF residui in valore assoluto", x = "lag", y = "ACF")
plot4<-ggPacf(res.m.abs, lag.max =100) + labs(title = "PACF dei residui in valore assoluto", x = "lag", y = "PACF")
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
#adattiamo un modello che possa catturare la struttura ancora presente nei dati
spec1<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(10,0)), 
          mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
fit1<-ugarchfit(spec1,lret)
show(fit1)
#i parametri sono tutti significativi, come ci si aspettava vista la forte persistenza dei valori assoluti e dei quadrati
#Proviamo a rendere il modello più parsimonioso adattando un GARCH:
spec2<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
fit2<-ugarchfit(spec2,lret)
show(fit2)  #omega non significativo
spec3<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm", fixed.pars = list(omega=0))
fit3<-ugarchfit(spec3,lret) #alpha 2 non significativo
show(fit3) 
spec4<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="norm", fixed.pars = list(omega=0))
fit4<-ugarchfit(spec4,lret)
show(fit4)
#stando alla significatività dei parametri il modello migliore sembra essere l'ARMA(1,1)-GARCH(1,1)
infocriteria(fit1)
infocriteria(fit2)
infocriteria(fit4)
infocriteria(fit3)
#guardando i criteri emerge che il modello che spiega meglio i dati è ARMA(1,1)-GARCH(2,1)

par(mfrow=c(2,1))
plot1<-plot(fit2, which=10)
plot2<-plot(fit2, which=11)
par(mfrow=c(1,1))
#i residui standardizzati ma soprattutto quelli al quadrato non mostrano più alcun tipo di correlazione, il modello è buono
plot(fit2, which=9)
#si nota un'evidente deviazione dalla gaussianita', in particolare sulla coda sinistra
AIC.n<-infocriteria(fit2)[1]

#ARMA-GARCH con errori Student-t:
spec1<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit1<-ugarchfit(spec1,lret)
show(fit1)
#ci sono diversi parametri non significativi, proviamo ad alleggerire il modello rimuovendone alcuni
spec2<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(0,0)),distribution.model="std")
fit2<-ugarchfit(spec2,lret)
show(fit2)  #omega non significativo

spec2<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)), 
                  mean.model=list(armaOrder=c(0,0)),distribution.model="std", fixed.pars = list(omega=0))
fit2<-ugarchfit(spec2,lret)
show(fit2)

spec3<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(0,0)),distribution.model="std", fixed.pars = list(omega=0))
fit3<-ugarchfit(spec3,lret) #alpha 2 non significativo
show(fit3)

spec4<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(1,1)),distribution.model="std", fixed.pars = list(omega=0))
fit4<-ugarchfit(spec4,lret)
spec5<-ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(0,1)),distribution.model="std")
fit5<-ugarchfit(spec5,lret)
show(fit5)

#stando alla significatività dei parametri il modello migliore sembra essere l'ARMA(1,1)-GARCH(1,1)
infocriteria(fit1)
infocriteria(fit2)
infocriteria(fit4)
infocriteria(fit3)
#guardando i criteri emerge che il modello che spiega meglio i dati è ARMA(1,1)-GARCH(2,1)

par(mfrow=c(2,1))
plot1<-plot(fit2, which=10)
plot2<-plot(fit2, which=11)
par(mfrow=c(1,1))
#i residui standardizzati ma soprattutto quelli al quadrato non mostrano più alcun tipo di correlazione, il modello è buono
plot(fit2, which=9)
#si nota un'evidente deviazione dalla gaussianita', in particolare sulla coda sinistra
AIC.n<-infocriteria(fit2)[1]


AIC.t<-infocriteria(fit.st)[1]
which.min(c(AIC.n,AIC.t))
#secondo il criterio di Akaike, ? migliore il secondo modello.



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









