# MSF 2020, Homework n. 3
# Data loading

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load the dataset 
data.indeces <- read.csv("International_Indexes_CSV.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
dates        <- as.Date(data.indeces$Date, format = "%d/%m/%Y")
n            <- nrow(data.indeces)
p            <- ncol(data.indeces)
indeces      <- data.indeces[,2:p]
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

# Punto 1.
# analisi descrittiva della serie dei prezzi logaritmici
head(jap.data$lvalue)
tail(jap.data$lvalue)
# grafico dei log dei prezzi
ggplot(data = jap.data, aes(x = day, y = lvalue)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Prezzi logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
# statistiche più importanti dei prezzi
summary(jap.data$lvalue)
round(basicStats(jap.data$lvalue), 4)
# calcolo l'ACF  PACF
ggAcf(jap.data$lvalue, type = c("correlation")) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
ggPacf(jap.data$lvalue) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")
#dal grafico risulta evidente che la serie dei prezzi logaritmici non sia stazionaria. In particolare la lenta decrescita della funzione di autocorrelazione testimonia la forte dipendenza temporale.
#da questa serie non epurata dall'effetto in media è difficile fare considerazioni sulla stazionarietà in varianza, tuttavia anche questa non sembra costante nel tempo, nonostante l'applicazione del logaritmo abbia sensibilmente ridotto il range di variazione dei dati, comprimendoli attorno al trend


#Punto 2.
#analisi dei rendimenti logaritmici 
ggplot(data = jap.data, aes(x = day, y = lret)) + geom_line(color="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d %b %Y") + labs(title = "Rendimenti logaritmici JAPDOWA", subtitle="Serie da 31-12-2002 a 05-04-2019", x = "time", y = "index")
# il fenomeno del volatility clustering risulta essere piuttosto evidente
sk <- mean(jap.data$lret^3)/(sd(jap.data$lret))^3;sk
#indice di asimmetria empirico, nei rendimenti sembra esserci una leggera asimmetria a sinistra
k <- mean(jap.data$lret^4)/(var(jap.data$lret))^2;k
#indice di curtosi, è >3 dunque le code di questa distribuzione sono più pesanti di quelle della normale
ggAcf(jap.data$lret,type = c("correlation"), lag.max = 100) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
ggPacf(jap.data$lret, lag.max = 100) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")
#dalle funzioni di ACF e PACF si notano parecche autocorrelazioni significative che si protraggono nel tempo fino a lag piuttosto lontani
auto.arima(jap.data$lret)
#proviamo ad adattare un modello ARMA(1,1) sui rendimenti logaritmici, che quindi sono già stai differenziati una volta (ARIMA(1,1,1) per jap.data$value)
model1<-arima(jap.data$lret, order=c(1,0,1), include.mean = F)
coeftest(model1)

###QUA NON SAPEVO BENE SE USARE AUTO.ARIMA PERò SENZA NON CI SAREI MAI ARRIVATA, LE AUTOCORR SIGNIFICATIVE SONO TROPPO IN Là NEL TEMPO PER PROCEDERE "CONTANDO LE SBARRETTE" E CMQ PRESI DA SOLI I COEF NON SONO MAI SIGNIFICATIVI...

ggAcf(residuals(model1),type = c("correlation"), lag.max = 100) +labs(title = "ACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "ACF")
ggPacf(residuals(model1), lag.max = 100) + labs(title = "PACF dei prezzi logaritmici JAPDOWA", x = "lag", y = "PACF")

###manca parte arch



acf(residuals(model1))
in particolare verificare la significativita` della media, 

la presenza di autocorrelazione e la presenza di effetti ARCH.




Costruiamo l’istogramma dei rendimenti logaritmici,
il qqnorm, gli indici di curtosi e di asimmetria. 




ggplot(jap.data$lret, aes(x = lret, y = ..density..))

+ geom_histogram(bins = 50, col = "white", fill = "red", alpha = 0.5) 
+geom_density(col = 3) +  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 16), axis.text.y = element_text(size = 16), title = element_text(size = 20)) + labs(title = "Histogram for DJI log-returns", x = "DJI log-returns", y = "Frequency")
