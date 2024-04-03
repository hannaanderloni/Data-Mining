#MODELLO LINEARE ROBUSTO

setwd("/Users/hannaaro/Desktop/GitHub/Data-Mining")

options(scipen=999) 

####IMPORTAZIONE####

r <- read.csv("car details v4.csv" , sep="," , dec = ".",  
              stringsAsFactors=TRUE, na.strings=c("NA","NaN", ""))

#Sistemiamo le variabili 
r$Prezzo_eu <- r$Price * 0.011 ; head(r$Price_eu)
r$Model <- as.character(r$Model)
#r <- r[,-3] #togliamo Model in quanto variabile del tipo id-key #NB: già fatto, non eseguire

variabili <- paste(colnames(r), collapse="','")
variabili

features <- c('Make','Prezzo_eu','Model','Year','Kilometer','Fuel.Type','Transmission','Location','Color','Owner','Seller.Type','Engine','Drivetrain','Length','Width','Height','Seating.Capacity','Fuel.Tank.Capacity')
r=r[,features]

#Conversione della variabile Engine in una numerica (da cc 1234 a 1234)
library("tidyr")
r <- r %>%
  separate(Engine, c("col1", "col2"), " ")

library(dplyr)
#r <- r[, -13] #NB: già fatto, non eseguire

colnames(r)[12] <- "Engine"
r$Engine <- as.numeric(r$Engine)

library(Hmisc)
describe(r)

####DATI MANCANTI####

sapply(r, function(x)(sum(is.na(x)))) 

library(VIM)
missingness_r <- aggr(r, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(r), cex.axis=.7,gap=3)

####PRIMO MODELLO####

boxplot(r$Prezzo_eu)

#Fittiamo un primo modello con tutte le variabili del dataset
fit_0 <- lm(Prezzo_eu ~ ., data = r)
summary(fit_0)
nrow(r)
length(fit_0$residuals) #il modello usa 1874 obs -> ci sono 1874 righe complete

drop1(fit_0, test="F")

#plot diagnostici
par(mfrow=c(2,2)) 
plot(fit_0)
par(mfrow=c(1,1)) 

plot(r$Color) #controlliamo la distribuzione di Color (più avanti ignoreremo la variabile in quanto non significativa)
plot(r$Seller.Type) #controlliamo Seller.Type: in model selection con stepwise non viene considerata: tant'é che sono tutti "individual" (near zero variance) (verrà infatti ingorato in futuro)
plot(r$Height) #decideremo di ignorarla dopo aver anche tentato un gam, in quanto non significativa, nonostante la model selection la salvi 
plot(r$Owner) #verrà ignorato in futuro in quanto non significativo

####COLLINEARITÀ (1)####

covariate0 = attr(termã(fit_0), "term.labels") ; covariate0 #estrazione delle covariate usate dal modello fit_0

#Estrazione dei fattori dalle covariate
library(plyr) 
library(dplyr)
b2_factors <- r[,covariate0] %>% 
  dplyr::select_if(is.factor) ; b2_factors 
colnames(b2_factors)
str(b2_factors)

#Analisi della collinearità per le variabili factor tramite Chi quadrato normalizzato
combos <- combn(ncol(b2_factors),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b2_factors[, x[1]], b2_factors[, x[2]])
  tab  <- table(b2_factors[, x[1]], b2_factors[, x[2]])
  out <- data.frame("Row" = colnames(b2_factors)[x[1]]
                    , "Column" = colnames(b2_factors[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))
                    , "u1" =length(unique(b2_factors[,x[1]]))-1
                    , "u2" =length(unique(b2_factors[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b2_factors[,x[1]], b2_factors[,x[2]]))* min(length(unique(b2_factors[,x[1]]))-1 , length(unique(b2_factors[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 

#Si vede che non vi sono dipendenze rilevanti tra i fattori

#Estraiamo le variabili quantitative
isnumeric <- sapply(r, function(x) is.numeric(x))
numericdata <- r[, isnumeric]

#Correlogramma
require(corrgram)
corrgram(r_numeric)
corrgram(r_numeric, lower.panel = panel.cor, cex=1, cex.labels = 1)

#Grafico correlaizione e distribuzioni
library(PerformanceAnalytics)
chart.Correlation(numericdata , histogram=TRUE, pch=19)

##TOL e VIF per le numeriche
y <- as.numeric(r$Prezzo_eu) 
X <- numericdata
X_matrix = as.matrix(X)
mod <- lm(y ~ X_matrix) 
library(mctest)
imcdiag(mod) # TOL e VIF
#Si notano valori critici di TOL e VIF per Engine, Lenght, Width e Fuel.Tank.Capacity
#Decidiamo allora di procedere con un optimal grouping di fattori con alto numero di livelli per poi eseguire una PCA per tentare di risolvere la collinearitò tra Engine, Lenght, Width e Fuel.Tank.Capacity

####OPTIMAL GROUPING di Location####
library(dplyr)
library(factorMerger)

reduce_levels <- mergeFactors(response = r$Prezzo_eu, factor = r$Location)
mergingHistory(reduce_levels, showStats = TRUE ) %>%head(5)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )
plot(reduce_levels, palette = "Reds")
plot(reduce_levels, responsePanel = "boxplot", colorCluster = TRUE)
?cutree
og = cutTree(reduce_levels)
r$optimal_grouping = og
head(r)

r$optimal_grouping <- as.factor(as.numeric(r$optimal_grouping))
colnames(r)[19] <- "Location_G"
#r <- r[,-8] #Togliamo Location originale NB: già fatto, non eseguire

plot(r$Location_G, r$Prezzo_eu)

####OPTIMAL GROUPING di Make####

library(dplyr)
library(factorMerger)

reduce_levels1 <- mergeFactors(response = r$Prezzo_eu, factor = r$Make)
mergingHistory(reduce_levels1, showStats = TRUE ) %>%head(50)
plot(reduce_levels1 , panel = "GIC",title = "", panelGrid = FALSE )
plot(reduce_levels1, palette = "Reds")
plot(reduce_levels1, responsePanel = "boxplot", colorCluster = TRUE)

og_2 <- cutTree(reduce_levels1, stat = "loglikelihood" , value = -23000); str(og_2) # taglio del dendrogramma in corrispondenza di 5 gruppi
r$optimal_grouping1 = og_2
head(r)

fit_g3 = lm(Prezzo_eu ~ Make  ,r)
fit_g4 = lm(Prezzo_eu ~ optimal_grouping1 ,r)
summary(fit_g3)
summary(fit_g4)

r$optimal_grouping1 <- as.factor(as.numeric(r$optimal_grouping1))

r <- r[,-1]
colnames(r)[17] <- "Make_G"
fit_1 <- lm(Prezzo_eu ~ ., data = r)
summary(fit_1)
drop1(fit_1, test ="F")

plot(r$Make_G, r$Prezzo_eu)

####PRINCIPAL COMPONENTS ANALYSIS####
#2 modi

#Selezioniamo le variabili che vogliamo sintetizzare tramite PCA (sono internamente coerenti)
library(dplyr)
r_noNA <- na.omit(r)  ; nrow(r_noNA) #costruiamo il dataset senza NA
r_pca <- r_noNA %>% select("Length","Width","Fuel.Tank.Capacity","Engine") ; head(r_pca)
nrow(r_pca)

#Visualizzazione  della varianza riprodotta (singolarmente e cumulativamente) dalle PC
#Decidiamo di estrarre le PC dalla matrice di correlazione poiché le unità di misura e gli ordini di grandezza sono differenti
fit_pca_1 <- princomp(r_pca,  cor=TRUE) 
summary(fit_pca_1) 

#Nel seguito estraiamo la prima PC, che riteniamo sufficiente
library(psych)
fit_pca_2 <- principal(r_pca, nfactors=1, scores = TRUE,  covar=FALSE) ; head(fit_pca_2$scores) 
pc1 <- data.frame(fit_pca_2$scores) ; head(pc1)

r_noNA_pc <- cbind(r_noNA, pc1) ; head(r_noNA_pc) #agganciamo al dataset senza NA la PC1

####COLLINEARITÀ (2)####

r2 <- r_noNA_pc[,!names(r_noNA_pc) %in% c("Length","Width","Fuel.Tank.Capacity","Engine")] 

isnumeric <- sapply(r2, function(x) is.numeric(x))
numericdata2 <- r2[, isnumeric]

y <- as.numeric(r_noNA_pc$Prezzo_eu) 
X2 <- numericdata2
X_matrix2 = as.matrix(X2)
mod2 <- lm(y ~ X_matrix2) 
library(mctest)
imcdiag(mod2) 
#Collinearità risolta

####LINEARITÀ####

fit_c <- lm(Prezzo_eu ~ . , data = r2) #attenzione: r2 viene poi modificato, quello che appare qui NON è quello su cui è stato fittato il modello fit_c
summary(fit_c)
drop1(fit_c, test = "F")

par(mfrow=c(2,2)) 
plot(fit_c)
par(mfrow=c(1,1)) 

#boxcox

library(MASS)
boxcoxreg1 <- boxcox(fit_c) ; boxcoxreg1 ; summary(boxcoxreg1)
title("Lambda vs Log-Likelihood")
lambdamax <- boxcoxreg1$x[which.max(boxcoxreg1$y)] ; lambdamax #lambdamax=  -0.1010101 quindi prendiamo il logaritmo di Prezzo_eu

ylog <- log(r2$Prezzo_eu)
r2 <- cbind(r2, ylog) 
#r2 <- r2[,-1] #per togliere Prezzo_eu #NB: già fatto, non eseguire

fit_l0 <- lm(ylog ~ . , data= r2) 

library(lmtest) 
resettest(fit_l0, power = 2, type = "fitted",  data = r2) # non correttamente specificato 

#gam 

variabili2 <- paste(colnames(r2), collapse="+") ; variabili2

library(gam)

gam_2 <- gam(ylog ~ s(Year) + s(Kilometer) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain+s(Height)+s(Seating.Capacity)+Location_G+Make_G+s(PC1), data = r2)
summary(gam_2) 

par(mfrow=c(3,3)) 
plot(gam_2)
par(mfrow=c(1,1)) 

#per controllare la distribuzione e avere un'idea di possibili outlier/alto leverage/valori influenti produciamo i boxplot
boxplot(r2$Year)
boxplot(r2$Kilometer)
boxplot(r2$Seating.Capacity)

#modello post gam (ma è necessario gestire prima gli influenti)
fit_l1 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain + Height+ I(Height^2)+I(Height^3) + Seating.Capacity + Location_G+Make_G+ PC1, data = r2)
drop1(fit_l1, test = "F")
library(lmtest) 
resettest(fit_l1, power = 2, type = "fitted",  data = r2) # non corettamente specificato (ma meglio del reset pre boxcox)

####VALORI INFLUENTI####

#Outlier molto elevati potrebbero dar problemi alle gam evidenziando finte forme funzionali nelle distribuzioni
library(car)
influencePlot(fit_l0,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#Cook's D

cooksd <- cooks.distance(fit_l0)
cd <- data.frame(cooksd)
#Settiamo ora il valore di cut-off, pari a 4/n, dove n = numero di osservazioni usate dal modello
cutoff <- 4/(length(fit_l0$residuals)-length(fit_l0$coefficients)-2)
oss_influenti <- data.frame(r2[cooksd > cutoff, ])  # influential row numbers
nrow(oss_influenti) 
#Ora sappiamo quante osservazioni sono influenti secondo Cook's D, ovvero 108

#Si tratta ora di considerare solo le righe non influenti e fittare il modello (gam) su questi dati

r2_noinflu <- data.frame(r2[cooksd < cutoff, ])  
fit_l0_noinflu <- lm(ylog ~ . , data= r2_noinflu) 
influencePlot(fit_l0_noinflu,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#rimangono osservazioni ad alto leverage (quindi influenti per le x ma non per la y) e un'osservazione (nr. 1516) che può essere considerata influente (e verrà eliminata in futuro)

gam_3 <- gam(ylog ~ s(Year) + s(Kilometer) + Fuel.Type + Transmission + Color +
               Owner+Seller.Type+Drivetrain+s(Height)+s(Seating.Capacity)+Location_G+Make_G+s(PC1), data = r2_noinflu)

par(mfrow=c(3,3)) 
plot(gam_3)
par(mfrow=c(1,1)) 

fit_l2_noin <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + Color +
                    Owner+Seller.Type+Drivetrain + Height+ I(Height^2)+I(Height^3) + Seating.Capacity + Location_G+Make_G+ PC1, data = r2_noinflu)
drop1(fit_l2_noin, test = "F")
resettest(fit_l2_noin, power = 2, type = "fitted",  data = r2_noinflu) 
#comunque non specificato bene ma meglio di prima NB: non significative height e relative potenze e seller type
#decidiamo di togliere le variabili non significative Height, Color (peggiora il RESET), Seller.Type 

#modello post gestione di collinearità, non-linearità, valori influenti
fit_clo <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                Owner+Drivetrain+ Seating.Capacity + Location_G+Make_G+ PC1, data = r2_noinflu)
drop1(fit_clo, test="F")
summary(fit_clo)
library(lmtest)
resettest(fit_clo, power = 2, type = "fitted",  data = r2_noinflu) 

#Osserviamo i partial plot
library(car)
par(mforw=c(4,4))
avPlots(fit_clo) 
par(mfrow=c(1,1)) 
#commento: in Kilometer (e Kilometer^2) l'osservazione 1516 influenza fortemente la pendenza della retta (quindi l'effetto della covariata): poiché evidentemente dopo un tot nr di km il prezzo non varia più. Quindi non ha senso tenerla perchè la retta così fatta non riflette la vera relazione. Proviamo allora a rimuovere l'osservazione 1516.  
#In Make_G si vede che (essendo contrasti con il reference di fascia bassa) l'evidenza di outlier (che non sono però influenti dalla prima tornata di influent detection) è progressivaemnte più marcata più ci si allontata come fascia di prezzo di Make.

r_z <- r2_noinflu[r2_noinflu$Kilometer < 300000,] #r2_noinflu senza obs 1516 
fit_cloz <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                 Owner+Drivetrain+ Seating.Capacity + Location_G+Make_G+ PC1, data = r_z)
drop1(fit_cloz, test="F")
summary(fit_cloz)
library(lmtest)
resettest(fit_cloz, power = 2, type = "fitted",  data = r_z)
library(car)

par(mforw=c(4,4))
avPlots(fit_cloz) 
par(mfrow=c(1,1)) 

par(mfrow=c(2,2)) 
plot(fit_cloz) 
par(mfrow=c(1,1)) 
#diagnostici molto promettenti, non ci azzardiamo a rimuovere le osservazioni con alto leverage perchè abbiamo pochi dati e non vogliamo correre il rischio di inficiare l'analisi

####MODEL SELECTION####

library(MASS)
selectedMod <- step(fit_cloz, direction="both")I 
#stepwise non toglie nulla quindi confermiamo il modello precedente

fit_clozm <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Fuel.Type + Transmission + 
                  Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) #modello post model selection
summary(fit_clozm)
drop1(fit_clozm, test="F")
library(lmtest)
resettest(fit_clozm, power = 2, type = "fitted",  data = r_z) # non ancora specificato bene ma migliorato

####ETEROSCHEDASTICITÀ####

plot(fit_clozm, which=1) 

#White Test of Heteroskedasticity
library(car)
ncvTest(fit_clozm) 
#Rifiutiamo l'ipotesi nulla di omoschedasticità, quindi è presente eteroschedasticità

#Breusch-Pagan Test
library(lmtest)
bptest(fit_clozm)
#Stesso risultato del prededente

#Impieghiamo la correzione di White per gli errori standard
library(lmtest)
library(sandwich)
coeftest(fit_clozm,vcov=vcovHC(fit_clozm)) #inferenza robusta (valida anche in presenza di eteroschedasticitò)
summary(fit_clozm) #inferenza senza correzione
#Con coeftest le stime sono identiche ma gli standard error sono tendenzialmente più grandi (quindi le stime sono in genere più variabili, o almeno più variabilità viene concessa, poi, se la variabile singola non è affetta da eteroshcedasticità, gli s.e. saranno simili) 
#Infatti l'inferenza è differente e quella robusta è quella prodotta da coeftest
#Vediamo che in generale s.e robusti > s.e "normali" e che p-value robusto >  p-value "normale"; 
#Notiamo però che la significatività dei coefficienti è pressoché identica 
library(coefplot)
coefplot(fit_clozm, decreasing=T,sort="magnitude",intercept=F)
#Grafico per l'inferenza
library(forestmodel)
forest_model(fit_clozm)

#Proviamo a fittare un modello senza Fuel.Type poiché ha solo un livello significativo e notiamo che il RESET migliora notevolmente

fit_clozm3 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) 

resettest(fit_clozm3, power = 2, type = "fitted",  data = r_z) # migliorato:
#  0.000000000000001238
#< 0.00000000000000022
#=> migliorato sia il numero puro (65 contro 76) ma anche molto il p-value!

#Proviamo anche senza km^2 che è meno significativo degli altri
fit_clozm4 <- lm(ylog ~ Year + Kilometer + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) 
resettest(fit_clozm4, power = 2, type = "fitted",  data = r_z) 
#peggiora

#Modello definitivo post-eteroschedasticità
fit_clozme <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z) 
summary(fit_clozme)
drop1(fit_clozme, test="F")
resettest(fit_clozme, power = 2, type = "fitted",  data = r_z) 
library(coefplot)
coefplot(fit_clozme, decreasing=T,sort="magnitude",intercept=F)
library(forestmodel)
forest_model(fit_clozme)

par(mfrow=c(2,2)) 
plot(fit_clozme) 
par(mfrow=c(1,1)) 
#Osserviamo che i diagnostici sono pressoché identici a prima; c'è da dire che di fatto l'eteroschedasticità non si risolve bensì si "schiva" per produrre un'inferenza che non sai influenzata da essa

?gvlma

#Per il gvlma usiamo il modello finale ma senza KM^2 poiché restituisce problemi con la collinearità
library(gvlma)
fit_clozme_xgvlma <- lm(ylog ~ Year + Kilometer + Transmission + 
                          Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z)
gvlma(fit_clozme_xgvlma)
library(Hmisc)
describe(r_z)

#le assunzioni sembra essere rispettate ma non ci spieghiamo il risultato riguardo a Link Function.

####BOOTSTRAP####

#bootstrap richiede righe complete, per qualche motivo ci sono 2 righe interamente vuote in r_z 
sapply(r_z, function(x)(sum(is.na(x)))) 
r_z2 <- na.omit(r_z)  ; nrow(r_z2); nrow(r_z)
sapply(r_z2, function(x)(sum(is.na(x)))) 

fit_clozme_2 <- lm(ylog ~ Year + Kilometer + I(Kilometer^2) + Transmission + 
                     Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z2) 

library(car)
BOOT.MOD=Boot(fit_clozme_2, R=1999)
summary(BOOT.MOD, high.moments=TRUE)

# confint boot
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

#l'inferenza prodotta dai coefficienti risulta robusta dal bootstrap, tranne per un livello di owner (trascurabile) e km^2
#Nonostante il RESET migliori leggermente includendo km^2, il bootstrap lo boccia, quindi lo rimuoviamo

fit_finale <- lm(ylog ~ Year + Kilometer + Transmission + 
                   Owner + Drivetrain + Seating.Capacity + Location_G + Make_G + PC1, data = r_z2) 

summary(fit_finale)
drop1(fit_finale, test = "F")
resettest(fit_finale, power = 2, type = "fitted",  data = r_z2)

library(car)
BOOT.MOD=Boot(fit_finale, R=1999)
summary(BOOT.MOD, high.moments=TRUE)
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")
  
par(mfrow=c(2,2)) 
plot(fit_finale) 
par(mfrow=c(1,1)) 

plot(r_z2$ylog, fit_finale$fitted.values)


####REGRESSIONE LOGISTICA####

#Aggiustiamo il dataset (l'ultimo risultante dal lavoro sul modello lineare)
r_rl90 <- r_z2 
r_rl90$Prezzo_eu_reverse <- exp(r_rl90$ylog) ; head(r_rl90)
r_rl0 <- r_rl90[,-14] ; head(r_rl0)

#Informazioni per decidere come trasformare in binaria Prezzo_eu
library(Hmisc)
describe(r_rl0$Prezzo_eu_reverse)
plot(r_rl0$Prezzo_eu_reverse)

#Prendiamo come threshold per la conversione in variabile binaria di Prezzo_eu la mediana

r_rl0$Prezzo_alto <- ifelse(r_rl0$Prezzo_eu_reverse > 9185, 1, 0) ; head(r_rl0) # Prezzo_alto= 1 => sopra la mediana
#Drop di Prezzo_eu non binario
r_rl1 <- r_rl0[,-14] ; head(r_rl1)

#Usiamo il dataset con collinearità già risolta e non abbiamo fattori o variabili continue con zero-variance o near-zero-variance, quindi l'unico problema può essere la separation/quasi-separation

#fit logistico con covariate dell'ultimo modello lineare

rl_0 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission + Make_G + 
              Owner + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial)
#warning che suggerisce separation (su Make_G, col senno di poi)
summary(rl_0)
drop1(rl_0, test = "LRT") 

table(r_rl1$Prezzo_alto) #frequenze assolute
prop.table(table(r_rl1$Prezzo_alto)) #frequenze relative 

print(rl_0$coefficients)
exp(rl_0$coefficients) #odds ratio
exp(confint(rl_0))
#notiamo che i valori dell'OR per Make_G e PC1 sono sospetti

#quasi separation/separation per Make_G

table(r_rl1$Prezzo_alto, r_rl1$Make_G)
#ecco il problema: Make_G soffre di searation nei livelli 3,4,5: proprio quelli non significativi sul summary e caratterizzati da OR estremi
#soluzione: rimuoviamo Make_G in quanto costituisce di fatto una regola classificativa deterministica 

table(r_rl1$Prezzo_alto, r_rl1$PC1)
describe(r_rl1$PC1)
plot(r_rl1$PC1, r_rl1$Prezzo_alto) #dal plot non sembra esserci grave separation => proviamo a tenerla nel modello

#Controlliamo anche le tabelle di frequenza delle altre covariate

table(r_rl1$Prezzo_alto, r_rl1$Year) #vediamo che Year invece è a posto
table(r_rl1$Prezzo_alto, r_rl1$Location) #perfetta
table(r_rl1$Prezzo_alto, r_rl1$Owner) #presumiamo che nel corso della pulizia del dataset siano stati rimosse alcune osservazioni che avevano Owner = 4 o 4+, quindi per mantenere Owner modifichiamo la variabile eliminando i livelli in questione

#Eliminiamo i livelli inutili" di Owner (con 0 osservazioni)
library(MASS)
r_rl1$Owner <- droplevels(r_rl1$Owner) 

table(r_rl1$Prezzo_alto, r_rl1$Seating.Capacity) #perfetto
table(r_rl1$Prezzo_alto, r_rl1$Drivetrain) #perfetto
table(r_rl1$Prezzo_alto, r_rl1$Transmission) #perfetto

rl_1 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission + 
              Owner + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_1) 

library(forestmodel)
print(forest_model(rl_1),text_size = 5) 

#togliamo Owner in quanto significativo solo un livello e al 10% e al limite proprio, quindi si leva

rl_2 <- glm(Prezzo_alto ~ Year + Kilometer + Transmission 
            + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_2)
# vediamo che Kilometer - controintuitivamente - non è molto significativo

exp(rl_2$coefficients) #notiamo che l'odds ratio di kilometer (dato da exp(beta_km)) è =ca. 1 => togliamo km 
exp(confint(rl_2))

rl_3 <- glm(Prezzo_alto ~ Year  + Transmission 
            + Drivetrain + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) #gira
summary(rl_3)
drop1(rl_3, test = "LRT")

library(forestmodel)
print(forest_model(rl_3),text_size = 5) 

#OR e confint del mod finale

a <- exp(rl_3$coefficients) 
b <- exp(confint(rl_3))
tab_OR <- cbind("OR"=a,b); tab_OR
round(tab_OR, digits=2)

#plots
par(mfrow=c(2,2)) 
plot(rl_3)
par(mfrow=c(1,1)) 

#ricaviamo l'R^2 

null = glm(Prezzo_alto ~ 1, data =r_rl1  ,family = binomial) #modello costante
summary(null) #la devianza residua COINCIDE CON LA DEVIANZA TOTALE DA SPIEGARE 
#Residual deviance: 2446.8  on 1764  degrees of freedom = deivanza totale da spiegare

ls(null)
dev_tot <- null$deviance # devianza totale (da spiegare)

ls(rl_3)
dev_residua <- rl_3$deviance #devianza residua del modello in questione

R2 <- 1 - (dev_residua/dev_tot) ; R2
#0.8036409

#faccamo un'altra logistica per curiosità per poi fare un confronto

rl_4 <- glm(Prezzo_alto ~ Year  + Transmission 
            + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial) 
anova(rl_4, rl_3, test="LRT") #meglio il 3 con drivetrain

#Proviamo a decidere usando aic e bic

npar.all=c(length(rl_3$coefficients),length(rl_4$coefficients))
aic.all=c(rl_3$aic, rl_4$aic) #aic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza Drivetrain)
bic.all = aic.all-2*npar.all+npar.all*log(nrow(r_rl1)) #bic del modello completo rl_3 vs aic del modello ridotto rl_4 (senza Drivetrain)
aic.all #rl_3 minimizza il aic
bic.all #rl_4 minimizza il bic (che penalizza di più per il numero di covariate)

#il test anova concorda con l'aic => teniamo Drivetrain

#proviamo a togliere PC1 in quanto possibilmente problematica per separation

rl_5 <- glm(Prezzo_alto ~ Year  + Transmission + Drivetrain 
            + Seating.Capacity + Location_G, data = r_rl1, family = binomial) 
anova(rl_5, rl_3, test="LRT") #meglio tenerla

npar.all2=c(length(rl_3$coefficients),length(rl_5$coefficients))
aic.all2=c(rl_3$aic, rl_5$aic) 
bic.all2 = aic.all2-2*npar.all2+npar.all2*log(nrow(r_rl1)) 
aic.all2 
bic.all2 
#confermato, prevedibilmente: meglio tenerlo

rl_finale <- glm(Prezzo_alto ~ Year  + Transmission + Drivetrain 
                 + Seating.Capacity + Location_G + PC1, data = r_rl1, family = binomial)

describe(r_rl1$PC1)

