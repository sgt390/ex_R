# classificazione
data(mtcars)
dim(mtcars)
names(mtcars)


summary(mtcars)

# prendo i valori che mi interessano
# am: cambio automatico
dati <- mtcars[, c('vs','mpg','am')]
dati[1,]

is.numeric(dati$am)
dati$am <- as.factor(dati$am)
summary(dati)

# vs: variabile Y da classificare
# 18 "vs" con valore 0, 14 "vs" con valore 1
table(dati$vs)

# nb mediana = linea nera
# boxplot "vs" "mpg"(efficienza). Boxplot diversi e ben separati
# le mediane ben lontane
# mpg sembra ok per spiegare vs
# c'è un valore un po' discostato
boxplot(dati$mpg ~ dati$vs)
# mpg cambia al variare con am
boxplot(dati$mpg ~ dati$am)

# tutte le combinazioni tra le variabili e mpg
# nel passaggio dal primo al secondo, cambia la mediana ma c'è un po' di sovrapposizione del boxplot. Le scatole si sovrappongono e la parte sopra della mediana è mangiata dal secondo boxplot. (no interazione)
# Dal terzo al quarto (da am0 a am1) ho più separazione dei boxplot (possibile interazione)
# In entrambi (1->2 3->4) ho che mpg cresce
# Possiamo inserire tutto, poi vediamo come va
boxplot(dati$mpg ~ dati$am*dati$vs, names=c('am0.vs0','am1.vs0','am0.vs1','am1.vs1'))


## Regressione logistica
m1 <- glm(vs ~ mpg*am, data=dati, family=binomial)

# "Deviance residuals" non li usiamo
# "(Dispersion parameter for binomial family taken to be 1) famiglia classica. Se ho maggiore variabilità posso sistemare questo parametro."
# Number of Fisher Scoring iterations: 7. n° iterazioni per stimare i parametri del modello

# l'iterazione mpg:am1 non è significativa, la tolgo.
summary(m1)

# le table non evidenziano classi vuote o povere.
table(dati$vs)
table(dati$am)
table(dati$vs, dati$am)

# nota: lo standard error sembra alto (~11), ma è dovuto dal fatto che sto osservando non molte osservazioni.
# lo standard error è abastanza alto quando ho solo variabili fattore (perché ho meno valori... 0 1 vs 32 valori diversi).


# Nuovo modello senza l'interazione.
# am1 risulta poco significativo, ma lo tengo perché ho pochi dati e non voglio rimuovere informazione! Inoltre approssimo una 0-1 a una normale, quindi le stelline non sono super precise.
m2 <- glm(vs ~ mpg + am, data=dati, family=binomial)
summary(m2)

# provo il modello senza am
m3 <- glm(vs ~ mpg + am, data=dati, family=binomial)
summary(m3)

# confronto le devianze dei residui
25.533-20.646
# D m3 - D m2 ~ chi^2_1       (dovrebbe essere distribuito come chi quadrato con 1 grado libertà). 4.886 è molto lontano dalla media 1.
## p.value: P(chi^2_1 > 4.887)
# p value piccolo, tengo la variabile!
1 - pchisq(4.887, df=1)

# chi squared. tengo m2 rispetto a m3.
anova(m3, m2, test='Chisq')


## intervallo confidenza (IC) 95% per beta1 coeff associato a mpg
## stima - quantile * se;    stima + quantile*se
## stimo tramite il quantile della normale
0.6809 - qnorm(0.025)*0.2524
0.6809 + qnorm(0.025)*0.2524

#oppure in automatico:
# verosimiglianza profilo. "Profila sugli altri parametri" e considera solo beta1
# buona quando ho tanti parametri, e mi interssa un solo parametro.
confint(m2)
# la nostra. Mette tutti i parametri assieme
confint.default(m2)


## STIMA
valori.stimati <- predict(m2)
valori.stimati

# così ottengo le probabilità
probabilita.previste <- predict(m2, type='response')
probabilita.previste

# curva delle probabilità per andare da vs=0 a vs=1
plot(dati$mpg, dati$vs)
curve(predict(m2, newdata=data.frame(mpg=x, am='1'),type='response'), add=TRUE, col=1)
curve(predict(m2, newdata=data.frame(mpg=x, am='0'),type='response'), add=TRUE, col=2)

legend('bottomright', col=c(1,2), lty=c(2,2), legend=c('am=0', 'am=1'))



## costruisco un vettore con le probabilità 0 (se <0.5) o 1
previsioni <- rep(0, 32)
previsioni[probabilita.previste > 0.5] <- 1
previsioni

# 7 errori
table(previsioni, dati$vs)


## training set e test set
## 60% dati come training
n <- 32
set.seed(222)
selezione <- sample(n, 0.6*n, replace=FALSE)
selezione
sample(n, 0.6*n)
sample(n, 0.6*n)

training.set <- dati[selezione,]
test.set <- dati[-selezione,]
m2.train <- glm(vs ~ mpg + am, data=training.set, family=binomial)
m2.test <- glm(vs ~ mpg + am, data=test.set, family=binomial)

#prev. test
previsioni.test <- predict(m2.test, newdatA=test.set, type='response')
previsioni.test

length(previsioni.test)

# ora porto a 0-1
previsioni.test.vs <- rep(0, nrow(test.set))
previsioni.test.vs[previsioni.test>0.5] <- 1

# tasso di errore 23%, come il modello originale
table(previsioni.test.vs, test.set$vs)
3/13

# nb. posso ripetere con seed diverso o cambiare dimensioni del train/test


### libreria MASS
library(MASS)
m.lda <- lda(vs ~ mpg + am, data=training.set)

names(m.lda)
plot(m.lda)

previsioni.lda <- predict(m.lda, test.set)
previsioni.lda

previsioni.lda$class
table(previsioni.lda$class, test.set$vs)


## CURVA ROC
##pROC
install.packages('pROC')
library(pROC)
# todo definire roc
valori.roc  <- roc(test.set$vs, prvsioni.lds$posterior[,2])

plot(valori.roc, legacy.axes=TRUE)

# qda <- analisi discriminante quadratica