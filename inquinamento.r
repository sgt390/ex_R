load('inquinamento.RData')
dim(inquinamento)

summary(inquinamento)

is.factor(inquinamento$SO2)

dati <- inquinamento[,c('mortalita', 'precipitazioni', 'umidita', 'HC', 'NOX', 'SO2')]

## reg lineare
par(mfrow=c(1,2))
# mortalità (y) è una normale? i dati son vicino a zero?
# si è una normale, lontana da zero
hist(dati$mortalita, prob=TRUE)
boxplot(dati$mortalita, prob=TRUE)

# relazione positiva
plot(dati$precipitazioni, dati$mortalita, main='precipitazioni')
# prob. spline
plot(dati$umidita, dati$mortalita, main='umidità')
# prob. spline
plot(dati$HC, dati$mortalita, main='hc')
# la variabilità cresce con "Si"
boxplot(dati$mortalita ~ dati$NOX, main='nox')
# meno variabilità per SI, SO2 sarà significativo
boxplot(dati$mortalita ~ dati$NOX, main='nox')


# altre idee per visualizzare dati, per avere idea di iterazioni tra variabili
# per precipitazioni, sembra che ci sia una interazione con NOX
# Proviamo tutte le interazioni, anche se non sembra che tutte servano
plot(dati$precipitazioni, dati$mortalita, main='precipitazioni', col=dati$NOX)
plot(dati$umidita, dati$mortalita, main='umidità', col=dati$NOX)
plot(dati$HC, dati$mortalita, main='umidità', col=dati$NOX)
# SO2...
plot(dati$precipitazioni, dati$mortalita, main='precipitazioni', col=dati$SO2)
plot(dati$umidita, dati$mortalita, main='umidità', col=dati$SO2)
plot(dati$HC, dati$mortalita, main='umidità', col=dati$SO2)

# si legge come "livello_dannoso_NOX.livello_dannoso_SO2"
# controlliamo interazioni tra nox e so2
# sembra che ci sia interazione tra le variabili
# p.e. fissato nox=si allora passando da so2=si a so2=no c'è un salto
# anche viceversa. la linea significa che ho una sola osservazione
boxplot(dati$mortalita ~ dati$NOX : dati$SO2)

m <- lm(mortalita ~ HC*NOX + HC*SO2 + precipitazioni*NOX + precipitazioni*SO2 +
 + umidita*NOX + umidita*SO2 + NOX*SO2, data=dati)

# Analisi modello m:
# tante variabili possono essere buttate
# alcuni standard errors son troppi (poche osservazioni)
# andando avanti semplificherò il modello, quindi è possibile che
# gli std. error calino. butto via un paio di valori
summary(m)
m <- lm(mortalita ~ HC*NOX + precipitazioni*NOX +
 + umidita*NOX + umidita*SO2 + NOX*SO2, data=dati)
summary(m)
# tolgo umidità e nox
m <- lm(mortalita ~ HC*NOX + precipitazioni*NOX +
 + umidita*NOX + NOX*SO2, data=dati)
summary(m)
# NON TOGLIERE VARIABILI CON INTERAZIONE!!
# non tolgo completamente la variabile se scompare, quindi lasciare
# la var. singola (senza *)
m <- lm(mortalita ~ HC*NOX + precipitazioni*NOX + umidita + SO2, data=dati)
summary(m)

m <- lm(mortalita ~ HC*NOX + precipitazioni + umidita + SO2, data=dati)
summary(m)

# occhio, std error sembra grande, però i dati son pochi e la stima associata è alta
# quindi è ok.
m7 <- lm(mortalita ~ HC*NOX + precipitazioni + SO2, data=dati)
summary(m)
# se i grafici iniziali lo suggeriscono, aggiungo i quadrati delle var.

par(mfrow=c(2,2))
plot(m7)
# non abbiamo un punto influente perché è tutto sotto 1
plot(m7, which=4)

# è utile una spline?
# riprendiamo il plot delle precipitazioni e mortalità:
plot(dati$precipitazioni, dati$mortalita)
plot(dati$umidita, dati$mortalita)
plot(dati$HC, dati$mortalita)

# smoothing spline per precipitazioni, umidita e HC
library(leaps)
library(gam) #funzione s()
sp.precipitazioni <- smooth.spline(x=dati$precipitazioni, y=dati$mortalita, cv=TRUE)
sp.precipitazioni #2 gradi di libertà
sp.HC <- smooth.spline(x=dati$precipitazioni, y=dati$mortalita, cv=TRUE)
sp.HC #2 gradi di libertà

# modifico m7 con le spline
m7
# ho diverse possibilità:
# - posso buttare tutto dentro con le smoothing splines
# - modifico solo le variabili per aggiungere le splines
m.gam <- lm(mortalita ~ s(HC, 33) * NOX + s(precipitazioni, 2) + SO2, data=dati)





# ora proviamo le altre variabili
library(glmnet)

X <- model.matrix(glm(mortalita ~ . + HC:NOX, data=inquinamento))
m.lasso <- glmnet(X, inquinamento$mortalita, alpha=1)

set.seed(222)
# lasso non fa alcuna selezione!
m.lasso.cv <- cv.glmnet(X, inquinamento$mortalita, alpha=1)
m.lasso.cv
plot(m.lasso, xvar='lambda')
abline(v=log(0.1429), col=2)
abline(v=log(1.1067), col=2)

# proviamo a stimare il modello sulle variabili selezionate (tutte meno 1)
m.lasso <- glmnet(X, inquinamento$mortalita, alpha=1, lambda=1.1067)
coef(m.lasso)

#probiamo selezione automatica. così seleziona 8 variabili
library(leaps)
m.forward <- regsubsets(mortalita ~ . + HC:NOX, data=inquinamento, nvmax=15, method='forward')
m.forward
plot(m.forward)

summary(m.forward)$bic
which.min(summary(m.forward)$bic)
coef(m.forward, 8)

# vediamo anche la regr. a componenti principali
library(pls)
set.seed(222)
# servono 11 componenti... non c'è molto vantaggio
m.pcr <- pcr(mortalita ~ . + HC:NOX, validation='CV', data=inquinamento)
summary(m.pcr)

selectNcomp(m.pcr, method='onesigma', ncomp=15)
# le componenti sono un po' sovrapposte, diveramente dal solito.
# la prima componente e seconda viaggiano assieme, e danno più peso alla variabile 12 e 14
# la quinta comp. principale viaggia da sola
# è difficile interpretare questo grafico e assegnare le componenti alle variabili
coefplot(m.pcr, ncom=1:5, legendpos='topright')
