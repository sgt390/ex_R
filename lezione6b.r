## DATSET: Baseball
library(ISLR)
data(Hitters)
dim(Hitters)
?Hitters


# Problema: Non abbiamo 59 valori di salary
# cosa fare? mancano valori sulle x o sulle y?
# aggiungiamo dei sostituti (soggetto a errore),
# oppure rimuoviamo delle righe.
# Noi li eliminiamo
summary(Hitters)

hitters <- na.omit(Hitters)
dim(Hitters)
dim(hitters)

hitters[1:3,]

# Come procedere? Model selection e Regolarizzazione...


# Grafici...
## la normale non c'è (al massimo una semi-normale)
## c'è un piccolo grouppo di giocatori pagati molto bene.
## ci suggerisce di usare il logaritmo
par(mfrow=c(1,2))
hist(hitters$Salary, prob=TRUE)
boxplot(hitters$Salary)

## con log è meglio!
par(mfrow=c(1,2))
hist(log(hitters$Salary), prob=TRUE)
boxplot(log(hitters$Salary))

hitters$Salary <- log(hitters$Salary)


#install.packages('leaps')
library(leaps)

# nvmax=19 covariate da usare
# regsubsets fa selezione automatica nel modello lineare. non c'è una versione per
# 0 1
m.forward <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='forward')
## logistica --> logit(P(Y=1)) modello lineare in X

# vedo quali variabili forzo tenere/non tenere
m.forward

summary(m.forward)

names(summary(m.forward))

summary(m.forward)$rss
summary(m.forward)$bic
summary(m.forward)$adjr2

which.min(summary(m.forward)$rss)
min(summary(m.forward)$rss)

which.min(summary(m.forward)$bic)

coef(m.forward, 4)

## matrice delle varianze e covarianze
vcov(m.forward, 4)

se <- sqrt(diag(vcov(m.forward, 4)))
se

coef(m.forward, 4)

## plot modello tramite selezione automatica...
## ordinate: valori dello strumento che usuamo per stimare (in questo caso BIC, dal basso all'alto)
## "ascisse": etichetta 
# dal basso all'alto, in basso c'è il modello vuoto
# all'alto ho le variabili/modello vincitore
plot(m.forward)

plot(m.forward, scale='adjr2')

which.max(summary(m.forward)$adjr2)

summary(m.forward)$bic
# al crescere delle covariate il bic scende molto, poi torna a salire con troppe covariate
plot(summary(m.forward)$bic, type='l') # type='l' mette  una linea sui punti

# mette un punto sul 4th modello
points(4, summary(m.forward)$bic[4], col='red', pch=19)


## posso usare anche backward o mista
m.backward <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='backward')
which.min(summary(m.backward)$bic)

coef(m.backward, 8)
coef(m.forward, 4)

m.mista <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='seqrep')

# torniamo alla forward...

modello <- lm(formula = Salary ~ Hits + Years + CRuns + PutOuts, data=hitters)

summary(modello)

par(mfrow=c(2,2))
plot(modello)


# now we can click the points and see who they represent
plot(Hitters$CAtBat, Hitters$Salary)
identify(Hitters$CAtBat, Hitters$Salary, n=4, labels=rownames(Hitters), plot=TRUE)

par(mfrow=c(2,2))
plot(modello)



plot(Hitters$Salary)
identify(Hitters$Salary, n=4, labels=rownames(Hitters), plot=TRUE)

## come migliorare il modello? - queste modifiche vanno fatte PRIMA del model selection!!
# rimuovere soggetti lontani dal corpo centrale
# modellare alcune relazioni