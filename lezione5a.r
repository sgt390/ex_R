# regressione logistica -> più teoria sotto, usabile però se ho dati "corretti". Altrimenti devo usare le funzioni discriminanti lineari.
install.packages('ISLR')
library(ISLR)
data(Auto)
?Auto


summary(Auto)



new.mpg <- rep(1, 392)
new.mpg[Auto$mpg < median(Auto$mpg)] <- 0
table(new.mpg)

dati <- data.frame(new.mpg=new.mpg, Auto[,c('displacement', 'horsepower', 'origin')])
summary(dati)

dati$origin <- as.factor(dati$origin)
is.numeric(dati$origin)

levels(dati$origin)
levels(dati$origin) <- c('America', 'Europa', 'Giappone')
summary(dati)

# boxplot invertendo il ruolo della x e y, perché lavoro con classficazione

# displacement sarà significativo per la classificazione perché i boxplot sono molto distanti
# giappone: la scatola grande mangia quella piccola, potrebbe ridurre l'effetto di displacement, ma gli altri due sono buoni.
# il coefficiente sarà negativo
## nell'insieme, guardando mpg=0: ho differenze di comportamento tra stati
## guardando mpg=1 anche qui ho variazioni a seconda dell'origine
## l'iterazione quindi è una buona idea.

par(mfrow=c(1,3))
boxplot(displacement ~ new.mpg, data=dati, subset=(origin=='America'))
boxplot(displacement ~ new.mpg, data=dati, subset=(origin=='Europa'))
boxplot(displacement ~ new.mpg, data=dati, subset=(origin=='Giappone'))

boxplot(horsepower ~ new.mpg, data=dati, subset=(origin=='America'))
boxplot(horsepower ~ new.mpg, data=dati, subset=(origin=='Europa'))
boxplot(horsepower ~ new.mpg, data=dati, subset=(origin=='Giappone'))


table(dati$new.mpg, dati$origin)
# mi da una idea visiva di come son fatte le classi tra variabili fattore. E' come una tabella ma più visiva
mosaicplot(table(dati$new.mpg, dati$origin))

# MODELLO
## displacement + origin + displ:origin + horsepower + origin + hors:origin
## R capisce che una delle due "origin" va tolta
m <- glm(new.mpg ~ displacement*origin + horsepower*origin, data=dati, family=binomial)

## nessuna var va tolta per il principio di gerarchia (le var con iterazione significative si tengono).
summary(m)


# update m con displacement al quadrato e horsepower al quadrato.
# displacement ha coeff. negativo
# displcament^2 ha coeff positivo => al crescere di displacement ho che la Y decresce, non linearmente perché la decrescita diventa più lenta.
m2 <- update(m, . ~ . + I(displacement^2) + I(horsepower^2))
summary(m2)
m3 <- update(m, . ~ . + I(displacement^2))
## residual deviance è confronto con il modello che ha tutto dentro: chi2 con 382 gradi di libertà --> media parti a 382. Il p-value di questo è quasi vicino a 1, perché è probabile trovare valori a destra di 185.
summary(m3)

# m3 è più piccolo, e il migliore
anova(m3, m2, test='Chisq')

# curva ROC
install.packages('pROC')
library('pROC')
prob.stimate <- predict(m3, type='response')
valori.roc <- roc(dati$new.mpg, prob.stimate)
valori.roc
plot(valori.roc, legacy.axes=TRUE, xlim=c(1.0, 0.0), print.auc=TRUE,
auc.polygon=TRUE)
# n.b. quì è meglio usare regressione logistica rispetto all'analisi lineare altrimenti violiamo la normalità.