library(MASS)
data(Boston)
?Boston
summary(Boston)
Boston[1:3,]

# Modello che lega medv a lstat
# medv e lstat=X
# Y=beta0 + beta1 + epsilon
summary(Boston$medv)
n <- nrow(Boston)
hist(Boston$medv, prob=TRUE, main='Istogramma', xlab='prezzo mediano delle abitazioni')

## Y e X
plot(Boston$lstat, Boston$medv, main='diagramma di dispersione medv vs lstat', xlab='% proprietari basso stato socio-economico', ylab='prezzo mediano delle abitazioni', pch=19, cex=0.5)

# Y = beta0 + beta1X + espilon
# beta1x = cov(X, Y)/Var(X)
cov(Boston$lstat, Boston$medv)
# var(Boston$lstat) = sum((lstat-mean.lstat)^2)/(n-1)
# se voglio la varianza che conosciamo: moltiplico per (n-1)/n (stessa cosa per cov)
var(Boston$lstat)
# se le metto assieme, il fattore (n-1) si elimina, quindi non serve farlo a mano
# beta1 = cov/var
cov(Boston$lstat, Boston$medv)/var(Boston$lstat)

modello <- lm(medv ~ lstat , data=Boston)
modello
summary(modello)
# epsilon ~ N(0, sigma2)
# summary dovrebbe avere mediana 0 (o vicino)
# se lstat cresce di 0.95 unità --> medv decresce di 0.95*1000 dollari
# RSE stima di sigma
# multiple R-squared (R quadro, oppure R2)
# adjusted R-squared (R2 migliorato)

names(modello)
modello$coefficients

# stime
valori.stimati <- fitted(modello)
## modello$fitted.values
## aggiungo (lstat, medv stimato) = (x, y cappello)
points(Boston$lstat, valori.stimati, pch='x', col='green')
abline(34, -0.95, col='red', lty=2, lwd=3)

vcov(modello)
# Intervallo di confidenza
# 95%
-0.95 - qt(0.975, n-2)*0.039
-0.95 + qt(0.975, n-2)*0.039
# oppure in automatico
confint(modello)
confint(modello, level=0.90)

# H0: beta1=0 vs H1: beta1 \not =0 (default R)
# H0: beta1=-1 vs H1: beta1 \not =-1 (custom, può essere quello che voglio)
# t = (beta1 - (-1))/se    #se: standard error
(-0.95-(-1))/0.039
qt(0.975, n-2)
# p-value (guardo cosa c'è a dx e sx di 1.28, nella t-di-student)
2*min(pt(1.28, n-2), 1-pt(1.28, n-2))  # ottengo 0.201134, valore grande, quindi non posso rifiutare la variabile


# Intervallo di previsione (linee nere)
predict(modello, newdata=data.frame(list(lstat=c(5,10,25))))
y0 <- predict(modello, interval='prediction')
y0[1:5,]
points(Boston$lstat, y0[,'lwr'], col='black', lwd=3, type='l')
points(Boston$lstat, y0[,'upr'], col='black', lwd=3, type='l')

# residui
residui <- modello$residuals
residui <- residuals(modello)
# divido la finestra in 2 righe e 2 colonne
par(mfrow=c(2,2))
hist(residui, prob=TRUE)
plot(residui, pch=19, cex=0.5)
abline(h=0, col='red', lwd=3) # abline:"fiex" line; lwd:line size
plot(fitted(modello), residui, pch=19, cex=0.5)
plot(Boston$lstat, residui, pch=19, cex=0.5)

# standardizzazione modello
r.standard <- rstandard(modello)
summary(r.standard)
summary(residui)

hist(r.standard, prob=TRUE)
plot(r.standard, pch=19, cex=0.5)
plot(fitted(modello), r.standard, pch=19, cex=0.5)
plot(Boston$lstat, r.standard, pch=19, cex=0.5)
plot(modello)


hist(r.standard, prob=TRUE)
qqnorm(r.standard)
qqline(r.standard)
# punti influenti: un punto è veramente influente quando 
# il suo valore è alto (sotto 0.1 è non inflente)
hist(r.standard, prob=TRUE)
hist(r.standard, prob=TRUE)
plot(modello)
# distanza di Cook
plot(modello, 4)

# Rendiamo il modello più grande
# aggiungiamo criminalità (crim)
summary(Boston)
plot(Boston$crim, Boston$medv, pch=19, cex=0.5)
plot(Boston$lstat, Boston$medv, pch=19, cex=0.5)

modello.mv <- lm(medv ~ lstat + crim, data=Boston)
summary(modello.mv) # modello nuovo
summary(modello)    # modello vecchio
# standard error è un po' diminuito, R2 è simile, F-statistic(significabilità dei parametri,
# paragone modello con modello con solo intercetta, p-value è bassa => il modello senza variabili non va bene, non mi dice quale variabile è significativa)

# criminalità ha un puntino, però non va buttato subito
# Possibilità:
# 1) manca qualcosa al modello, che può essere catturata dalle altre variabili in un modello
#    più grande (criminalità è spuria).
# 2) oppure, criminalità serve effettivamente.

# 1) facciamo un modello più complesso
# modello polinomiale con lstat e lstat^2
modello2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(modello2)

# nel primo grafico, la curva è un po' più piatta
# il grafico dei quantili (2°) è migliorato, anche se ci sono ancora puntini sulla destra strani
# il quarto grafico è migliorato, ora tutti i punti sono più bassi. I punti continuano ad essere non influenti.

# i grafici sono tutti migliorati, ma il grafico Quantile Quantile è da sistemare
plot(Boston$crim, Boston$medv, pch=19, cex=0.5)
plot(modello2)

# Esiste una funzione che confronta i grafici: statistica F (RSS0 modello più semplice, RSS modello più complesso)
## F = RSS (RSS0-RSS)/RSS * (n-p-1)/q
## n=506, p=2 (covariate), q=1 (intercetta); 6.216 è il RSE e ricavo il rss con formula inversa; 504 gradi di libertà
rss0 <- (6.216^2) * 504
## RSE = sqrt(rss/(n-p-1))
rss <- (5.524^2) * 503
rss0
rss

F = (rss0-rss)/rss * (506-2-1)
F
## statistica F con q, n-p-1 gradi di libertà (gdl)... F (1, 503)
# --- non posso eliminare la variabile lstat
hist(rf(1000, 1, 503), prob=T)
# quantile 0.95 a sinistra, 1 e 503 gradi di libertà
qf(0.95, 1, 503)

points(3.86, 0, col=2)
points(3.86, 0, col=2, pch=19)
# p-value: prob di trovare qualcosa più estremo di 135.183
# Prob(F > 135.183)
1 - pf(135.138, 1, 503)
summary(modello2)

# T-value^2 = F

# R esegue tutto questo in automatico!
# ANalysis Of VAriance anova(modello piccolo, modello grande)
# posso buttare via qualcosa del modello più grande?
# il modello più piccolo deve essere contenuto nel modello più grande (e.g. modello grande contiene X1 X2 X3, il modello più piccolo deve contenere solo i coefficenti contenuti nel modello grande e.g. X1. Se il modello piccolo contiene X1 X4 non si può fare!)

# Df: quanti parametri sto testando
# posso tenere il modello più piccolo semplificato, oppure no?

# possibile errore: inserire i modelli al contrario, esce una somma dei quadrati negativa
anova(modello, modello2)

# polinomio


par(mfrow=c(2,2))
plot(modello2)
modello3 <- lm(medv ~ lstat + I(lstat^2) + crim, data=Boston)
summary(modello3)

# proviamo con lstat^3. La curva è più flessibile (ci si aspetta che ci sia overfitting)
modello3 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + crim, data=Boston)
plot(Boston$lstat, Boston$medv, pch=19, cex=0.5)

# ma se aggiungiamo nuove variabili, l'overfitting viene ridotto
summary(Boston)

# esistono strumenti automatici per la selezione delle variabili
# manualmente, posso usare la procedura forward, backward, mista
# forward: creo il modello base, aggiungo una variabile alla volta e calcolo le statistiche per capire se tenere o meno;
# backward: inserisco tutte le variaibli, e le tolgo una alla volta