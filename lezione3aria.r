## aria.RData
load('aria.RData')

aria[1,]

## mancano dei dati!
## 2 possibilità: elimino le righe (na.omit)
##    oppure aggiungo nuovi dati tramite la sua media, mediana, media valori vicini... (risulta in molti problemi). Non si tiene conto della variabilità del problema.
aria <- na.omit(aria)

aria$mese <- as.factor(aria$mese)
is.factor(aria$mese)

levels(aria$mese)

levels(aria$mese) <- c('maggio','giugno', 'luglio', 'agosto', 'settembre')

summary(aria)

par(mfrow=c(1,2))
hist(aria$ozono, prob=TRUE)
boxplot(aria$ozono)

pairs(aria)

boxplot(aria$ozono ~ aria$mese)

# mediane alte, relazione tra le variabili?(lo provo)
par(mfrow=c(1,2))
boxplot(aria$radiazione ~ aria$mese, xlab='', las=2)
boxplot(aria$vento ~ aria$mese)

# plot di raziazione e ozono, colorando i punti per i mesi. se faccio delle rette, queste si intersecano, quindi mese-radiazione è probabilmente correlato
plot(aria$radiazione, aria$ozono, col=aria$mese)
legend('topleft', col=1:5, pch=1, legend=levels(aria$mese))
plot(aria$vento, aria$ozono, col=aria$mese)
legend('topleft', col=1:5, pch=1, legend=levels(aria$mese))

modello1 <- lm(ozono ~ radiazione + vento + mese + radiazione:mese + vento:mese, data=aria)
## stessa cosa di: radiazione*mese + vento*mese
## vento*mese (equivalente)--> vento + mese + vento:mese

# se siamo a maggio e passiamo ad agosto, il vento riduce molto di più l'ozono (dato dalla colonna Pr(), che se è molto differente dal livello base alle altre, allora la significabilità è alta). Questo era anche previsto dal suo boxplot.
summary(modello1)

par(mfrow=c(2,2))
plot(modello1)
pairs(aria)

# i seguenti comandi fanno la stessa cosa
modello2 <- lm(ozono ~ radiazione + vento + mese + radiazione:mese + vento:mese + I(radiazione^2)+ I(vento^2), data=aria)
# modello2 <- update(modello1, . ~ . + I(radiazione^2) + I(vento^2))

summary(modello2)

# posso mettere  in relazione il modello più complesso modello2 con modello1?
anova(modello2, modello1)

# posso rimuovere vento:mese
summary(modello2)

modello3 <- update(modello2, . ~ . - vento:mese)

summary(modello3)

# controllo se posso metterli in relazione? si
anova(modello3, modello2)

par(mfrow=c(2,2))
plot(modello3)

par(mfrow=c(1,2))
plot(aria$ozono, fitted(modello1))
abline(0,1)
plot(aria$ozono, fitted(modello3))
abline(0,1)

# come rappresentare il grafico finale
par(mfrow=c(1,1))
aria[1:4,]
plot(aria$radiazione, aria$ozono)
media.vento <- mean(aria$vento)
vettore <- coef(modello3)
vettore

## maggio - gli indici sono la riga nella summary del modello
curve(vettore[1]+ vettore[2]*x + vettore[3]*media.vento + vettore[8]*(x^2)+ vettore[9]*(media.vento^2), col=1, lwd=3, add=TRUE)
## giugno
curve(vettore[1]+ (vettore[2]+vettore[10])*x + vettore[3]*media.vento + vettore[4] + vettore[8]*(x^2)+ vettore[9]*(media.vento^2), col=2, lwd=3, add=TRUE)
## luglio
curve(vettore[1]+ (vettore[2]+vettore[11])*x + vettore[3]*media.vento + vettore[4] + vettore[8]*(x^2)+ vettore[9]*(media.vento^2), col=2, lwd=3, add=TRUE)
## agosto
curve(vettore[1]+ (vettore[2]+vettore[11])*x + vettore[3]*media.vento + vettore[4] + vettore[8]*(x^2)+ vettore[9]*(media.vento^2), col=2, lwd=3, add=TRUE)

vettore[3]
