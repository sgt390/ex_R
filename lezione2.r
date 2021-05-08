## formato csv

dati <- read.csv('Gender_Discrimination.csv', sep=',')
# dati <- read.csv2('Gender_Discrimination.csv', sep=',')

summary(dati)

is.factor(dati$Gender)
table(dati$Gender)

dati$Gender <- as.factor(dati$Gender)

is.factor(dati$Gender)
levels(dati$Gender)

par(mfrow=c(1,2))
hist(dati$Salary, prob=TRUE)
boxplot(dati$Salary)

hist(log(dati$Salary), prob=TRUE)
boxplot(log(dati$Salary))

dati$Salary <- log(dati$Salary)

pie(table(dati$Gender))

# DEDUZIONI GRAFICHE
# I maschi prendono più soldi con variabilità maggiore (baffi lunghi) e mediana più alta (barra nera)
# I maschi hanno una mediana di esperienza più bassa e variabilità più alta
# Mi aspetto che: gender è molto importante per il salario; esperience è legato al gender
par(mfrow=c(1,2))
boxplot(dati$Salary ~ dati$Gender, col=c('pink', 'blue'))
boxplot(dati$Experience ~ dati$Gender, col=c('pink', 'blue'))
# Il grafico di dispersione è molto disperso, però non so dire se è così in generale, oppure se il problema è che manca il genere.
par(mfrow=c(1,2))
plot(dati$Experience, dati$Salary)
# ora coloriamo i dati
points(dati$Experience[dati$Gender=='Female'], dati$Salary[dati$Gender=='Female'], col='pink')
points(dati$Experience[dati$Gender=='Male'], dati$Salary[dati$Gender=='Male'], col='blue')
# pch: tipo del punto, bty='n': rimuove il box
legend('topleft', legend=c('Female', 'Male'), pch=c(1,1), col=c('pink', 'blue'), bty='n')
# Colore automatico dei punti nel grafico (1 rosso, 0 nero)
plot(dati$Experience, dati$Salary, col=dati$Gender)

# Modello
summary(dati)
modello <- lm(dati$Salary ~ dati$Gender + dati$Experience)
summary(modello)

boxplot(dati$Salary ~ dati$Gender, col=c('pink', 'blue'))
plot(dati$Experience, dati$Salary)
par(mfrow=c(2,2))
plot(modello)

## per female lsalary previsto=10.995323 + 0.017092 Experience
## per male lsalary previsto=10.995323 + 0.171236 + 0.017092 Experience
plot(dati$Experience, dati$Salary)
# ora coloriamo i dati
points(dati$Experience[dati$Gender=='Female'], dati$Salary[dati$Gender=='Female'], col='pink')
points(dati$Experience[dati$Gender=='Male'], dati$Salary[dati$Gender=='Male'], col='blue')
# pch: tipo del punto, bty='n': rimuove il box
legend('topleft', legend=c('Female', 'Male'), pch=c(1,1), col=c('pink', 'blue'), bty='n')

abline(10.995323, 0.017092, col='pink', lwd=3)
abline(10.995323+ 0.171236, 0.017092, col='blue', lwd=3)

# mettiamo in relazione Gender e Esperienza
modello2 <- lm(Salary ~ Gender * Experience, data=dati)
# Ora male non è più molto significativo...
summary(modello2)


plot(dati$Experience, dati$Salary)
points(dati$Experience[dati$Gender=='Female'], dati$Salary[dati$Gender=='Female'], col='pink')
points(dati$Experience[dati$Gender=='Male'], dati$Salary[dati$Gender=='Male'], col='blue')
# pch: tipo del punto, bty='n': rimuove il box
legend('topleft', legend=c('Female', 'Male'), pch=c(1,1), col=c('pink', 'blue'), bty='n')

abline(11.098597, 0.008577, col='pink', lwd=3)
abline(11.098597-0.026681, 0.008577+0.016480, col='blue', lwd=3)

par(mfrow=c(2,2))
plot(modello2)

## previsione Salary, per uomo/donna 20 anni di experience
## (rimuovo il logaritmo con exp())
exp(11.098597-0.026681 + 0.008577*20+0.016480*20)
exp(11.098597 + 0.008577*20)

exp(predict(modello2, newdata=data.frame(list(Gender='Male', Experience=20))))
exp(predict(modello2, newdata=data.frame(list(Gender='Female', Experience=20))))



plot(dati$Experience, exp(dati$Salary))
points(dati$Experience[dati$Gender=='Female'], exp(dati$Salary[dati$Gender=='Female']), col='pink')
points(dati$Experience[dati$Gender=='Male'], exp(dati$Salary[dati$Gender=='Male']), col='blue')
# pch: tipo del punto, bty='n': rimuove il box
legend('topleft', legend=c('Female', 'Male'), pch=c(1,1), col=c('pink', 'blue'), bty='n')

# curve() funziona in due modi:
# se la voglio disegnare su un grafico già creato, usa l'ascissa già nel grafico
# altrimenti devo specificarla.
# Le "x" sono le ascisse già presenti
curve(exp(11.098597 + 0.008577*x), col='pink', add=TRUE)
curve(exp(11.098597-0.026681 + (0.008577+0.016480)*x), col='blue', add=TRUE)

