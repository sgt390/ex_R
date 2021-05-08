# CORRELAZIONE DATI

cemento <- read.table('hald.dat')
cemento
colnames(cemento) <- c('calore','tric_all', 'tric_sil', 'ferrite', 'sil_bic')

boxplot(cemento$calore, col='grey')

# mette in relazione ogni variabile in una tabella
# si può notare se presente una relazione tra di loro
pairs(cemento)

m.cemento <- lm(calore ~ tric_all, data=cemento)
summary(m.cemento)

m.cemento2 <- lm(calore ~ tric_all + tric_sil, data=cemento)
summary(m.cemento2)

m.cemento3 <- lm(calore ~ tric_all + tric_sil + ferrite + sil_bic, data=cemento)
summary(m.cemento3)

# correlazione tra variabili
cor(cemento)

residui.standard <- rstandard(m.cemento2)
valori.previsti <- fitted(m.cemento2)
par(mfrow=c(2,2))
plot(residui.standard)
abline(h=0, lty=2)
plot(valori.previsti, residui.standard, xlab='valori previsti')
abline(h=0, lty=2)
plot(cemento$tric_all, residui.standard)
abline(h=0, lty=2)
plot(cemento$tric_sil, residui.standard)

abline(h=0, lty=2)
