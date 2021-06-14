load('inquinamento.RData')

names(inquinamento)

dati = inquinamento[, c('mortalita', 'precipitazioni', 'umidita', 'HC', 'NOX','SO2')]
summary(dati)
dim(dati)

# Y
hist(dati$mortalita, prob=TRUE)
boxplot(dati$mortalita, prob=TRUE)

# X ~ Y
par(mfrow=c(2,2))
plot(dati$precipitazioni, dati$mortalita)
plot(dati$umidita, dati$mortalita)
plot(dati$HC, dati$mortalita)

par(mfrow=c(1,2))
boxplot(dati$mortalita ~ dati$NOX)
boxplot(dati$mortalita ~ dati$SO2)

# X ~ X
plot(dati$precipitazioni, dati$umidita)
pairs(dati[,c('mortalita','precipitazioni', 'umidita', 'HC')])

# models

m <- lm(mortalita ~ precipitazioni + umidita + HC + 
        + precipitazioni*SO2 + precipitazioni*NOX
        + umidita*SO2 + umidita*NOX
        + HC*SO2 + HC*NOX, data=dati)
summary(m)
m <- lm(mortalita ~ precipitazioni + umidita + HC + 
        + precipitazioni*SO2 +
        + umidita*SO2 + umidita*NOX
        + HC*SO2 + HC*NOX, data=dati)
summary(m)
m <- lm(mortalita ~ precipitazioni + umidita + HC +
        + umidita*NOX +
        + HC*SO2 + HC*NOX, data=dati)
summary(m)

m <- lm(mortalita ~ precipitazioni + umidita + HC +
        + HC*SO2 + HC*NOX, data=dati)
summary(m)
m <- lm(mortalita ~ precipitazioni + umidita + HC +
        + HC*NOX, data=dati)
summary(m)
m <- lm(mortalita ~ precipitazioni + HC +
        + HC*NOX, data=dati)
summary(m)

# STESSA ANALISI CON BRIDGE O LASSO

# STESSA ANALISI CON COMP. PRINCIPALI
