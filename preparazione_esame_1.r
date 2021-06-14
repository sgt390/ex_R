#install.packages("ISLR")

library(ISLR)
data(Hitters)
dim(Hitters)

names(Hitters)

## Model selection
summary(Hitters)

hitters <- na.omit(Hitters)

summary(hitters)

# Grafici: istrogramma e boxplot delle y
hist(hitters$Salary, prob=TRUE)
boxplot(hitters$Salary)

# Se non seguono la normale, provo log
par(mfrow=c(1,2))
hist(log(hitters$Salary), prob=TRUE)
boxplot(log(hitters$Salary))

hitters$Salary <- log(hitters$Salary)

# install.packages('leaps')
library(leaps)


set.seed(123)
m.backward<- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='backward')

summary(m.backward)

names(m.backward)
names(summary(m.backward))

summary(m.backward)$rsq
which.min(summary(m.backward)$rsq)
min(summary(m.backward)$rsq)

# visualizzo quali variabili vengono rimosse per ogni ciclo
plot(m.backward)
plot(m.backward, scale='adjr2')

(summary(m.backward)$adjr2)
which.max(summary(m.backward)$adjr2)
max(summary(m.backward)$adjr2)

which.min(summary(m.backward)$bic)

# vedo come cala il bic a seconda di quante variabili rimuovo
summary(m.backward)$bic
plot(summary(m.backward)$bic, type='l')
points(3, summary(m.backward)$bic[3], col='red', pch=19)


summary(m.backward)

modello <- lm(Salary ~ Hits + Walks + Years, data=hitters)

summary(modello)

par(mfrow=c(2,2))
plot(modello)
