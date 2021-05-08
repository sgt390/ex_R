library(MASS)
data(Boston)
Boston[1,]

m2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)

m3 <- lm(medv ~ poly(lstat, 3), data=Boston)

anova(m2,m3)

## estraiamo il valore dal summary del modello
summary(m2)$adj.r.squared

summary(m3)$adj.r.squared

logLik(m2)
## 'log Lik.' -1581.258 (df=4)
aic.m2 <- 2*2 - 2*logLik(m2)
aic.m2
## 'log Lik.' 3166.516 (df=4)
logLik(m3)

## 'log Lik.' -1568.898 (df=5)
aic.m3 <- 2*3 - 2*logLik(m3)
aic.m3
## 'log Lik.' 3143.796 (df=5)

# generalized linear model
m2.glm <- glm(medv ~ lstat + I(lstat^2), data=Boston)
m3.glm <- glm(medv ~ lstat + I(lstat^2) + I(lstat^3), data=Boston)

m2.glm$aic - m3.glm$aic
## [1] 22.72039
## oppure
extractAIC(m2) - extractAIC(m3)
## [1] -1.00000 22.72039

##### Ora facciamo Cross Validation

##install.packages('boot')
library(boot)

# quale cv usare? dipende dalla dimensione del dataset
dim(Boston)
## CV K fold
set.seed(123)
## default K=1 leave one out CV; cv.glm è una funzione per calcolare il CV (boot)
cv.m2 <- cv.glm(Boston, m2.glm, K=10)

names(cv.m2)
cv.m2

set.seed(123)
cv.m3 <- cv.glm(Boston, m3.glm, K=10)
cv.m3 <- glm(medv ~ lstat + I(lstat^2) + I(lstat^3), data=Boston)
cv.m3 <- cv.glm(Boston, m3.glm, K=10)
cv.m3$delta

cv.m3 <- cv.glm(Boston, m3.glm)
cv.m2 <- cv.glm(Boston, m2.glm)
cv.m2$delta

cv.m3$delta

## polinomi dal grado 1 al grado 6 ---> R2 aggiustato, AIC

r2.aggiustato <- rep(0, 6) # vettore di 6 zeri
aic <- rep(0,6)

cv.glm

# uso lm e non glm perché voglio R2 aggiustato, calcolato solo da lm
for( i in 1:6){
    modello <- lm(medv ~ poly(lstat, i), data=Boston)
    r2.aggiustato[i] <- summary(modello)$adj.r.squared
    aic[i] <- 2*(i) - 2*logLik(modello)
}
r2.aggiustato
aic
min(aic)
max(r2.aggiustato)

## pulizia
rm(list=ls())
ls()

