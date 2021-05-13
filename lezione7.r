## DATSET: Baseball
library(ISLR)
data(Hitters)
dim(Hitters)

hitters <- na.omit(Hitters)
dim(hitters)

## ridge e lasso
## glmnet
install.packages('glmnet')

library('glmnet')

?glmnet

summary(hitters)

# sistema le variabili da solo. Tolgo la y per didge (chiesto da glmnet)
X <- model.matrix(Salary ~ ., data=hitters)[,-1]

par(mfrow=c(1,3))
hist(hitters$Salary, prob=TRUE)
hist(log(hitters$Salary), prob=TRUE)
# R lavora con le variabili scalate scale(.).
# Possiamo evitare di scalare a mano le variabili!
hist(scale(hitters$Salary), prob=TRUE)
y <- hitters$Salary

m.ridge <- glmnet(X, y, alpha=0)
m.ridge
# lambda=0 -> minimi quadrati
names(m.ridge)

modello.mq <- glm(Salary ~ ., data=hitters)

summary(modello.mq)

# vedo cosa succede a ogni variabile al crescere di lambda
plot(m.ridge, xvar='lambda', lab=TRUE, xlab=expression(log(lambda)))

## cross validation
set.seed(2906)
cv.ridge <- cv.glmnet(X, y, alpha=0)
## vediamo i valori più importanti
cv.ridge
# così si vede di più
names(cv.ridge)
plot(cv.ridge)
263/20

# λ minimo e lambda standard error cambia molto!
# forse è meglio il λ minimo
cv.ridge$lambda.min
cv.ridge$lambda.1se

best.lambda <- cv.ridge$lambda.min

m.ridge.min <- glmnet(X, y, alpha=0, lambda=best.lambda)
m.ridge.min

coef(m.ridge.min)
ls()

# vediamo cosa succede ai coefficenti (colonna ←)
# rispetto ai coeff. originali (colonna →)
cbind(coef(m.ridge.min), coef(modello.mq))

plot(m.ridge, xvar='lambda')
abline(v = log(best.lambda), lty=2, col='red')

# ci mangiamo 50% della devianza!
plot(log(m.ridge$lambda), m.ridge$dev.ratio, type='l')
abline(v = log(best.lambda), lty=2, col='red')


# LASSO

m.lasso <- glmnet(x=X, y=y, alpha=1)
# dal basso. ho 19 variabili e λ bassa.
# salendo, faccio selezione (scarto variabili) e λ cresce
m.lasso

set.seed(2906)
cv.lasso <- cv.glmnet(X, y, alpha=1)
cv.lasso
# guardando la tabella m.lasso posso trovare lambda minimo e lambda 1se
# meglio minimo perché scarto più variabili e comunque modello più semplice
best.lambda.lasso <- cv.lasso$lambda.min


m.lass.min <- glmnet(X, y, alpha=1, lambda=best.lambda.lasso)

# se costruisco un modello che viene da lasso
# non posso fare nuove modifiche perché mischierei p-value
# e tecniche di CV.
m.lasso <- lm(Salary ~ ., data=hitters)
summary(m.lasso)
# a volte è meglio fare le cose a mano se la regolarizzazione 
# non da risultati particolarmente buoni