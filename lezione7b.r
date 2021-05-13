load('Leukemia.RData')

# dataset particolare. Creato come una lista
# cioè un oggetto pieno di cose
dim(Leukemia)

is.list(Leukemia)
# Cosa c'è dentro?
names(Leukemia)
# n=72 pazienti, p=3571 dati genetici
dim(Leukemia$x)
Leukemia$y

# che modello?

# il modello di regress. logistica non funzionerà...
m <- glm(y ~ x, data=Leukemia, family='binomial')
# molte risposte (pvalue, coefficenti,...) sono NA!!
# perché mancano dati. Può stimare fino a n=72-1 (+intercetta)
# covariate. Inoltre li stima male. Gli SE sono enormi, in pratica usa
# un input per var.
summary(m)

# In questo caso la normalizzazione è indispensabile.
# abbiamo già X e y divisi. Non ci sono fattori (solo ℝ)
m.ridge <- glmnet(Leukemia$x, Leukemia$y, alpha=0, family=binomial)
plot(m.ridge, xvar='lambda')

cv.ridge <- cv.glmnet(Leukemia$x, Leukemia$y, alpha=0, family='binomial')
cv.ridge

# ora cerchiamo dove sta cv.ridge$min. 
# non mi piace perché ho troppe variabili 
m.ridge

m.lasso <- glmnet(Leukemia$x, Leukemia$y, alpha=1, family='binomial')
cv.lasso

set.seed(2906)
cv.lasso <- cv.glmnet(Leukemia$x, Leukemia$y, alpha=1, family='binomial')
cv.lasso
best.lambda <- cv.lasso$lambda.min
best.lambda

plot(cv.lasso)

m.lasso.min <- glmnet(Leukemia$x, Leukemia$y, family='binomial', alpha=1, lambda=best.lambda)
m.lasso.min
# posizioni uguali a zero
id.zero <- which(coef(m.lasso.min) == 0)
length(id.zero)
nonzero <- length(coef(m.lasso.min)) - length(id.zero) 
nonzero

# 1 è l'intercetta. Visualizzo chi non è zero
id.nonzero <- which(coef(m.lasso.min) != 0)
id.nonzero

id.nonzero <- which(coef(m.lasso.min)!=0)
varnames <- rownames(coef(m.lasso.min))[id.nonzero]
values <- coef(m.lasso.min)[id.nonzero]
names(values) <- varnames
values

# ora possiamo fare una reg. logistica
# seleziono le colonne meno l'intercetta
new.x <- Leukemia$x[, id.nonzero[-1]]
dim(new.x)

# warning: l'algoritmo non è riuscito a convergere.
# pochi dati per le varie classi. Problema di sparsità delle classi
# bisogna sistemare i dati.

# come prima, non devo modificare il modello a mano!!
# posso solo descrivere il modello così come è
m <- glm(y ~ new.x, data=Leukemia, family='binomial')

library(brglm2)
m2 <- glm(y ~ new.x, data=Lekuemia, family='binomial', method='brglmFit')