## rattle: vini in italia
## CLASSIFICAZIONE con 
## NON POTREI USARE LA REGRESSIONE LOGISTICA, PERCHE' HO 3 CLASSI (dovrei usare la Reg. Logistica polinomiale)
install.packages('rattle')
## library(rattle)
## data(wine)

data(wine, package='rattle')

dim(wine)

summary(wine)

is.factor(wine$Type)
table(wine$Type)

# come rappresento le variabili fattore?
## barplot(table(wine$Type)) stesso di quello sotto
plot(wine$Type)  # grafico a barre/nastri. non esiste ascissa ma solo ordinata, 1 2 3 sotto sono delle etichette, non numeri
pie(table(wine$Type))

## pairs(dataset - prima colonna)
# è illegibile perché troppe variabili
pairs(wine[,-1])
# lo spezzo (2 e 7 inclusi!!)
pairs(wine[, 2:7]) 
pairs(wine[,8:13])


# medie per ogni variabile
# coefficenti per ogni variabile
# Proportion of trace: come funzionano le funzioni discriminanti. La prima funzione disriminante discrimna il 68.75%.
m <- lda(Type ~ ., data=wine)
m

# la funzione discriminante LD1, cioè quello da 68.75%, funziona abbastanza bene. Si guarda da sinistra a destra, quando gli "1" e i "2" si sovrappongono, ho una classificazione "non buona".
# per LD2 devo guardare dal basso all'alto. Discrimina bene il 2. Non riesce a distinguere il gruppo 1 dal gruppo 3, perché sovrapposti!.
# se ci fossero più livelli sarebbe difficile usare questo tipo di grafico perché avrei troppe dimensioni
plot(m)


# ISTOGRAMMI
previsioni <- predict(m)
previsioni

# class: classificazione finale di R
# posterior: probabilità a posteriori per ogni classe
# $x valori per ciascuna funzione discriminante lineare

# Istogramma con la funz. discriminante 1, rispetto al Tipo di vino. E' analogo al plot visto prima
# LD1 è brava a discriminare le parti "centrali", ci sono delle sovrapposizioni nelle code.
ldahist(data=previsioni$x[,1], g=wine$Type)

# funzione discriminante 2 è brava a distinguere il gruppo 2 dagli altri due (cioè complementa LD1, che non è brava a distinguere le code del gruppo 1 con 2, e 3 con 2).
ldahist(data=previsioni$x[,2], g=wine$Type)

