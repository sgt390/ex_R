library(ISLR)
data(Auto)

head(Auto)
cols <- c('mpg', 'displacement', 'horsepower', 'origin')

dati <- Auto[, cols]
head(dati)

new.mpg <- rep(1, length(Auto$mpg))
new.mpg <- Auto$mpg < median(Auto$mpg)
dati <- data.frame(new.mpg=new.mpg, Auto[, c('displacement', 'horsepower', 'origin')])
head(dati)

boxplot(dati$displacement ~ dati$new.mpg)

dati$origin <- as.factor(dati$origin)
boxplot(dati$displacement ~ dati$origin)


model <- glm(new.mpg ~ ., data=dati, family='binomial')
summary(model)
