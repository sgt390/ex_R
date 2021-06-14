data(iris)

# list all datasets
library(help='datasets')

dim(iris)
names(iris)
iris[3,]

is.factor(iris$Species)

table(iris$Species)

plot(Sepal.Length ~ Sepal.Width, data=iris, col=Species)
plot(Sepal.Length ~ Petal.Width, data=iris, col=Species)
plot(Sepal.Length ~ Petal.Length, data=iris, col=Species)

par(mfrow=c(2,2))
boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
# linear discriminant analisys
library(MASS)
model <- lda(Species ~ ., data=iris)

plot(model)
