#Подключаем необходимые библиотеки
library(ggplot2)
library(gridExtra)
library(caret)
library(kknn)
#Загружаем данные
read.csv("iris.csv", sep=",")
#Графики
a <- qplot(Sepal.Length, Sepal.Width, data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "red", se = FALSE) 
b <- qplot(Petal.Length, Petal.Width,data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "red", se = FALSE)
grid.arrange(a, b, nrow = 2)
#Преобразование столбца в фактор
iris$Species <- factor(iris$Species)
#Тестирование модели
set.seed(56)
(samp.size <- floor(nrow(iris) * .8))
train.ind <- sample(seq_len(nrow(iris)), size = samp.size)
train <- iris[train.ind, ]
test <- iris[-train.ind, ] 
knn.iris <- class::knn(train = train[, -5], test = test[, -5], 
                       cl = train[, "Species"], k = 15, prob = TRUE)
#Результаты
(table(Факт = test$Species, Прогноз = knn.iris))
Acc <- mean(knn.iris == test$Species)
paste("Точность=", round(100*Acc, 2), "%", sep = "")


