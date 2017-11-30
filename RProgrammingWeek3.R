library(datasets)
library(dplyr)
data(iris)
?iris

#There will be an object called 'iris' in your workspace. In this dataset, what is the mean of 'Sepal.Length' for the species virginica? 
#Please round your answer to the nearest whole number.

sapply(split(iris$Sepal.Length,iris$Species), function(x) mean(x))

colMeans(iris[,c(1:4)])
apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
tapply(mtcars$mpg, mtcars$cyl, mean)
mean(mtcars$mpg, mtcars$cyl)
mtcars %>% group_by(cyl) %>% summarise(mean=mean(mpg))

with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)

x <- mtcars %>% group_by(cyl) %>% summarise(mean=mean(hp))
x$mean[3]-x$mean[1]


debug(ls)
ls
ls()
