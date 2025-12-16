library(datasets)
data("iris")
df <- iris
head(df)
names(iris)
iris_histplt <- hist(df$Sepal.Width, main = "Histogram of Iris Sepal Width",
                     xlab = "Sepal Wiidth")