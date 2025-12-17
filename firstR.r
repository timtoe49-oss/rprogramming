library(tidyverse)
library(datasets)
data("iris")
df <- iris
head(df)
names(iris)
iris_histplt <- hist(df$Sepal.Width, main = "Histogram of Iris Sepal Width",
                     xlab = "Sepal Wiidth")

df2 <- read_csv("state_trends.csv")
head(df2)
glimpse(df2)