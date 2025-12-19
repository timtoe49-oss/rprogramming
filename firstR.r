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
### color website www.datalab.cc\rcolors
colors()

df3 <- read_csv("state_trends.csv") |>
  mutate(across(c(region:psy_reg), factor))
names(df3)

df3 |> 
  select(psy_reg)|>
  plot()

df3 |>
  select(psy_reg) |>
  barplot() # Error: height must be a vector or a matrix

df3 |>
  select(psy_reg) |>
  table() |> # insert table for bar plot
  sort(decreasing = T) |> # sort bars by decreasing values 
  #(Not for ordinal X)
  barplot(col = heat.colors(3))

df3 |>
  select(psy_reg) |>
  table() |>
  sort(decreasing = FALSE) |>
  barplot(
    main = "Personalities of 48 Conigous US States",
    sub = "(Source: state_trend.csv)",
    horiz = TRUE, # Draw horizontal bars
    ylab = "Personality Profile",
    xlab = "Number of States",
    xlim = c(0, 25), # Limits for X axis
    border = NA, # no borders on bars
    col = heat.colors(3)
  )

# Histogram with defaults
hist(df3$data_science)

# Histogram with options
hist(df3$data_science,
     breaks = 7, #suggests number of breaks
     main = "Histogram of search for \"Data Science\"",
     sub = "(Source: state_trend.csv)",
     ylab = "Frequency",
     xlab = "Search for \"Data Science\"",
     col = heat.colors(6))

# Density  Plot ##############################

# Density Plot wiith defaults
plot(density(df3$data_science))

#Density Plot with options
df3 |>
   pull(data_science) |> # use pull insted of select() for single column
   as.numeric() |> # Coerces to numeric variable
   density() |>  # draws density curve
   plot(
        main = " Density Plot of Searches for \"Data Science\"", 
        sub = ("Source: state_ternds.csv"),
        ylab = "Frequency",
        xlab = "Search for \"Data Science\"")
# Filling the area under the density curve
df3 |> 
  pull(data_science) |>
  as.numeric() |>
  density() |>
  polygon(col = "#CD0000") # Sets fill color to red3

library(readxl) # Reads Exel Files
# LOAD DATA #########################
# Alsos convert all acharacter variables to factors
df4 <- read_csv("state_trends.csv") |>
  mutate(across(where(is_character), as.factor)) |>
  print()

# BOXPLOT OF FREQUENCIES ###########

# Boxplot with defaults
boxplot(df4$dance)

# Who is the outlier?
df4 |>
  filter(dance > 90) |>
  select(state, dance)

# Boxplot with options
df4 |>
  select(dance) |>
  boxplot(
    horizontal = T, # Horizontal plot
    notch = T, # Confidence interval for median value
    main = "Boxplot for Searches for \"Dance\"",
    sub = "(Source: State_trends.csv)",
    xlab = "Searches for \"Dance\"",
    col = "#9400D3"
  )

# BOXPLOT FOR MULTIPLE VARIABLES #######

df4 |>
  select(basketball:hockey) |>
  boxplot()

# Who are the outliers on "hockey"?
df4 |>
  filter(hockey > 45) |>
  select(state, hockey) |>
  arrange(desc(hockey))

# Boxplot by group using plot()
df4 |>
  select(has_nhl, hockey) |>
  plot(
    horizontal = T,   # Confidence interval for median
    notch = T, # Confidence interval for median values
    main = "Boxplot of Searches for \"Hockey\"",
    sub = "(Source: state_trends.csv)",
    xlab = "Searches for \"Hockey\"",
    ylab = "State has NHL Hockey Team",
    col = "#9400D3"
  )


# LOAD DATA #########################

df5 <- read_csv("state_trends.csv") |>
  select(basketball:hockey) |>
  glimpse()

# Plot all associations
df5 |> plot()

# Bivariate scatterplot with defaults
df5 |>
  select(soccer, hockey) |>
  plot()

# Bivariate scatterplot with options
df5 |>
  select(soccer, hockey) |>
  plot(
    main = "Scatter plot of Seraches by State",
    xlab = "Searches of \"Soccer\"",
    ylab = "Searches for \"Hockey\"",
    col = "red3",
    pch = 20,  # "Plotting character" (small circle)
  )

# Add fit linear regression line(y ~ x)
lm(df5$hockey ~ df5$soccer) |>
  abline()

# US poppulation data (uspop)
data("uspop")
uspop
# plot with default plot()
plot(uspop)

# plot with options
uspop |>
  plot(
    main = "US population 1790-1970",
    sub = "(Source: datasets::uspop)",
    xlab = "Year",
    ylab = "Population (in millions)"
  )
abline(v = 1930, col ="lightgrey")
text(1930, 10, "1930", col = "red3")
abline(v = 1940, col = "lightgray")
text(1940, 10, "1940", col="red3")

# plot with ts.plot()
ts.plot(uspop) # Although this can be used for a single time series,
               # plot is easier to use and is preferred.
