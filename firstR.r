library(tidyverse)
library(datasets)
library(readxl)
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
abline(v = 1930, col = "lightgrey")
text(1930, 10, "1930", col = "red3")
abline(v = 1940, col = "lightgray")
text(1940, 10, "1940", col = "red3")

# plot with ts.plot()
ts.plot(uspop) # Although this can be used for a single time series,
# plot is easier to use and is preferred.

# More powerful alternative
?plot.ts
plot.ts(uspop)

# MULTIPLE TIME SERIES PLOTS #######

#EuStockMarkets data
# DAX (Germany), SMI (Switzerland), CAC (France), FTSE (UK)
?EuStockMarkets
EuStockMarkets

# Three different plot functions
plot(EuStockMarkets) # Stacked windows
plot.ts(EuStockMarkets) # Identical
ts.plot(EuStockMarkets) # One window
# Plot with options
ts.plot(
  EuStockMarkets,
  plot.type = "single", # Single plot with multiple series
  col = rainbow(4),
  main = "European Stock Markets",
  sub = "(Source: datasets::EuStockMarkets)",
  xlab = "Year",
  ylab = "Index",
  lty = 1 # Line type : Solid line
)
legend(
  "topleft",
  legend = colnames(EuStockMarkets), #Names for legend
  col = rainbow(4), # colors for legend
  lty = 1 #Line type: solid
)

### CLKUSTER CHARTS #######
install.packages("ggdendro")
library(tidyverse)
library(ggdendro)

# Select state codes and search data
df6 <- read_csv("state_trends.csv") |>
  select(state_code, artificial_intelligence:hockey) |>
  drop_na() # Remove rows with missing data

df6

# STANDARDIZE & PREPARE DATA #####

# Standardize quatitative data

# for this case standardizing may not be necessary
# as all data values are between 0 and 1000, but date with different scales 
# need to be standardize before applying cluster analysis

df_scaled <- df6 |>
  select(-state_code) |> # Remopve state code from scaling
  scale()                # Standardize variables

# Assign state codes as row names
rownames(df_scaled) <- df6$state_code
df_scaled

# ANALYZE AND PLOT DATA #####

# Calculate cluster using hclust(), an agglomerative method
hc <- df_scaled |> # Get data
  dist() |> # Calculate distance/disimilarity matrix
  hclust() # Compute hierarchial clusters

# Plot dendrogram
hc |> plot(
  labels = rownames(df_scaled),
  main = "Cluster Dendrogram of States"
)

# Draw boxes aroud cluster
hc |> rect.hclust(k = 2, border = "gray80") # 2 boxes
hc |> rect.hclust(k = 3, border = 2:4)          # 3 boxes

# Alternative visualization from 'ggdendro'
hc |> ggdendrogram(
  rotate = TRUE,  # Rotates to horizontal
  theme_dendro = FALSE # Turns off
)

# LOAD DATA #############
df8 <- read_csv("state_trends.csv") |>
  select(state, region, psych_region, data_analysis) |>
  mutate(across(c(region:psych_region), as_factor)) |>
  print(n = 10)

# FILTER BY ONE VARIABLE #####

# "data_analysis" is a numeric variable
df8 |>
  filter( data_analysis > 50) |>
  arrange(desc(data_analysis)) |> # sort by data_analysis in descending order
  print()

# "psych_region is a text variable"
df8 |>
  filter(psych_region == "Relaxed and Creative") |>
  arrange(desc(data_analysis)) |>
  print()

# "or" is the vertical pipe | operator
df8 |>
  filter(region == "South" |
           psych_region == "Relaxed and Creative") |>
  arrange(region, psych_region) |> # Sort output by region and psych_region
  print(n = Inf) # print all rows

# "and" is the ambersand & operator
df8 |>
  filter(region == "South" &
            psych_region == "Relaxed and Creative") |>
  print()

# "not" is the exclamation mark ! operator
df8 |>
  filter(region == "South" &
           !psych_region == "Relaxed and Creative")|>
  arrange(psych_region, desc(data_analysis)) |>
  print()

# LOAD DATA ######
df9 <- read_csv("state_trends.csv") |>
  mutate(across(where(is_character), as_factor)) |>
  print()

# COMBINE CATEGORIES WITH RECODE #####

df9 |>
  mutate(relaxed = recode(psych_region,
                          "Relaxed and Creative" = "yes",
                          "Friendly and Conventional" = "no",
                          .default = "no")) |> # sets default value
  select(state_code, psych_region, relaxed)

# CREATE CATEGORIES WITH CASE_WHEN ####

?case_when

df9 |>
  mutate(
    like_arts = case_when(
      art > 75 | dance > 75 | museum > 75 ~ "yes",
      TRUE ~ "no" # all others no
    )
  ) |>
  select(state_code, like_arts, art:museum) |>
  arrange(desc(like_arts)) |> # put yes at top
  print(n = Inf)

df9 |>
  mutate(
    like_arts = case_when(
      art > 65 & dance > 65 & museum > 30 ~ "yes",
      TRUE ~ "no" # all others no
    )
  ) |>
  select(state_code, like_arts, art:museum) |>
  arrange(desc(like_arts)) |> # put yes at top
  print(n = Inf)

# CREATE DATA ######

# Create a small dataset with 1-7 data and a missing value
df10 <- tibble(
  x = 1:5,
  y = 7:3,
  z = c(2, 4, 3, 7, NA)
) |>
  print()
# AVERAGE ACROSS VARFIABLES #####

# Average variables with 'rowMeans'
df10 %>% mutate(
  mean_xy = rowMeans(across(x:y)),
  mean_xyz = rowMeans(across(x:z)),
  mean_xz = rowMeans(across(c(x, z)))
)

# Remove missing values by adding 'na.rm = T'
df10 %>% mutate(
  mean_xy = rowMeans(across(x:y), na.rm = T),
  mean_xyz = rowMeans(across(x:z), na.rm = T),
  mean_xz = rowMeans(across(c(x, z)), na.rm = T)
)

# REVERSE CODING ########

df10 %>% 
  mutate(y_r = 8 - y) |> # Create reversed variable
  select(x, y_r, z) |> # Select and reorder variables
  mutate(              # Compute average scores
    mean_xy = rowMeans(across(c(x, y_r)), na.rm = T),
    mean_xyz = rowMeans(across(c(x, y_r, z)), na.rm = T),
    mean__xz = rowMeans(across(c(x, z)), na.rm = T)
  )

# FREQUENCY SUMMARIES #####

# LOAD DATA #########

# Also convert several adjacent variables to factors
df11 <- read_csv("state_trends.csv") |>
  select(region:psy_reg) |>
  mutate(across(c(psych_region, psy_reg), as.factor)) |>
  print()

# SUMMARIZE DATAFRAME #######
# Simpliest analysis you can do is simply
# counting the number of observations in your data (frequencies)

summary(df11) # Gives frequency counts for factors

# summary not useful for character variables
df11 |>
  select(region) |>
  summary()

#  table works better
df11 |>
  select(region) |>
  table()

# Using summary()
df11 |>
  select(region) |>
  summary()

# Using table()
df11 |>
  select(psych_region) |>
  table()

# Convert region to factor
df11 <- df11 |>
  mutate(region = as.factor(region)) |>
  print()

# Summarize multiple factors
summary(df11)

mtcars |>
  ggplot(aes(x = hp, y = mpg, color = wt)) +
  geom_point(size = 3) + 
  scale_color_gradient(low = "blue", high="red") +
  labs(title = "MPG vs Horsepower Colored by Weight",
       x = "horsepower(hp)",
       y = "Miles per Gallon(mpg)") +
theme(
  panel.background = element_rect(fill = "lightgrey"),
  plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text = element_text(size = 12)
)

df12 <- read_csv("state_trends.csv") |>
  mutate(across(c(region, psych_region, psy_reg, has_nba:has_any), as_factor)
  ) |>
  print()

# SUMMARY ###################################

df12 |> summary()

# Summary for one variable
df12 |>
  select(statistics) |>
  summary()

# QUARTILES ###############################

# Tukey's five-nuymber summary: minimum, lower-hinge,
# median, upper-hinge and maximum
fivenum(df12$statistics)

# Boxplot stats: hinges, n, CI for median, and outliers
boxplot(df12$statistics, notch = T, horizontal = T)
boxplot.stats(df12$statistics)
 

# LOAD DATA #########################
# Also convert several adjacent variables to factors
df13 <- read_csv("state_trends.csv") |>
  select( # Rename variables with 'select'
         DS  = data_science, # New = old
         AI = artificial_intelligence,
         ML = machine_learning,
         DA = data_analysis,
         BI = business_intelligence,
         SS = spreadsheet,
         Stats = statistics) |>
  print()

# CORRELATION MATRIX ####################

# Scatterplot matrix
df13 |> plot()

# Correlation matrix
df13 |> cor()

# Rounded to 2 decimals
df13 |>
  cor() |>
  round(2)

# TEST AND CI FOR A SINGLE CORELATION #######

# Can test one pair of variables at a time
# Gives r, hypothesis test, and confidence interval
cor.test(df13$DS, df13$DA)

# PACKAGE FOR CORRELATION MATRIX WITH P-VALUES #######

# the 'Hmisc' package has a function rcorr() that
# computes a correlation matrix along with p-values

# The 'rstatix' package has a function 'cor_mat()' with graphical output

# LOAD DATA #########################
df14 <- read.csv("state_trends.csv") |>
  select(extraversion:hockey) |>
  print()

# Scatterplot of "data_science' and personality variables
df14 |>
  select(data_science, extraversion:openness) |>
  plot()

# Quick graphical check of bivariate association
df14 |>
  select(openness, data_science) |>
  plot()

# Add regression line with lm(); usage: y ~ x
# Note different variable order (vs plot)
lm(df14$data_science ~ df14$openness) |>
  abline(col = "red3", lwd = 2) # line color and width

# BIVARIATE REGRESSION #################

# Compute and save bivariate regression
fit1 <- lm(df14$data_science ~ df14$openness)

# Show model
fit1

# Summarize regression model
summary(fit1)

# Confidence interval for coefficients
confint(fit1)

# Predict values of "data_science" 
predict(fit1)

# Prediction interval for values of "data_science"
predict(fit1, interval = "prediction")

# Regrion diagnostics
lm.influence(fit1)
influence.measures(fit1)

# MULTIPLE REGRESSION #################

# Moving the outcome, y, to the front and having nothing 
# else but predictor variables, X, can make things easier
df14 <- df14 |> 
  select(data_science, extraversion:openness) |>
  print()

# Note that if you want to just move one variable to the
# front and keep everything else in the same order, you can
# do this : select(data_science, everything()) |>
# or use relocate: relocate(data_science, .before = extraversion) |>

# Three ways to specify model

# Most concise
lm(df14)

# Identify outcome, infer rest
lm(data_science ~ ., data = df14)

# Identify entire model
lm(data_science ~ extraversion + agreeableness + conscientiousnesss + 
     neuroticism + openness, data = df14)

# Save model
fit2 <- lm(df14)

# Show model
fit2

# Summarize regression model
summary(fit2)

# LOAD DATA #########################
df15 <- read_csv("state_trends.csv") |>
  select(region, psy_reg) |>
  mutate(across(everything(), as_factor)) |>
  print()

# ANALYZE DATA #######################

# Create contingency table
ct <- table(df15$region, df15$psy_reg)
ct 

# Call also get cell, row, and column percentages
# With rounding to get just 2 decimal places
# Multiply by 100 to get %

# Row percentages
ct |> 
  prop.table(1) |> # 1 is for row percentages
  round(2) * 100

# Column percentages
ct |>
  prop.table(2) |> # 2 is for column percentages
  round(2) * 100

# Total percentages
ct |>
  prop.table() |> # No 1 or 2 gives total percentages
  round(3) * 100

# Chi-squared test (but n is small)
tchi <- chisq.test(ct)
tchi

# Get p-value in one step
table(df15$region, df15$psy_reg) |>  chisq.test()

# Additional tables
tchi$observed # Observed counts
tchi$expected # Expected counts
tchi$residuals # Pearson residuals
tchi$stdres # Standardized residuals

showExpectedResult <- FALSE
showHints <- TRUE
showPackages <- FALSE

fizzbuzz <- function(fizz, buzz) {
  # Replace the next line with appropriate code
  print("I'm returning buzz")
  return("fizz")
}

fizzbuzz(3, 5)
