
library(tidyverse)
showExpectedResult <- FALSE
showHints <- FALSE

# Function to find the names of countries with population densities
# greater than the median density.

highPopDens <- function(worldPopFile) {
  # Add the appropriate code
  worldPop <- read.csv(worldprofile)
  return(worldPop[worldPop == 2021 & 
                    worldPop$PopDensity > median(worldPop$PopDensity) &
                    worldPop$Variant == "medium", "Location"])
}

worldPopFile <- read_csv("WorldPop.csv")