# Data Wrangling Exercise 2: Dealing with Missing Values

library(dplyr)
library(tidyr)

# 0: Load the data in RStudio
titanic_original <- read.csv("titanic_original.csv", header = TRUE)

# 1: Port of embarkation
titanic_clean <- titanic_original %>%
  mutate(
    embarked = replace(.$embarked, .$embarked == "", "S"),

# 2: Age
    age = replace(age, is.na(age), age == mean(age))

# 3: Lifeboat

)
# 4: Cabin


# 5: Submit the project on Github
