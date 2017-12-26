# Data Wrangling Exercise 2: Dealing with Missing Values

library(dplyr)
library(tidyr)

# 0: Load the data in RStudio
titanic_original <- read.csv("titanic_original.csv", header = TRUE)

# 1: Port of embarkation
titanic_clean <- titanic_original %>%
  mutate(
    embarked = replace(embarked, embarked == "", "S"),

# 2: Age
    age = replace(age, is.na(age), mean(age, na.rm = TRUE)),

# 3: Lifeboat
    boat = replace(boat, boat == "", NA),

# 4: Cabin
    has_cabin_number = ifelse(cabin == "", 1, 0)
)

# 5: Submit the project on Github

write.csv(titanic_clean, file="titanic_clean.csv")
