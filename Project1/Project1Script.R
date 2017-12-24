
# Data Wrangling Exercise 1

library(dplyr)
library(tidyr)

# 0: Load the data in RStudio
refine_original <- read.csv("refine_original.csv", header = TRUE)

# 1: Clean up brand names
refine_original$company <- sub(pattern = ".*(ps|pS)$", replacement = "philips", x = refine_original$company)
refine_original$company <- sub(pattern = "^(ak|Ak|AK).*", replacement = "akzo", x = refine_original$company)
refine_original$company <- sub(pattern = ".*ten$", replacement = "van houten", x = refine_original$company)
refine_original$company <- sub(pattern = ".*ver$", replacement = "unilever", x = refine_original$company)

# 2: Separate product code and number
refine_original <- separate(refine_original, col = Product.code...number, into = c("product_code", "product_number"), sep = "-")

# 3: Add product categories
refine_clean <- refine_original %>%
  mutate(product_category = case_when(
    .$product_code == "p" ~ "Smartphone",
    .$product_code == "v" ~ "TV",
    .$product_code == "x" ~ "Laptop",
    .$product_code == "q" ~ "Tablet"
    ),
    
# 4: Add full address for geocoding
    full_address = paste(address, city, country, sep = ", "),

# 5: Create dummy variables for company and product category
    company_philips = ifelse(company == "philips", 1, 0),
    company_azko = ifelse(company == "akzo", 1, 0),
    company_van_houten = ifelse(company == "van houten", 1, 0),
    company_unilever = ifelse(company == "unilever", 1, 0),

    product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
    product_tv = ifelse(product_category == "TV", 1, 0),
    product_laptop = ifelse(product_category == "Laptop", 1, 0),
    product_tablet = ifelse(product_category == "Tablet", 1, 0)
)

# 6: Submit the project on Github
write.csv(refine_clean, file="refine_clean.csv")

