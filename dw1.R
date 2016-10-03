#load librarues
library (tidyr)
library (dplyr)
library(stringr)
library(psych)

#Import .csv file as  a data frame
raw_data <- read.csv("C:/Users/deepa/OneDrive/Documents/refineori1.csv",head = TRUE, sep = ",")

tbl_df(raw_data)
View(raw_data)

# Clean up the names of brand
raw_data$company <- tolower(raw_data$company)
raw_data$company <- str_replace(raw_data$company, "f", "ph")
raw_data$company <- str_replace(raw_data$company, "^ph[a-z]+ps", "philips")
raw_data$company <- str_replace(raw_data$company, "^ak[a-z0-9 ]+", "akzo")
raw_data$company <- str_replace(raw_data$company,"unilver","unilever")

#separate the name from product code
raw_data <- separate(raw_data, Product.code...number, 
                     c("product_code", "product_number"), sep = "-")

# Add product categories
categorize_product <- function(product_code) {
  if (product_code == "p") {
    return ("Smartphone") 
  } else if (product_code == "v") {
    return ("TV") 
  } else if (product_code == "x") {
    return ("Laptop") 
  } else if (product_code == "q") {
    return("Tablet") 
  }}

raw_data <- raw_data %>% mutate(product_category = sapply(product_code, categorize_product))

# Add full address for geocoding
raw_data <- unite(raw_data, "full_address", address, city, country, sep = ",")

raw_data <- raw_data %>%
  mutate(company_philips = ifelse(company == "philips",1,0 )) %>%
  mutate(company_akzo = ifelse(company == "akzo",1,0 )) %>%
  mutate(company_van_houten = ifelse(company == "van houten",1,0 )) %>%
  mutate(company_unilever = ifelse(company == "unilever",1,0 )) %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone",1,0)) %>%
  mutate(product_tv = ifelse(product_category == "TV",1,0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop",1,0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet",1,0)) 

#write the clean file into csv
write.csv(raw_data, file="refine_clean.csv")