# Install and load required libraries
#required_packages <- c("haven", "dplyr", "tidyr", "psych", "openxlsx", "readxl", "jtools")

# Load required libraries
library(haven)       # For reading .dta files
library(dplyr)       # For data manipulation
library(tidyr)       # For handling missing data
library(psych)       # For descriptive statistics
library(openxlsx)    # For exporting to Excel
library(readxl)      # For reading .xslx files
library(jtools) # For the `summ()` function in regression summary
library(stringr)

# Set working directory
#setwd("C:/Users/juspiira/R")
setwd("C:/Users/hyvarant/OneDrive - Fimea/R_koodi/Piira")
# Clear environment
#rm(list = ls())

# Read Excel data file
#data <- read_xlsx("C:/Users/juspiira/R/2025-03-11_kannattavuus.xlsx")
data <- read_xlsx("2025-03-11_kannattavuus.xlsx")
###################################################################
result <- str_extract(data$apteekkiyritys, "^[^\\(]*")
result <- str_trim(result)
data$apt <- result

table(data$apt %in% aptRek$`Pitkä nimi`)
data[!data$apt %in% aptRek$`Pitkä nimi`,]

data[data$apt=="Maarianhaminan 1. apteekki", "apt"]<- "Mariehamns 1:a apotek"
data[data$apt=="Maarianhaminan 2. apteekki", "apt"]<- "Mariehamns 2:a apotek"
data[data$apt=="Raaseporin 1.", "apt"]<- "Raaseporin 1. apteekki"

data[!data$apt %in% aptRek$`Pitkä nimi`,]
#################################################################
# Summary statistics
summary(data)

# Remove rows with missing data
data <- data %>%
  filter(!is.na(katetuotto * koko * Apteekkienalueellinenlukumääräsi * Osakeyhtiö_kyllä_ei * 
                  sivuapteekkiluvatkpl * aukiolohviikko * Kilpailu * postinumero_val))

# Remove duplicates
data <- data %>%
  group_by(apteekkiyritys) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Descriptive statistics
desc_stats <- data %>%
  select(katetuotto, koko, Apteekkienalueellinenlukumääräsi, Osakeyhtiö_kyllä_ei, 
         sivuapteekkiluvatkpl, aukiolohviikko, Kilpailu) %>%
  summarise(across(everything(), list(
    N = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE),
    p25 = ~quantile(., 0.25, na.rm = TRUE),
    p50 = ~median(., na.rm = TRUE)
  )))

print(desc_stats)

# Correlation matrix
cor_matrix <- cor(data %>% select(katetuotto, koko, Apteekkienalueellinenlukumääräsi, 
                                  Osakeyhtiö_kyllä_ei, sivuapteekkiluvatkpl, aukiolohviikko, Kilpailu), 
                  use = "pairwise.complete.obs")
print(cor_matrix)

# Regression
reg_model <- lm(katetuotto ~ koko + Apteekkienalueellinenlukumääräsi + Osakeyhtiö_kyllä_ei + 
                  sivuapteekkiluvatkpl + aukiolohviikko + Kilpailu + factor(postinumero_val), 
                data = data)

# Print regression results
summary(reg_model)

# Export results to a file
sink("regression_results.txt")
print("Descriptive Statistics:")
print(desc_stats)
print("\nCorrelation Matrix:")
print(cor_matrix)
print("\nRegression Model Summary:")
print(summary(reg_model))
sink()