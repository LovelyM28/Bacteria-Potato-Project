# Load necessary library
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/Soil _data/Soil_data.xlsx"

# Read the Excel file (default is the first sheet)
soil_data <- read_excel(file_path)

# View the first few rows
head(soil_data)
str(soil_data)
library(dplyr)
# Split the data by parameters and perform the Shapiro-Wilk test
soil_data %>%
  group_by(Parameters) %>%
  summarise(
    shapiro_non_straw = shapiro.test(`Non-straw`)$p.value,
    shapiro_straw = shapiro.test(Straw)$p.value
  )
library(dplyr)
library(tidyr)
library(ggpubr)  # For visualization (optional)

# Convert data from wide to long format
soil_data_long <- soil_data %>%
  pivot_longer(cols = c(`Non-straw`, Straw), names_to = "Treatment", values_to = "Value") %>%
  mutate(Treatment = as.factor(Treatment))  # Ensure Treatment is a factor

# Perform ANOVA for each parameter
anova_results <- soil_data_long %>%
  group_by(Parameters) %>%
  summarise(
    p_value = summary(aov(Value ~ Treatment))[[1]][["Pr(>F)"]][1]  # Extract p-value from ANOVA
  )

# Print results
print(anova_results)
