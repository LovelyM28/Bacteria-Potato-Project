# Load necessary library
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Tuber_size.xlsx"

# Read the data from the Excel file
tuber_size_data <- read_excel(file_path, sheet = 2)

# View the first few rows of the data
head(tuber_size_data)
str(tuber_size_data)
# Perform Shapiro-Wilk test on the 'Tuber_numbers (%)' column
shapiro_test_tuber_numbers <- shapiro.test(tuber_size_data$`Tuber_numbers`)

# Print the results
print(shapiro_test_tuber_numbers)
# Perform Shapiro-Wilk test on the 'Tuber_numbers (%)' column
shapiro_test_tuber_numbers <- shapiro.test(tuber_size_data$`Tuber_numbers`)

# Print the results
print(shapiro_test_tuber_numbers)

library(ARTool)  
library(dplyr)

# Ensure Treatment, Size, and Genotype are factors
tuber_size_data <- tuber_size_data %>%
  mutate(Treatment = as.factor(Treatment),
         Size = as.factor(Size),
         Genotype = as.factor(Genotype))  # Add Genotype as a factor

# Apply the Aligned Rank Transform (ART)
art_model <- art(Tuber_numbers ~ Treatment * Size * Genotype, data = tuber_size_data)

# Run ANOVA on the ART-transformed data
anova_results <- anova(art_model)

# Print ANOVA results
print(anova_results)


# Run ART ANOVA separately for each Genotype
genotype_results <- tuber_size_data %>%
  group_by(Genotype) %>%
  do({
    art_model <- art(Tuber_numbers ~ Treatment * Size, data = .)
    anova_results <- anova(art_model)
    # Return the ANOVA results for each Genotype
    tibble(Genotype = unique(.$Genotype), anova_results)
  })

# View the results
print(genotype_results)


library(dplyr)
library(ARTool)

# Ensure Treatment, Size, and Genotype are factors
tuber_size_data <- tuber_size_data %>%
  mutate(Treatment = as.factor(Treatment),
         Size = as.factor(Size),
         Genotype = as.factor(Genotype))

# Run ART ANOVA separately for each Genotype and Size combination
genotype_size_results <- tuber_size_data %>%
  group_by(Genotype, Size) %>%
  do({
    art_model <- art(Tuber_numbers ~ Treatment, data = .)  # Run ANOVA for each Genotype and Size group
    anova_results <- anova(art_model)
    # Return the ANOVA results for each Genotype and Size combination
    tibble(Genotype = unique(.$Genotype), 
           Size = unique(.$Size),
           anova_results)
  })

# View the results
print(genotype_size_results)










