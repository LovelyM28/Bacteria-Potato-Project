library(readxl)

# Define the file path to your Excel file
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Tubers_weight.xlsx"

# Read the Excel file
tubers_weight_data <- read_excel(file_path)

# View the first few rows of the data
head(tubers_weight_data)

# Check the structure of the dataset
str(tubers_weight_data)

# Perform Shapiro-Wilk normality test
shapiro_test_tubers_weight <- shapiro.test(tubers_weight_data$Tubers_weight)

# Print the results
print(shapiro_test_tubers_weight)

library(ARTool)
art_model <- art(Tubers_weight ~ Treatment * Genotype, data = tubers_weight_data)
anova(art_model)
# Load necessary package
library(ARTool)

# Convert Treatment to a factor
tubers_weight_data$Treatment <- as.factor(tubers_weight_data$Treatment)

# ART ANOVA for Mandel
art_mandel <- art(Tubers_weight ~ Treatment, data = subset(tubers_weight_data, Genotype == "Mandel"))
anova(art_mandel)

# ART ANOVA for KingEdward
art_kingedward <- art(Tubers_weight ~ Treatment, data = subset(tubers_weight_data, Genotype == "KingEdward"))
anova(art_kingedward)

