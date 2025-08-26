# Load necessary library
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Tuber_yield.xlsx"

# Read the Excel file into R
tuber_yield_data <- read_excel(file_path)

# Display the first few rows of the dataset
head(tuber_yield_data)
str(tuber_yield_data)
# Perform Shapiro-Wilk test on Tuber_yield
shapiro_test_result <- shapiro.test(tuber_yield_data$Tuber_yield)

# Print the result
print(shapiro_test_result)
library(ARTool)

# Convert 'Treatment' and 'Genotype' to factors
tuber_yield_data$Treatment <- as.factor(tuber_yield_data$Treatment)


# Perform ART
art_model <- art(Tuber_yield ~ Treatment * Genotype, data = tuber_yield_data)

# View the summary of the ART model
anova(art_model)

# ART ANOVA for Mandel
art_mandel <- art(Tuber_yield ~ Treatment, data = subset(tuber_yield_data, Genotype == "Mandel"))
anova(art_mandel)

# ART ANOVA for KingEdward
art_kingedward <- art(Tuber_yield ~ Treatment, data = subset(tuber_yield_data, Genotype == "KingEdward"))
anova(art_kingedward)
