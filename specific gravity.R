# Load the readxl package
library(readxl)

# Define the file path to your Excel file
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Specific_gravity.xlsx"

# Read the Excel file
specific_gravity_data <- read_excel(file_path)

# View the first few rows of the data
head(specific_gravity_data)
str(specific_gravity_data)
# Perform Shapiro-Wilk test for normality on Specific_gravity data
shapiro_test_specific_gravity <- shapiro.test(specific_gravity_data$Specific_gravity)

# Print the result of the Shapiro-Wilk test
print(shapiro_test_specific_gravity)
# Convert 'Treatment' and 'Genotype' to factors
specific_gravity_data$Treatment <- as.factor(specific_gravity_data$Treatment)
specific_gravity_data$Genotype <- as.factor(specific_gravity_data$Genotype)

# Perform ANOVA for the effect of Treatment and Genotype on Specific_gravity
anova_specific_gravity <- aov(Specific_gravity ~ Treatment * Genotype, data = specific_gravity_data)

# Display the summary of the ANOVA result
summary(anova_specific_gravity)
# Subset data for Mandel Genotype
mandel_data <- subset(specific_gravity_data, Genotype == "Mandel")

# Perform ANOVA for Mandel Genotype (Treatment effect on Specific_gravity)
anova_mandel <- aov(Specific_gravity ~ Treatment, data = mandel_data)

# Display the results for Mandel Genotype
summary(anova_mandel)

# Subset data for KingEdward Genotype
kingedward_data <- subset(specific_gravity_data, Genotype == "KingEdward")

# Perform ANOVA for KingEdward Genotype (Treatment effect on Specific_gravity)
anova_kingedward <- aov(Specific_gravity ~ Treatment, data = kingedward_data)

# Display the results for KingEdward Genotype
summary(anova_kingedward)
