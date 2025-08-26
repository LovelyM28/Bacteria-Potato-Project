library(readxl)

# Define the file path to your Excel file
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Ascorbic_acid_data.xlsx"

# Read the Excel file
ascorbic_data <- read_excel(file_path)

# View the first few rows of the data to check it
head(ascorbic_data)
# Perform Shapiro-Wilk test for normality on Ascorbic_acid data
shapiro_test_ascorbic <- shapiro.test(ascorbic_data$Ascorbic_acid)

# Print the result of the Shapiro-Wilk test
print(shapiro_test_ascorbic)
# Convert 'Treatment' and 'Genotype' to factors (if they are not already)
ascorbic_data$Treatment <- as.factor(ascorbic_data$Treatment)
ascorbic_data$Genotype <- as.factor(ascorbic_data$Genotype)

# Perform ANOVA for Treatment and Genotype (including interaction)
anova_result <- aov(Ascorbic_acid ~ Treatment * Genotype, data = ascorbic_data)

# Display the summary of the ANOVA result
summary(anova_result)
# Subset data for Mandel Genotype
mandel_data <- subset(ascorbic_data, Genotype == "Mandel")

# Perform ANOVA for Mandel Genotype (Straw vs Non straw)
anova_mandel <- aov(Ascorbic_acid ~ Treatment, data = mandel_data)

# Display the results for Mandel Genotype
summary(anova_mandel)

# Subset data for KingEdward Genotype
kingedward_data <- subset(ascorbic_data, Genotype == "KingEdward")

# Perform ANOVA for KingEdward Genotype (Straw vs Non straw)
anova_kingedward <- aov(Ascorbic_acid ~ Treatment, data = kingedward_data)

# Display the results for KingEdward Genotype
summary(anova_kingedward)
