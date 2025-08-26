library(readxl)

# Define the file path to your Excel file
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Dry_matter.xlsx"

# Read the Excel file
dry_matter_data <- read_excel(file_path)

# View the first few rows of the data
head(dry_matter_data)
str(dry_matter_data)
# Perform Shapiro-Wilk test for normality on Dry_matter data
shapiro_test_dry_matter <- shapiro.test(dry_matter_data$Dry_matter)

# Print the result of the Shapiro-Wilk test
print(shapiro_test_dry_matter)
# Convert 'Treatment' and 'Genotype' to factors
dry_matter_data$Treatment <- as.factor(dry_matter_data$Treatment)
dry_matter_data$Genotype <- as.factor(dry_matter_data$Genotype)

# Perform ANOVA for the effect of Treatment on Dry_matter
anova_dry_matter <- aov(Dry_matter ~ Treatment * Genotype, data = dry_matter_data)

# Display the summary of the ANOVA result
summary(anova_dry_matter)
# Subset data for Mandel Genotype
mandel_data <- subset(dry_matter_data, Genotype == "Mandel")

# Perform ANOVA for Mandel Genotype (Straw vs Non straw)
anova_mandel <- aov(Dry_matter ~ Treatment, data = mandel_data)

# Display the results for Mandel Genotype
summary(anova_mandel)

# Subset data for KingEdward Genotype
kingedward_data <- subset(dry_matter_data, Genotype == "KingEdward")

# Perform ANOVA for KingEdward Genotype (Straw vs Non straw)
anova_kingedward <- aov(Dry_matter ~ Treatment, data = kingedward_data)

# Display the results for KingEdward Genotype
summary(anova_kingedward)
