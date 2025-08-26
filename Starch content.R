# Load the readxl package
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Starch_content.xlsx"

# Read the first sheet of the Excel file
starch <- read_excel(file_path)

# View the first few rows of the data
head(starch)
shapiro_test <- shapiro.test(starch$Starch_content)

# Print results
print(shapiro_test)
starch$Treatment <- as.factor(starch$Treatment)
starch$Genotype <- as.factor(starch$Genotype)
anova_result <- aov(Starch_content ~ Treatment * Genotype, data = starch)

# Display the results
summary(anova_result)

# Subset data for Mandel Genotype
mandel_data <- subset(starch, Genotype == "Mandel")

# Perform ANOVA for Mandel Genotype (Straw vs Non straw)
anova_mandel <- aov(Starch_content ~ Treatment, data = mandel_data)
summary(anova_mandel)

# Subset data for KingEdward Genotype
kingedward_data <- subset(starch, Genotype == "KingEdward")

# Perform ANOVA for KingEdward Genotype (Straw vs Non straw)
anova_kingedward <- aov(Starch_content ~ Treatment, data = kingedward_data)
summary(anova_kingedward)

