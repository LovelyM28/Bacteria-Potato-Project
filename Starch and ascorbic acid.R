# Load the readxl package
library(readxl)

# Define the file path for the starch content data
file_path_starch <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Starch_content.xlsx"

# Read the Excel file
starch_content_data <- read_excel(file_path_starch)

library(dplyr)

starch_content_data <- starch_content_data %>%
  mutate(Treatment = recode(Treatment,
                            "Non straw" = "Control",
                            "Straw" = "Mulch"))


# View the first few rows of the data
head(starch_content_data)

# Check the structure of the dataset
str(starch_content_data)

# Ensure Genotype and Treatment are factors
starch_content_data$Genotype <- as.factor(starch_content_data$Genotype)
starch_content_data$Treatment <- as.factor(starch_content_data$Treatment)

library(ggplot2)
# Plot
ggplot(starch_content_data, aes(x = Genotype, y = Starch_content, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Starch Content by Genotype and Treatment",
       x = "Genotype",
       y = "Starch Content (µg/g tuber weight)",
       fill = "Treatment") +
  theme_minimal()


# Load the required package
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Excel files/Ascorbic_acid_data.xlsx"

# Read the Excel file into R
ascorbic_acid_data <- read_excel(file_path)

ascorbic_acid_data <- ascorbic_acid_data %>%
  mutate(Treatment = recode(Treatment,
                            "Non straw" = "Control",
                            "Straw" = "Mulch"))



# View the first few rows of the data
head(ascorbic_acid_data)

# View the structure of the dataset
str(ascorbic_acid_data)

# Ensure Genotype and Treatment are factors
ascorbic_acid_data$Genotype <- as.factor(ascorbic_acid_data$Genotype)
ascorbic_acid_data$Treatment <- as.factor(ascorbic_acid_data$Treatment)

# Plot
ggplot(ascorbic_acid_data, aes(x = Genotype, y = Ascorbic_acid, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Ascorbic Acid Content by Genotype and Treatment",
       x = "Genotype",
       y = "Ascorbic Acid (µg/g tuber weight)",
       fill = "Treatment") +
  theme_minimal()





library(ggplot2)
library(dplyr)

# Add a new column to each dataset to label the trait
ascorbic_acid_data_long <- ascorbic_acid_data %>%
  mutate(Trait = "Ascorbic Acid",
         Value = Ascorbic_acid)

starch_content_data_long <- starch_content_data %>%
  mutate(Trait = "Starch",
         Value = Starch_content)

# Combine both datasets into one
combined_data <- bind_rows(ascorbic_acid_data_long, starch_content_data_long)

# Ensure factors are properly set
combined_data$Genotype <- as.factor(combined_data$Genotype)
combined_data$Treatment <- as.factor(combined_data$Treatment)
combined_data$Trait <- as.factor(combined_data$Trait)

# Custom theme
custom_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  )

# Faceted boxplot
facet_plot <- ggplot(combined_data, aes(x = Genotype, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(y = "Content (µg/g tuber weight)", fill = "Treatment") +
  custom_theme

# Print the plot
print(facet_plot)

# Save the plot as a PNG file
ggsave("facet_plot.png", plot = facet_plot, width = 10, height = 6, dpi = 300)

