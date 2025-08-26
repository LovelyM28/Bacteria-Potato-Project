# Load the CSV file
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/Field data/Plant_height.csv"

# Read the CSV file into R
plant_data <- read.csv(file_path)

# View the first few rows of the dataset
head(plant_data)
library(tidyr)
# Split the 'Date.Treatment.Cultivars.Plant_height' column by semicolons
plant_data_split <- separate(plant_data, Date.Treatment.Cultivars.Plant_height, 
                             into = c("Date", "Treatment", "Cultivars", "Plant_height"), 
                             sep = ";")

# Check the structure of the data to confirm it was split correctly
head(plant_data_split)
# Convert 'Plant_height' to numeric
plant_data_split$Plant_height <- as.numeric(plant_data_split$Plant_height)

# Check the structure again to confirm the conversion
str(plant_data_split)

# Perform the Shapiro-Wilk normality test on the 'Plant_height' column
shapiro_result <- shapiro.test(plant_data_split$Plant_height)

# Print the result of the Shapiro-Wilk test
print(shapiro_result)


# Loop over each Date to perform Kruskal-Wallis test for each Treatment and Cultivar
unique_dates <- unique(plant_data_split$Date)

for (date in unique_dates) {
  
  # Loop over each Treatment for the specific Date
  unique_treatments <- unique(plant_data_split$Treatment[plant_data_split$Date == date])  # Update treatments based on date
  
  for (treatment in unique_treatments) {
    
    # Subset the data for the specific Date and Treatment
    data_subset <- subset(plant_data_split, Date == date & Treatment == treatment)
    
    # Check if there are at least two cultivars to perform the test
    if (nrow(data_subset) > 1 && length(unique(data_subset$Cultivars)) > 1) { 
      # Perform Kruskal-Wallis test for Cultivars
      kruskal_result <- kruskal.test(Plant_height ~ Cultivars, data = data_subset)
      
      # Print the result
      cat("Kruskal-Wallis result for Treatment", treatment, "on Date", date, ":\n")
      print(kruskal_result)
    } else {
      # Skip the test if there's only one cultivar or not enough data
      cat("Skipping test for Treatment", treatment, "on Date", date, "due to insufficient levels in Cultivars\n")
    }
  }
}


# Ensure 'Cultivars' and 'Treatment' are factors
plant_data_split$Cultivars <- as.factor(plant_data_split$Cultivars)
plant_data_split$Treatment <- as.factor(plant_data_split$Treatment)
plant_data_split$Date <- as.factor(plant_data_split$Date)

# Loop over each Date
unique_dates <- unique(plant_data_split$Date)

for (date in unique_dates) {
  
  # Subset the data for the specific Date
  data_subset <- subset(plant_data_split, Date == date)
  
  # Loop over each Cultivar for the specific Date
  unique_cultivars <- unique(data_subset$Cultivars)
  
  for (cultivar in unique_cultivars) {
    
    # Subset the data for the specific Cultivar
    cultivar_subset <- subset(data_subset, Cultivars == cultivar)
    
    # Perform Kruskal-Wallis test to compare the Treatment (Control vs Straw) for this Cultivar
    kruskal_result_treatment <- kruskal.test(Plant_height ~ Treatment, data = cultivar_subset)
    
    # Print the result for this Cultivar and Date
    cat("Kruskal-Wallis result for Cultivar:", cultivar, "on Date:", date, "\n")
    print(kruskal_result_treatment)
    cat("\n\n")
  }
}

install.packages("dunn.test")
library(dunn.test)

# Loop over each Date to perform Kruskal-Wallis test for each Treatment and Cultivar
for (d in unique(plant_data_split$Date)) {
  cat("\n\n### Date:", d, "###\n")
  
  for (t in unique(plant_data_split$Treatment)) {
    # Subset the data for the specific Date and Treatment
    subset_data <- subset(plant_data_split, Date == d & Treatment == t)
    
    # Check if there are more than one cultivar and if the dataset is not empty
    if (nrow(subset_data) > 1 && length(unique(subset_data$Cultivars)) > 1) {
      # Perform Kruskal-Wallis test for Plant_height across Cultivars within Treatment and Date
      test_result <- kruskal.test(Plant_height ~ Cultivars, data = subset_data)
      cat("Kruskal-Wallis test for Treatment", t, "on date", d, ":\n")
      print(test_result)  # Print the Kruskal-Wallis test result
      
      # Run Dunn's post-hoc test for all comparisons between cultivars
      cat("Dunn's post-hoc test for all comparisons between cultivars:\n")
      posthoc_result <- dunn.test(subset_data$Plant_height, subset_data$Cultivars, method = "bonferroni")
      print(posthoc_result)
      
    } else {
      cat("Skipping test for Treatment", t, "on date", d, "due to insufficient levels in Cultivars\n")
    }
  }
}
# Check for missing values in the whole dataset
colSums(is.na(plant_data_split))
# Remove rows where Plant_height is NA
plant_data_clean <- subset(plant_data_split, !is.na(Plant_height))

# Check if NAs are gone
colSums(is.na(plant_data_clean))

install.packages("ARTool")
library(ARTool)

# Ensure categorical variables are factors
plant_data_clean$Treatment <- as.factor(plant_data_clean$Treatment)
plant_data_clean$Cultivars <- as.factor(plant_data_clean$Cultivars)
plant_data_clean$Date <- as.factor(plant_data_clean$Date)

Plant_data_art <- art(Plant_height ~ Treatment * Cultivars * Date, data = plant_data_clean)
anova(Plant_data_art)
# Check current levels
levels(plant_data_clean$Treatment)
# [1] "Control" "Straw"

# Rename "Control" to "Non straw"
levels(plant_data_clean$Treatment)[levels(plant_data_clean$Treatment) == "Control"] <- "Non straw"

# Check updated levels
levels(plant_data_clean$Treatment)
# [1] "Non straw" "Straw"

library(dplyr)

plant_data_clean <- plant_data_clean %>%
  mutate(Date = recode(Date,
                       "14/07/2023" = "W1",
                       "21/07/2023" = "W2",
                       "28/07/2023" = "W3",
                       "04/08/2023" = "W4",
                       "11/08/2023" = "W5",
                       "18/08/2023" = "W6",
                       "25/08/2023" = "W7",
                       "29/08/2023" = "W8"))


library(ggplot2)

# Ensure the Date column is ordered correctly
plant_data_clean$Date <- factor(plant_data_clean$Date, 
                                levels = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))

# Create the plot
plot <- ggplot(plant_data_clean, aes(x = Date, y = Plant_height, fill = interaction(Treatment, Cultivars))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Sampling Time (Weeks)",
    y = "Plant Height (cm)",
    fill = "Cultivars & Treatment"
  ) +
  scale_fill_manual(
    name = "Treatment.Variety",
    values = c("#FFCC80", "#FF8C00", "#b2df8a", "#33a02c"), 
    labels = c("Non straw.King Edward", "Straw.King Edward", "Non straw.Mandel", "Straw.Mandel")  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

# Save the plot as a PNG file with a white background
ggsave(filename = "boxplot.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")

