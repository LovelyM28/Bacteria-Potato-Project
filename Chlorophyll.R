# Load the readxl package
library(readxl)

# Define the file path
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/Chlorophyll/Chl_data.xlsx"

# Load the Excel file
chl_data <- read_excel(file_path)

# Check the first few rows of the data
head(chl_data)
# Perform Shapiro-Wilk normality test on the Chlorophyll column
shapiro_result <- shapiro.test(chl_data$Chlorophyll)

# Print the result of the Shapiro-Wilk test
print(shapiro_result)

# Loop over each Date to perform Kruskal-Wallis test for each Treatment and Cultivar
unique_dates <- unique(chl_data$Date)

for (date in unique_dates) {
  
  # Loop over each Treatment for the specific Date
  unique_treatments <- unique(chl_data$Treatment[chl_data$Date == date])  # Update treatments based on date
  
  for (treatment in unique_treatments) {
    
    # Subset the data for the specific Date and Treatment
    data_subset <- subset(chl_data, Date == date & Treatment == treatment)
    
    # Check if there are at least two cultivars to perform the test
    if (nrow(data_subset) > 1 && length(unique(data_subset$Cultivars)) > 1) { 
      # Perform Kruskal-Wallis test for Cultivars
      kruskal_result <- kruskal.test(Chlorophyll ~ Cultivars, data = data_subset)
      
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
chl_data$Cultivars <- as.factor(chl_data$Cultivars)
chl_data$Treatment <- as.factor(chl_data$Treatment)
chl_data$Date <- as.factor(chl_data$Date)

# Loop over each Date
unique_dates <- unique(chl_data$Date)

for (date in unique_dates) {
  
  # Subset the data for the specific Date
  data_subset <- subset(chl_data, Date == date)
  
  # Loop over each Cultivar for the specific Date
  unique_cultivars <- unique(data_subset$Cultivars)
  
  for (cultivar in unique_cultivars) {
    
    # Subset the data for the specific Cultivar
    cultivar_subset <- subset(data_subset, Cultivars == cultivar)
    
    # Perform Kruskal-Wallis test to compare the Treatment (Control vs Straw) for this Cultivar
    kruskal_result_treatment <- kruskal.test(Chlorophyll ~ Treatment, data = cultivar_subset)
    
    # Print the result for this Cultivar and Date
    cat("Kruskal-Wallis result for Cultivar:", cultivar, "on Date:", date, "\n")
    print(kruskal_result_treatment)
    cat("\n\n")
  }
}
library(dunn.test)

# Loop over each Date to perform Kruskal-Wallis test for each Treatment and Cultivar
for (d in unique(chl_data$Date)) {
  cat("\n\n### Date:", d, "###\n")
  
  for (t in unique(chl_data$Treatment)) {
    # Subset the data for the specific Date and Treatment
    subset_data <- subset(chl_data, Date == d & Treatment == t)
    
    # Check if there are more than one cultivar and if the dataset is not empty
    if (nrow(subset_data) > 1 && length(unique(subset_data$Cultivars)) > 1) {
     
      test_result <- kruskal.test(Chlorophyll ~ Cultivars, data = subset_data)
      cat("Kruskal-Wallis test for Treatment", t, "on date", d, ":\n")
      print(test_result)  # Print the Kruskal-Wallis test result
      
      # Run Dunn's post-hoc test for all comparisons between cultivars
      cat("Dunn's post-hoc test for all comparisons between cultivars:\n")
      posthoc_result <- dunn.test(subset_data$Chlorophyll, subset_data$Cultivars, method = "bonferroni")
      print(posthoc_result)
      
    } else {
      cat("Skipping test for Treatment", t, "on date", d, "due to insufficient levels in Cultivars\n")
    }
  }
}
library(ARTool)

# Ensure categorical variables are factors
chl_data$Treatment <- as.factor(chl_data$Treatment)
chl_data$Cultivars <- as.factor(chl_data$Cultivars)
chl_data$Date <- as.factor(chl_data$Date)

chl_data_art <- art(Chlorophyll ~ Treatment * Cultivars * Date, data = chl_data)
anova(chl_data_art)

library(ggplot2)

# Ensure the Date column is ordered correctly
chl_data$Date <- factor(chl_data$Date, 
                                levels = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"))

# Create the plot
plot <- ggplot(chl_data, aes(x = Date, y = Chlorophyll, fill = interaction(Treatment, Cultivars))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Sampling Time (Weeks)",
    y = "Chlorophyll",
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
ggsave(filename = "boxplotchl.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")


