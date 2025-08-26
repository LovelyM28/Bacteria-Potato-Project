# Step 1: Install the 'readxl' package (if not already installed)
install.packages("readxl")

# Step 2: Load the 'readxl' package
library(readxl)

# Step 3: Define the file path (use either double backslashes or forward slashes)
file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/abs-abund-table-6.xlsx"

# Step 4: Read the Excel file (by default, it reads the first sheet)
data <- read_excel(file_path)

# Step 5: Preview the data (prints the first few rows)
head(data)

data_df <- as.data.frame(data)

# Step 6: Preview the data frame (prints the first few rows)
head(data_df)
otu_mat <- as.matrix(data_df[, sapply(data_df, is.numeric)])  # Create matrix with numeric data only

install.packages("hillR")
library(hillR)
Species_richness<- hill_taxa(otu_mat, q=0, MARGIN = 2, base = exp(1))

Shannon_index<- hill_taxa(otu_mat, q=1, MARGIN = 2, base = exp(1))
file_path <- "C:\\Users\\loma0112\\OneDrive - Umeå universitet\\Documents\\Kempe project\\Summer field experiment 2023\\metagenomics from potato field experiment\\Plots and codes\\Sample_sheet_bacteria.xlsx"
# Step 4: Read the Excel file into R (by default, it reads the first sheet)
sample_data <- read_excel(file_path)
sample_data_df <- as.data.frame(sample_data)
combined_df <- data.frame(
  Sample = colnames(otu_mat),
  Species_Richness = Species_richness,
  Shannon_Index = Shannon_index)
combined_df <- merge(combined_df, sample_data_df, by.x = "Sample", by.y = "Sample")
View(combined_df)
library(tidyr)
library(dplyr)
long_df <- combined_df %>%
  pivot_longer(cols = c(Species_Richness, Shannon_Index),
               names_to = "Diversity_Index",
               values_to = "Value")

library(ggplot2)
long_df$Cultivar_Treatment <- interaction(long_df$Cultivar, long_df$Treatment, sep = " ", drop = TRUE)
long_df$Cultivar_Treatment <- factor(long_df$Cultivar_Treatment,
                                     levels = c("Mandel Control", "Mandel Mulch", 
                                                "KingEdward Control", "KingEdward Mulch"))


shannon_df <- long_df %>% filter(Diversity_Index == "Shannon_Index")


plot_shannon_boxplot <- ggplot(shannon_df, aes(x = Cultivar, y = Value, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Shannon Index by Sample Type and Treatment",
       x = "Cultivar",
       y = "Shannon Index",
       fill = "Sample Type and Treatment") +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Print the plot
print(plot_shannon_boxplot)

shannon_df$Sample_type_Treatment <- paste(shannon_df$Sample_type, shannon_df$Treatment, sep = " ")

plot_shannon_boxplot <- ggplot(shannon_df, aes(x = Cultivar, y = Value, fill = Sample_type_Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = NULL,           # Removes the x-axis label
    y = "Shannon Index",
    fill = NULL         # Removes the legend title
  ) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Print the plot
print(plot_shannon_boxplot)
# Save the plot as a PNG file
ggsave(filename = "plot_shannon_boxplot.png", plot = plot_shannon_boxplot, width = 10, height = 7, units = "in", dpi = 300)


Shannon_Index_df <- long_df %>% filter(Diversity_Index == "Shannon_Index")
#Create the box plot for Shannon_Index with Sample_type on the x-axis
plot_Shannon_Index_boxplot <- ggplot(Shannon_Index_df, aes(x = Sample_type, y = Value, fill = Cultivar_Treatment)) +
  geom_boxplot() +
  labs(title = "Distribution of Shannon Index by Sample Type",
       x = "Sample Type",
       y = "Shannon Index",
       fill = "Cultivar and Treatment") +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1), # Increase and bold x-axis text, rotate for readability
    axis.text.y = element_text(size = 14, face = "bold"),                        # Increase and bold y-axis text
    axis.title.x = element_text(size = 16, face = "bold"),                        # Increase x-axis title size
    axis.title.y = element_text(size = 16, face = "bold"),                        # Increase y-axis title size
    legend.text = element_text(size = 14),                                        # Increase legend text size
    legend.title = element_text(size = 14, face = "bold"),                        # Increase and bold legend title size
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)        # Add border around the plot
  )

# Print the plot
print(plot_Shannon_Index_boxplot)



filtered_data_root <- shannon_df %>% filter(Sample_type == "Root")

# Perform ANOVA for Root samples
anova_results_root <- aov(Value ~ Cultivar * Treatment, data = filtered_data_root)

# Display ANOVA summary
summary(anova_results_root)

# Perform Tukey's HSD test for Root samples
tukey_results_root <- TukeyHSD(anova_results_root)

# Print Tukey's post-hoc test results for Root samples
print(tukey_results_root)

# Filter data for Rhizosphere samples
filtered_data_rhizosphere <- shannon_df %>% filter(Sample_type == "Rhizosphere")

# Perform ANOVA for Rhizosphere samples
anova_results_rhizosphere <- aov(Value ~ Cultivar * Treatment, data = filtered_data_rhizosphere)

# Display ANOVA summary
summary(anova_results_rhizosphere)

# Perform Tukey's HSD test for Rhizosphere samples
tukey_results_rhizosphere <- TukeyHSD(anova_results_rhizosphere)

# Print Tukey's post-hoc test results for Rhizosphere samples
print(tukey_results_rhizosphere)

# Filter data for Tuber Peel samples
filtered_data_tuber_peel <- shannon_df %>% filter(Sample_type == "Tuber Peel")

# Perform ANOVA for Tuber Peel samples
anova_results_tuber_peel <- aov(Value ~ Cultivar * Treatment, data = filtered_data_tuber_peel)

# Display ANOVA summary
summary(anova_results_tuber_peel)

# Perform Tukey's HSD test for Tuber Peel samples
tukey_results_tuber_peel <- TukeyHSD(anova_results_tuber_peel)

# Print Tukey's post-hoc test results for Tuber Peel samples
print(tukey_results_tuber_peel)

# Filter data for Soil samples
filtered_data_soil <- shannon_df %>% filter(Sample_type == "Soil")

# Perform ANOVA for Soil samples
anova_results_soil <- aov(Value ~ Cultivar * Treatment, data = filtered_data_soil)

# Display ANOVA summary
summary(anova_results_soil)

# Perform Tukey's HSD test for Soil samples
tukey_results_soil <- TukeyHSD(anova_results_soil)

# Print Tukey's post-hoc test results for Soil samples
print(tukey_results_soil)

#Species richness
Species_Richness_df <- long_df %>% filter(Diversity_Index == "Species_Richness")
#Create the box plot for Species_Richness with Sample_type on the x-axis
plot_Species_Richness_boxplot <- ggplot(Species_Richness_df, aes(x = Sample_type, y = Value, fill = Cultivar_Treatment)) +
  geom_boxplot() +
  labs(title = "Distribution of Species Richness by Sample Type",
       x = "Sample Type",
       y = "Species Richness",
       fill = "Cultivar and Treatment") +
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1), # Increase and bold x-axis text, rotate for readability
    axis.text.y = element_text(size = 14, face = "bold"),                        # Increase and bold y-axis text
    axis.title.x = element_text(size = 16, face = "bold"),                        # Increase x-axis title size
    axis.title.y = element_text(size = 16, face = "bold"),                        # Increase y-axis title size
    legend.text = element_text(size = 14),                                        # Increase legend text size
    legend.title = element_text(size = 14, face = "bold"),                        # Increase and bold legend title size
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)        # Add border around the plot
  ) +
  scale_y_continuous(limits = c(400, 800), breaks = seq(400, 800, by = 200))     # Y-axis from 100 to 800, intervals of 100

# Print the plot
print(plot_Species_Richness_boxplot)


Species_Richness_df$Sample_type_Treatment <- paste(Species_Richness_df$Sample_type, Species_Richness_df$Treatment, sep = " ")

plot_species_boxplot <- ggplot(Species_Richness_df, aes(x = Cultivar, y = Value, fill = Sample_type_Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = NULL,           # Removes the x-axis label
    y = "Species Richness",
    fill = NULL         # Removes the legend title
  ) +
  scale_y_continuous(limits = c(400, 700)) +  # Adjust the y-axis scale from 400 to 700
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Print the plot
print(plot_species_boxplot)

ggsave(filename = "plot_species_richness_boxplot.png", plot = plot_species_boxplot, width = 10, height = 7, units = "in", dpi = 300)




filtered_data_root_sr <- Species_Richness_df %>% filter(Sample_type == "Root")

# Perform two-way ANOVA
anova_results_root <- aov(Value ~ Cultivar * Treatment, data = filtered_data_root_sr)

# Print the summary of ANOVA
summary(anova_results_root)

# Perform Tukey's HSD test for Root samples
tukey_results_root <- TukeyHSD(anova_results_root)

# Print Tukey's test results
print(tukey_results_root)

# Filter the data for Rhizosphere samples
filtered_data_rhizosphere_sr <- Species_Richness_df %>% filter(Sample_type == "Rhizosphere")

# Perform two-way ANOVA
anova_results_rhizosphere <- aov(Value ~ Cultivar * Treatment, data = filtered_data_rhizosphere_sr)

# Print the summary of ANOVA
summary(anova_results_rhizosphere)

# Perform Tukey's HSD test for Rhizosphere samples
tukey_results_rhizosphere <- TukeyHSD(anova_results_rhizosphere)

# Print Tukey's test results
print(tukey_results_rhizosphere)


# Filter the data for Tuber Peel samples
filtered_data_tuber_peel_sr <- Species_Richness_df %>% filter(Sample_type == "Tuber Peel")

# Perform two-way ANOVA
anova_results_tuber_peel <- aov(Value ~ Cultivar * Treatment, data = filtered_data_tuber_peel_sr)

# Print the summary of ANOVA
summary(anova_results_tuber_peel)

# Perform Tukey's HSD test for Tuber Peel samples
tukey_results_tuber_peel <- TukeyHSD(anova_results_tuber_peel)

# Print Tukey's test results
print(tukey_results_tuber_peel)

# Filter the data for Soil samples
filtered_data_soil_sr <- Species_Richness_df %>% filter(Sample_type == "Soil")

# Perform two-way ANOVA
anova_results_soil <- aov(Value ~ Cultivar * Treatment, data = filtered_data_soil_sr)

# Print the summary of ANOVA
summary(anova_results_soil)

# Perform Tukey's HSD test for Soil samples
tukey_results_soil <- TukeyHSD(anova_results_soil)

# Print Tukey's test results
print(tukey_results_soil)

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Create the Shannon Index boxplot without titles and without legend
plot_shannon_boxplot <- ggplot(shannon_df, aes(x = Sample_type, y = Value, fill = Cultivar_Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = NULL,  # Remove plot title
       x = NULL,      # Remove X-axis title
       y = "Shannon Index") +  # Keep Y-axis title
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),  # Keep Y-axis title size
    legend.position = "none",  # Remove individual plot legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add border around the plot
  )

# Create the Species Richness boxplot without titles and without legend
plot_species_richness_boxplot <- ggplot(Species_Richness_df, aes(x = Sample_type, y = Value, fill = Cultivar_Treatment)) +
  geom_boxplot() +
  labs(title = NULL,  # Remove plot title
       x = NULL,      # Remove X-axis title
       y = "Species Richness") +  # Keep Y-axis title
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1), # Increase and bold x-axis text, rotate for readability
    axis.text.y = element_text(size = 14, face = "bold"),                        # Increase and bold y-axis text
    axis.title.y = element_text(size = 16, face = "bold"),                        # Keep Y-axis title size
    legend.position = "none",  # Remove individual plot legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)        # Add border around the plot
  ) +
  scale_y_continuous(limits = c(400, 800), breaks = seq(400, 800, by = 200))     # Y-axis from 400 to 800, intervals of 200

# Combine the two plots into two rows and keep the legend for the combined plot
combined_plot <- (plot_shannon_boxplot / plot_species_richness_boxplot) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'right')  # Collect legends and position it to the right

# Print the combined plot
print(combined_plot)



#Combined Plot with Cultivar on X -axis
shannon_df$Sample_type_Treatment <- paste(shannon_df$Sample_type, shannon_df$Treatment, sep = " ")

Species_Richness_df$Sample_type_Treatment <- paste(Species_Richness_df$Sample_type, Species_Richness_df$Treatment, sep = " ")

plot_shannon_boxplot <- ggplot(shannon_df, aes(x = Cultivar, y = Value, fill = Sample_type_Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = NULL,           # Removes the x-axis label
    y = "Shannon Index",
    fill = NULL         # Removes the legend title
  ) +
  scale_fill_manual(values = c("Soil Control" = "#ff6347", "Soil Mulch" = "#ff0000", "Rhizosphere Control" = "#ffbf00","Rhizosphere Mulch" = "#c46210", "Root Control" = "#66b032", "Root Mulch" = "#008000", "Tuber Peel Control" = "#1e90ff", "Tuber Peel Mulch" = "#0000ff"  )) +  # Specify your colors
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = "none",  # Remove individual plot legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

plot_species_boxplot <- ggplot(Species_Richness_df, aes(x = Cultivar, y = Value, fill = Sample_type_Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = NULL,           # Removes the x-axis label
    y = "Species Richness",
    fill = NULL         # Removes the legend title
  ) +
  scale_y_continuous(limits = c(400, 700)) +  # Adjust the y-axis scale from 400 to 700
  scale_fill_manual(values = c("Soil Control" = "#ff6347", "Soil Mulch" = "#ff0000", "Rhizosphere Control" = "#ffbf00","Rhizosphere Mulch" = "#c46210", "Root Control" = "#66b032", "Root Mulch" = "#008000", "Tuber Peel Control" = "#1e90ff",  "Tuber Peel Mulch" = "#0000ff"  )) +  # Specify your colors
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = "none",  # Remove individual plot legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Combine the two plots into two rows and keep the legend for the combined plot
combined_plot <- (plot_species_boxplot / plot_shannon_boxplot) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'right', legend.text = element_text(size = 11))  # Collect legends and position it to the right

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PNG file
ggsave(
  filename = "combined_plot.png",  # Specify the file name
  plot = combined_plot,           # The plot object to save
  width = 10,                     # Width of the output image in inches
  height = 8,                     # Height of the output image in inches
  dpi = 300                       # Resolution of the output image
)






# Load the dplyr package
library(dplyr)

# Filter the data to keep only rows where Cultivar is "KingEdward"
kingedward_soil_sr <- filtered_data_soil_sr %>%
  filter(Cultivar == "KingEdward")


kingedward_root_sr <- filtered_data_root_sr %>%
  filter(Cultivar == "KingEdward")


kingedward_rhizosphere_sr <- filtered_data_rhizosphere_sr %>%
  filter(Cultivar == "KingEdward")


kingedward_tuberpeel_sr <- filtered_data_tuber_peel_sr %>%
  filter(Cultivar == "KingEdward")


kingedward_soil_shannon <- filtered_data_soil %>%
  filter(Cultivar == "KingEdward")


kingedward_root_shannon <- filtered_data_root %>%
  filter(Cultivar == "KingEdward")


kingedward_rhizosphere_shannon <- filtered_data_rhizosphere %>%
  filter(Cultivar == "KingEdward")


kingedward_tuberpeel_shannon <- filtered_data_tuber_peel %>%
  filter(Cultivar == "KingEdward")

# Filter the data to keep only rows where Cultivar is "KingEdward"
kingedward_data <- combined_df %>%
  filter(Cultivar == "KingEdward")

pairwise_t_test_results_kingEdward_speciesrichness <- pairwise.t.test(
  kingedward_data$Species_Richness,  # Corrected to match column name
  kingedward_data$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_speciesrichness)

pairwise_t_test_results_kingEdward_shannon <- pairwise.t.test(
  kingedward_data$Shannon_Index,  # Corrected to match column name
  kingedward_data$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_shannon)



pairwise_t_test_results_kingEdward_soil_speciesrichness <- pairwise.t.test(
  kingedward_soil_sr$Value,  # Corrected to match column name
  kingedward_soil_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_soil_speciesrichness)



pairwise_t_test_results_kingEdward_root_speciesrichness <- pairwise.t.test(
  kingedward_root_sr$Value,  # Corrected to match column name
  kingedward_root_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_root_speciesrichness)



pairwise_t_test_results_kingEdward_rhizosphere_speciesrichness <- pairwise.t.test(
  kingedward_rhizosphere_sr$Value,  # Corrected to match column name
  kingedward_rhizosphere_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_rhizosphere_speciesrichness)


pairwise_t_test_results_kingEdward_tuberpeel_speciesrichness <- pairwise.t.test(
  kingedward_tuberpeel_sr$Value,  # Corrected to match column name
  kingedward_tuberpeel_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_tuberpeel_speciesrichness)


pairwise_t_test_results_kingEdward_soil_shannon <- pairwise.t.test(
  kingedward_soil_shannon$Value,  # Corrected to match column name
  kingedward_soil_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_soil_shannon)



pairwise_t_test_results_kingEdward_root_shannon <- pairwise.t.test(
  kingedward_root_shannon$Value,  # Corrected to match column name
  kingedward_root_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_root_shannon)



pairwise_t_test_results_kingEdward_rhizosphere_shannon <- pairwise.t.test(
  kingedward_rhizosphere_shannon$Value,  # Corrected to match column name
  kingedward_rhizosphere_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_rhizosphere_shannon)


pairwise_t_test_results_kingEdward_tuberpeel_shannon <- pairwise.t.test(
  kingedward_tuberpeel_shannon$Value,  # Corrected to match column name
  kingedward_tuberpeel_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_kingEdward_tuberpeel_shannon)


# Filter the data to keep only rows where Cultivar is "Mandel"
mandel_soil_sr <- filtered_data_soil_sr %>%
  filter(Cultivar == "Mandel")


mandel_root_sr <- filtered_data_root_sr %>%
  filter(Cultivar == "Mandel")


mandel_rhizosphere_sr <- filtered_data_rhizosphere_sr %>%
  filter(Cultivar == "Mandel")


mandel_tuberpeel_sr <- filtered_data_tuber_peel_sr %>%
  filter(Cultivar == "Mandel")


mandel_soil_shannon <- filtered_data_soil %>%
  filter(Cultivar == "Mandel")


mandel_root_shannon <- filtered_data_root %>%
  filter(Cultivar == "Mandel")


mandel_rhizosphere_shannon <- filtered_data_rhizosphere %>%
  filter(Cultivar == "Mandel")


mandel_tuberpeel_shannon <- filtered_data_tuber_peel %>%
  filter(Cultivar == "Mandel")

# Filter the data to keep only rows where Cultivar is "Mandel"
mandel_data <- combined_df %>%
  filter(Cultivar == "Mandel")


pairwise_t_test_results_mandel_speciesrichness <- pairwise.t.test(
  mandel_data$Species_Richness,  # Corrected to match column name
  mandel_data$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_speciesrichness)

pairwise_t_test_results_mandel_shannon <- pairwise.t.test(
  mandel_data$Shannon_Index,  # Corrected to match column name
  mandel_data$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_shannon)



pairwise_t_test_results_mandel_soil_speciesrichness <- pairwise.t.test(
  mandel_soil_sr$Value,  # Corrected to match column name
  mandel_soil_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_soil_speciesrichness)



pairwise_t_test_results_mandel_root_speciesrichness <- pairwise.t.test(
  mandel_root_sr$Value,  # Corrected to match column name
  mandel_root_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_root_speciesrichness)



pairwise_t_test_results_mandel_rhizosphere_speciesrichness <- pairwise.t.test(
  mandel_rhizosphere_sr$Value,  # Corrected to match column name
  mandel_rhizosphere_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_rhizosphere_speciesrichness)


pairwise_t_test_results_mandel_tuberpeel_speciesrichness <- pairwise.t.test(
  mandel_tuberpeel_sr$Value,  # Corrected to match column name
  mandel_tuberpeel_sr$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_tuberpeel_speciesrichness)


pairwise_t_test_results_mandel_soil_shannon <- pairwise.t.test(
  mandel_soil_shannon$Value,  # Corrected to match column name
  mandel_soil_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_soil_shannon)



pairwise_t_test_results_mandel_root_shannon <- pairwise.t.test(
  mandel_root_shannon$Value,  # Corrected to match column name
  mandel_root_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_root_shannon)



pairwise_t_test_results_mandel_rhizosphere_shannon <- pairwise.t.test(
  mandel_rhizosphere_shannon$Value,  # Corrected to match column name
  mandel_rhizosphere_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_rhizosphere_shannon)


pairwise_t_test_results_mandel_tuberpeel_shannon <- pairwise.t.test(
  mandel_tuberpeel_shannon$Value,  # Corrected to match column name
  mandel_tuberpeel_shannon$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_mandel_tuberpeel_shannon)



pairwise_t_test_results_treatment_shannon <- pairwise.t.test(
  combined_df$Shannon_Index,  # Corrected to match column name
  combined_df$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_treatment_shannon)


pairwise_t_test_results_treatment_speciesrichness <- pairwise.t.test(
  combined_df$Species_Richness,  # Corrected to match column name
  combined_df$Treatment,         # Grouping variable
  p.adjust.method = "bonferroni"     # Adjust p-values if needed
)


# View the results
print(pairwise_t_test_results_treatment_speciesrichness)

#Three way annova

anova_results_speciesrichness <- aov(Species_Richness ~ Cultivar * Treatment * Sample_type , data = combined_df)

# Display ANOVA summary
summary(anova_results_speciesrichness)

anova_results_speciesrichness2 <- aov(Species_Richness ~ Cultivar + Treatment + Sample_type , data = combined_df)

# Display ANOVA summary
summary(anova_results_speciesrichness2)


anova_results_shannonindex <- aov(Shannon_Index ~ Cultivar * Treatment * Sample_type , data = combined_df)

# Display ANOVA summary
summary(anova_results_shannonindex)

anova_results_shannonindex2 <- aov(Shannon_Index ~ Cultivar + Treatment + Sample_type , data = combined_df)

# Display ANOVA summary
summary(anova_results_shannonindex2)



library(dplyr)

# Remove rows where Sample_type is "Tuber Peel"
combined_df_filtered <- combined_df %>%
  filter(Sample_type != "Tuber Peel")

# Check the structure of the updated data
str(combined_df_filtered)

anova_results_shannonindex_filtered <- aov(Shannon_Index ~ Cultivar * Treatment * Sample_type , data = combined_df_filtered)

# Display ANOVA summary
summary(anova_results_shannonindex_filtered)

anova_results_shannonindex_filtered2 <- aov(Shannon_Index ~ Cultivar + Treatment + Sample_type , data = combined_df_filtered)

# Display ANOVA summary
summary(anova_results_shannonindex_filtered2)

anova_results_speciesrichness_filtered <- aov(Species_Richness ~ Cultivar * Treatment * Sample_type , data = combined_df_filtered)
# Display ANOVA summary
summary(anova_results_speciesrichness_filtered)

anova_results_speciesrichness_filtered2 <- aov(Species_Richness ~ Cultivar + Treatment + Sample_type , data = combined_df_filtered)

# Display ANOVA summary
summary(anova_results_speciesrichness_filtered2)

#Combined Plot of Species Richness nad Shannon index by cultivar and Treatment type

library(ggplot2)
library(patchwork)

# Shannon Index Boxplot
plot_shannon_boxplot <- ggplot(shannon_df, aes(x = Cultivar, y = Value, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(y = "Shannon Index", fill = "Treatment") +  # Removed title and x-axis label
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = "none",        # Hide individual legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Species Richness Boxplot
Species_Richness_df <- long_df %>% filter(Diversity_Index == "Species_Richness")

plot_Species_Richness_boxplotcultivar <- ggplot(Species_Richness_df, aes(x = Cultivar, y = Value, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(y = "Species Richness", fill = "Treatment") +  # Removed title and x-axis label
  theme(
    axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = "none",        # Hide individual legend
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(limits = c(400, 700), breaks = seq(400, 700, by = 100))  

# Combine plots with a shared legend
combined_plot <- plot_Species_Richness_boxplotcultivar / plot_shannon_boxplot +
  plot_layout(guides = "collect") & theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_blank())  # Common legend at the bottom

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PNG file
ggsave(
  filename = "combined_plot.png",  # Specify the file name
  plot = combined_plot,           # The plot object to save
  width = 10,                     # Width of the output image in inches
  height = 8,                     # Height of the output image in inches
  dpi = 300                       # Resolution of the output image
)








