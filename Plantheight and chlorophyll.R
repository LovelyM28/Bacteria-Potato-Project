library(readxl)

# Set file path (use double backslashes or forward slashes)
library(readxl)

file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/Plant height and Chlorophyll/Chlorophyll/Chl_data.xlsx"

chl_data <- read_excel(file_path)

chl_data <- read_excel(file_path)

library(dplyr)

# Replace values in Treatment column
chl_data <- chl_data %>%
  mutate(Treatment = recode(Treatment,
                            "Non straw" = "Control",
                            "Straw" = "Mulch"))

# Check the changes
table(chl_data$Treatment)


plant_file_path <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/Plant height and Chlorophyll/Plant height/plant_data_clean.xlsx"
# Load the Plant Height dataset
plant_data_clean <- read_excel(plant_file_path)
# View the first few rows of each dataset
head(chl_data)
head(plant_data_clean)
plant_data_clean <- plant_data_clean %>%
  mutate(Treatment = recode(Treatment,
                            "Non straw" = "Control",
                            "Straw" = "Mulch"))


# View the datasets in RStudio
View(chl_data)
View(plant_data_clean)

# Check column names
colnames(chl_data)
colnames(plant_data_clean)

# Summary of datasets
summary(chl_data)
summary(plant_data_clean)
# Load necessary libraries
library(ggplot2)
library(gridExtra)
# Load necessary libraries
library(ggplot2)
library(patchwork)

# Create the first plot (Plant Height) without x-axis title
plot1 <- ggplot(plant_data_clean, aes(x = Date, y = Plant_height, fill = interaction(Treatment, Cultivars))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    y = "Plant Height (cm)",
    fill = "Cultivars & Treatment"
  ) +
  scale_fill_manual(
    name = "Variety.Treatment",
    values = c("#FFCC80", "#FF8C00", "#b2df8a", "#33a02c"), 
    labels = c("King Edward.Control", "King Edward.Mulch", "Mandel.Control", "Mandel.Mulch")  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),  # Remove x-axis title here
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

# Create the second plot (Chlorophyll) with x-axis title
plot2 <- ggplot(chl_data, aes(x = Date, y = Chlorophyll, fill = interaction(Treatment, Cultivars))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Sampling Time (Weeks)",  # x-axis title only here
    y = "Chlorophyll content (µg/cm²)",
    fill = "Cultivars & Treatment"
  ) +
  scale_fill_manual(
    name = "Variety.Treatment",
    values = c("#FFCC80", "#FF8C00", "#b2df8a", "#33a02c"), 
    labels = c("King Edward.Control", "King Edward.Mulch", "Mandel.Control", "Mandel.Mulch")  
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

# Combine the two plots with a shared legend
combined_plot <- plot1 / plot2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")  # Combined legend on the right side

# Print combined plot
combined_plot
print(combined_plot)
ggsave(
  filename = "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/combined_plot.png",
  plot = combined_plot,
  width = 10,   # Width in inches (adjust as needed)
  height = 12,  # Height in inches (adjust as needed)
  dpi = 300     # High resolution
)
