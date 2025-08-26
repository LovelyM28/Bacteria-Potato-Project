library(dplyr)
library(VennDiagram)
library(grid)

# Filtering and selecting relevant data
rhizosphere_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Rhizosphere") %>%
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Defining unique genera
unique_genera_mandel_straw <- unique(rhizosphere_data$Genus[
  rhizosphere_data$Cultivar == "Mandel" & rhizosphere_data$Treatment == "Straw"])

unique_genera_mandel_non_straw <- unique(rhizosphere_data$Genus[
  rhizosphere_data$Cultivar == "Mandel" & rhizosphere_data$Treatment == "Non-straw"])

unique_genera_king_straw <- unique(rhizosphere_data$Genus[
  rhizosphere_data$Cultivar == "KingEdward" & rhizosphere_data$Treatment == "Straw"])

unique_genera_king_non_straw <- unique(rhizosphere_data$Genus[
  rhizosphere_data$Cultivar == "KingEdward" & rhizosphere_data$Treatment == "Non-straw"])

# Creating the list for Venn diagram
venn_list <- list(
  Mandel_non_straw = unique_genera_mandel_non_straw,
  Mandel_Straw = unique_genera_mandel_straw,
  KingEdward_non_straw = unique_genera_king_non_straw,
  KingEdward_Straw = unique_genera_king_straw
)

# Create the Venn diagram (but do not draw yet)
venn.plot_rhizosphere <- venn.diagram(
  x = venn_list,
  category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
  filename = NULL,  # Do not save yet
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Open the png device to save the Venn diagram
png(filename = "venn_diagram_rhizosphere.png", width = 1000, height = 1000)

# Draw the Venn diagram on the device
grid.draw(venn.plot_rhizosphere)

# Close the device to save the file
dev.off()

# Message indicating success
cat("Venn diagram saved as 'venn_diagram_rhizosphere.png'\n")


library(dplyr)
library(VennDiagram)
library(grid)

# Filtering and selecting relevant data for Root samples
root_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Root") %>%  # Filter for Root instead of Rhizosphere
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Defining unique genera for Root samples
unique_genera_mandel_straw <- unique(root_data$Genus[
  root_data$Cultivar == "Mandel" & root_data$Treatment == "Straw"])

unique_genera_mandel_non_straw <- unique(root_data$Genus[
  root_data$Cultivar == "Mandel" & root_data$Treatment == "Non-straw"])

unique_genera_king_straw <- unique(root_data$Genus[
  root_data$Cultivar == "KingEdward" & root_data$Treatment == "Straw"])

unique_genera_king_non_straw <- unique(root_data$Genus[
  root_data$Cultivar == "KingEdward" & root_data$Treatment == "Non-straw"])

# Creating the list for Venn diagram
venn_list <- list(
  Mandel_non_straw = unique_genera_mandel_non_straw,
  Mandel_Straw = unique_genera_mandel_straw,
  KingEdward_non_straw = unique_genera_king_non_straw,
  KingEdward_Straw = unique_genera_king_straw
)

# Create the Venn diagram (but do not draw yet)
venn.plot_root <- venn.diagram(
  x = venn_list,
  category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
  filename = NULL,  # Do not save yet
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Open the png device to save the Venn diagram
png(filename = "venn_diagram_root.png", width = 1000, height = 1000)

# Draw the Venn diagram on the device
grid.draw(venn.plot_root)

# Close the device to save the file
dev.off()

# Message indicating success
cat("Venn diagram saved as 'venn_diagram_root.png'\n")



library(dplyr)
library(VennDiagram)
library(grid)

# Filtering and selecting relevant data for Soil samples
soil_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Soil") %>%  # Filter for Soil
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Defining unique genera for Soil samples
unique_genera_mandel_straw <- unique(soil_data$Genus[
  soil_data$Cultivar == "Mandel" & soil_data$Treatment == "Straw"])

unique_genera_mandel_non_straw <- unique(soil_data$Genus[
  soil_data$Cultivar == "Mandel" & soil_data$Treatment == "Non-straw"])

unique_genera_king_straw <- unique(soil_data$Genus[
  soil_data$Cultivar == "KingEdward" & soil_data$Treatment == "Straw"])

unique_genera_king_non_straw <- unique(soil_data$Genus[
  soil_data$Cultivar == "KingEdward" & soil_data$Treatment == "Non-straw"])

# Creating the list for Venn diagram
venn_list <- list(
  Mandel_non_straw = unique_genera_mandel_non_straw,
  Mandel_Straw = unique_genera_mandel_straw,
  KingEdward_non_straw = unique_genera_king_non_straw,
  KingEdward_Straw = unique_genera_king_straw
)

# Create the Venn diagram (but do not draw yet)
venn.plot_soil <- venn.diagram(
  x = venn_list,
  category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
  filename = NULL,  # Do not save yet
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Open the png device to save the Venn diagram
png(filename = "venn_diagram_soil.png", width = 1000, height = 1000)

# Draw the Venn diagram on the device
grid.draw(venn.plot_soil)

# Close the device to save the file
dev.off()

# Message indicating success
cat("Venn diagram saved as 'venn_diagram_soil.png'\n")


library(dplyr)
library(VennDiagram)
library(grid)

# Filtering and selecting relevant data for Tuber Peel samples
tuber_peel_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Tuber Peel") %>%  # Filter for Tuber Peel
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Defining unique genera for Tuber Peel samples
unique_genera_mandel_straw <- unique(tuber_peel_data$Genus[
  tuber_peel_data$Cultivar == "Mandel" & tuber_peel_data$Treatment == "Straw"])

unique_genera_mandel_non_straw <- unique(tuber_peel_data$Genus[
  tuber_peel_data$Cultivar == "Mandel" & tuber_peel_data$Treatment == "Non-straw"])

unique_genera_king_straw <- unique(tuber_peel_data$Genus[
  tuber_peel_data$Cultivar == "KingEdward" & tuber_peel_data$Treatment == "Straw"])

unique_genera_king_non_straw <- unique(tuber_peel_data$Genus[
  tuber_peel_data$Cultivar == "KingEdward" & tuber_peel_data$Treatment == "Non-straw"])

# Creating the list for Venn diagram
venn_list <- list(
  Mandel_non_straw = unique_genera_mandel_non_straw,
  Mandel_Straw = unique_genera_mandel_straw,
  KingEdward_non_straw = unique_genera_king_non_straw,
  KingEdward_Straw = unique_genera_king_straw
)

# Create the Venn diagram (but do not draw yet)
venn.plot_tuber_peel <- venn.diagram(
  x = venn_list,
  category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
  filename = NULL,  # Do not save yet
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Open the png device to save the Venn diagram
png(filename = "venn_diagram_tuber_peel.png", width = 1000, height = 1000)

# Draw the Venn diagram on the device
grid.draw(venn.plot_tuber_peel)

# Close the device to save the file
dev.off()

# Message indicating success
cat("Venn diagram saved as 'venn_diagram_tuber_peel.png'\n")





# Load required libraries
library(dplyr)
library(VennDiagram)
library(grid)
library(gridExtra)  # For arranging grid objects

# Define a function to create Venn diagrams for a given dataset
create_venn_diagram <- function(data, title) {
  # Defining unique genera for each treatment and cultivar
  unique_genera_mandel_straw <- unique(data$Genus[data$Cultivar == "Mandel" & data$Treatment == "Straw"])
  unique_genera_mandel_non_straw <- unique(data$Genus[data$Cultivar == "Mandel" & data$Treatment == "Non-straw"])
  unique_genera_king_straw <- unique(data$Genus[data$Cultivar == "KingEdward" & data$Treatment == "Straw"])
  unique_genera_king_non_straw <- unique(data$Genus[data$Cultivar == "KingEdward" & data$Treatment == "Non-straw"])
  
  # Creating the list for Venn diagram
  venn_list <- list(
    Mandel_non_straw = unique_genera_mandel_non_straw,
    Mandel_Straw = unique_genera_mandel_straw,
    KingEdward_non_straw = unique_genera_king_non_straw,
    KingEdward_Straw = unique_genera_king_straw
  )
  
  # Create the Venn diagram (do not save yet)
  venn_plot <- venn.diagram(
    x = venn_list,
    category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
    filename = NULL,  # Do not save to file yet
    output = TRUE,
    lwd = 2,
    lty = "dashed",
    fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
    cex = 1.5,  # Text size for counts
    fontface = "bold",  # Font style for counts
    cat.fontface = "plain",  # Font style for category labels (non-bold)
    cat.cex = 1.0,  # Text size for category labels
    main = title,  # Title for each plot
    main.cex = 1.5,  # Text size for the main title
    main.fontface = "bold"  # Font style for the main title
  )
  
  return(venn_plot)
}

# 1. Filtering and selecting relevant data for each sample type

# For Rhizosphere
rhizosphere_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Rhizosphere") %>%
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)

venn_rhizosphere <- create_venn_diagram(rhizosphere_data, "Rhizosphere")

# For Root
root_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Root") %>%
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)

venn_root <- create_venn_diagram(root_data, "Root")

# For Soil
soil_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Soil") %>%
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)

venn_soil <- create_venn_diagram(soil_data, "Soil")

# For Tuber Peel
tuber_peel_data <- physeq_melted_split_filtered %>%
  filter(Sample_type == "Tuber Peel") %>%
  filter(Cultivar %in% c("Mandel", "KingEdward")) %>%
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%
  select(Sample_type, Cultivar, Treatment, Abundance, Genus)

venn_tuber_peel <- create_venn_diagram(tuber_peel_data, "Tuber Peel")

# 2. Combine all Venn diagrams into one panel using gridExtra
combined_venn <- grid.arrange(
  venn_rhizosphere,
  venn_root,
  venn_soil,
  venn_tuber_peel,
  ncol = 2  # Specify the number of columns
)

# Save the combined plot as a PNG file
png("combined_venn_diagrams.png", width = 1200, height = 800)
grid.draw(combined_venn)
dev.off()

# Message indicating success
cat("Combined Venn diagrams saved as 'combined_venn_diagrams.png'\n")










library(dplyr)
library(VennDiagram)
library(grid)

# Filtering and selecting relevant data for Tuber Peel samples
mandel_data <- physeq_melted_split_filtered %>%
  filter(Cultivar == "Mandel") %>%  # Filter for Mandel
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Cultivar, Sample_type, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

king_data <- physeq_melted_split_filtered %>%
  filter(Cultivar == "KingEdward") %>%  # Filter for Mandel
  filter(Treatment %in% c("Straw", "Non-straw")) %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Cultivar, Sample_type, Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Defining unique genera for Mandel
unique_genera_mandel_straw <- unique(mandel_data$Genus[
  mandel_data$Treatment == "Straw"])

unique_genera_mandel_non_straw <- unique(mandel_data$Genus[
  mandel_data$Treatment == "Non-straw"])

unique_genera_king_straw <- unique(king_data$Genus[
  king_data$Treatment == "Straw"])

unique_genera_king_non_straw <- unique(king_data$Genus[
  king_data$Treatment == "Non-straw"])

# Creating the list for Venn diagram
venn_list <- list(
  Mandel_non_straw = unique_genera_mandel_non_straw,
  Mandel_Straw = unique_genera_mandel_straw,
  KingEdward_non_straw = unique_genera_king_non_straw,
  KingEdward_Straw = unique_genera_king_straw
)

# Create the Venn diagram (but do not draw yet)
venn.plot_cultivar <- venn.diagram(
  x = venn_list,
  category.names = c("Mandel Non-Straw", "Mandel Straw", "KingEdward Non-Straw", "KingEdward Straw"),
  filename = NULL,  # Do not save yet
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightblue", "lightgreen", "lightcoral", "lightyellow"),  # Distinct colors
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Open the png device to save the Venn diagram
png(filename = "venn_diagram_cultivar.png", width = 1000, height = 1000)

# Draw the Venn diagram on the device
grid.draw(venn.plot_cultivar)

# Close the device to save the file
dev.off()

# Message indicating success
cat("Venn diagram saved as 'venn_diagram_cultivar.png'\n")






# Load necessary libraries
library(dplyr)
library(VennDiagram)
library(grid)

# Filter Non-straw data
Non_straw_data <- physeq_melted_split_filtered %>%
  filter(Treatment == "Non-straw") %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Filter Straw data
Straw_data <- physeq_melted_split_filtered %>%
  filter(Treatment == "Straw") %>%
  filter(Abundance > 0) %>%  # Filter based on abundance threshold
  select(Treatment, Abundance, Genus)  # Keep only relevant columns, including Genus

# Clean data: remove rows with empty genus names and duplicates
Straw_data <- Straw_data %>% 
  filter(Genus != "") %>% 
  distinct(Genus, .keep_all = TRUE)

Non_straw_data <- Non_straw_data %>% 
  filter(Genus != "") %>% 
  distinct(Genus, .keep_all = TRUE)

# Make Genus names lowercase for case-insensitive comparison
Straw_data$Genus <- tolower(Straw_data$Genus)
Non_straw_data$Genus <- tolower(Non_straw_data$Genus)

# Get unique genera for Straw and Non-straw
unique_genera_straw <- unique(Straw_data$Genus)
unique_genera_non_straw <- unique(Non_straw_data$Genus)

# Find overlapping genera between Straw and Non-straw
overlap_genera <- intersect(unique_genera_straw, unique_genera_non_straw)

# Find unique genera without overlap for both treatments
unique_genera_straw_no_overlap <- setdiff(unique_genera_straw, overlap_genera)
unique_genera_non_straw_no_overlap <- setdiff(unique_genera_non_straw, overlap_genera)

# Print unique genera without overlap for both treatments
cat("Unique genera without overlap in Straw:", unique_genera_straw_no_overlap, "\n")
cat("Number of unique genera in Straw without overlap:", length(unique_genera_straw_no_overlap), "\n")
cat("Unique genera without overlap in Non-straw:", unique_genera_non_straw_no_overlap, "\n")
cat("Number of unique genera in Non-straw without overlap:", length(unique_genera_non_straw_no_overlap), "\n")

# Prepare the Venn diagram data
venn_list <- list(
  Straw = unique_genera_straw,
  Non_straw = unique_genera_non_straw
)

# Create the Venn diagram and save it as a PNG
venn.plot <- venn.diagram(
  x = venn_list,
  category.names = c("Straw", "Non-Straw"),
  filename = "venn_diagram_treatment.png",  # Save as PNG
  output = TRUE,
  lwd = 2,
  lty = "dashed",
  fill = c("lightcoral", "lightgreen"),  # Distinct colors for each category
  cex = 1.5,
  fontface = "bold",
  cat.fontface = "bold",
  cat.cex = 1.2
)

# Close the graphical device if open
dev.off()
library(writexl)

write_xlsx(unique_straw_abundance, "unique_straw_abundance.xlsx")
write_xlsx(unique_non_straw_abundance, "unique_non_straw_abundance.xlsx")

# Print confirmation messages
cat("Saved unique genera without overlap in Straw to 'unique_straw_abundance.xlsx'.\n")
cat("Saved unique genera without overlap in Non-straw to 'unique_non_straw_abundance.xlsx'.\n")
