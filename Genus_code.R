# Load necessary libraries
library(dplyr)

physeq_melted_split_mandel <- physeq_melted_split_mandel %>%
  mutate(Treatment = recode(Treatment,
                            "Non-straw" = "Control",
                            "Straw" = "Mulch"))

# Step 1: Summarize total abundance by Genus, Treatment, and Sample_type
Genus_summary_mandel <- physeq_melted_split_mandel %>%
  group_by(Genus, Treatment, Sample_type) %>%
  summarise(total_abundance = sum(Abundance), .groups = 'drop')  # Assuming you want to summarize Abundance here

Genus_summary_filtered_mandel <- Genus_summary_mandel %>%
  filter(Genus != "") 
top_Genus_mandel <- Genus_summary_filtered_mandel %>%
  group_by(Genus) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop') %>%
  arrange(desc(total_abundance)) %>%
  slice_head(n = 11) %>%  # Select 21 to account for potential exclusion of the empty genus
  pull(Genus)
if (length(top_Genus_mandel) > 10) {
  top_Genus_mandel <- top_Genus_mandel[1:10]
}

# Step 4: Filter the dataset to include only these top 20 genera
Genus_summary_top10_mandel <- Genus_summary_filtered_mandel %>%
  filter(Genus %in% top_Genus_mandel)

# Step 5: Check the unique genera in the final filtered data
unique_Genus_mandel <- unique(Genus_summary_top10_mandel$Genus)

# Print unique genera for verification
print(unique_Genus_mandel)
color_codes <- c(
  "Acidibacter"                                        = "#FF5733",  # Red-Orange
  "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium" = "#228B22",  # Forest Green
  "Arthrobacter"                                       = "#8A2BE2",  # Blue Violet
  "Bryobacter"                                         = "#DAA520",  # Goldenrod
  "Candidatus Solibacter"                              = "#FF8C00",  # Dark Orange
  "Chryseobacterium"                                   = "#FF6347",  # Tomato
  "Clostridium sensu stricto 13"                       = "#7c4848",  # Crimson
  "Flavobacterium"                                     = "#44d7a8",  # Medium Sea Green
  "Gemmatimonas"                                       = "#4682B4",  # Steel Blue
  "Granulicella"                                       = "#6A5ACD",  # Slate Blue
  "Massilia"                                           = "#FFD700",  # Gold
  "Mucilaginibacter"                                   = "#20B2AA",  # Light Sea Green
  "Pedobacter"                                         = "#FF4500",  # Orange Red
  "Pseudolabrys"                                       = "#e4717a",  # Deep Pink
  "Pseudomonas"                                        = "#FFDAB9",  # Peach Puff
  "Rhodanobacter"                                      = "#32CD32",  # Lime Green
  "Sphingobacterium"                                   = "#8B008B",  # Dark Magenta
  "Sphingomonas"                                       = "#00BFFF",  # Deep Sky Blue
  "Streptomyces"                                       = "#C71585",  # Medium Violet Red
  "TM7a"                                               = "#FFA07A"   # Light Salmon
)
# Summarize the total abundance for top 20 genera
Genus_summary_abundance_mandel <- Genus_summary_top10_mandel %>%
  group_by(Genus) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop')

# Check if Genus_summary_abundance is created correctly
print("Genus Summary Abundance:")
print(Genus_summary_abundance_mandel)

# Check if Genus_summary_top20_mandel contains 'total_abundance' before reordering
if (!"total_abundance" %in% colnames(Genus_summary_top10_mandel)) {
  stop("Error: 'total_abundance' column not found in Genus_summary_top20_mandel")
}

# Step 5: Reorder Genus based on total abundance from bottom to top
Genus_summary_top10_mandel <- Genus_summary_top10_mandel %>%
  mutate(Genus = factor(Genus, 
                        levels = Genus_summary_abundance_mandel$Genus[order(Genus_summary_abundance_mandel$total_abundance)]))

# Check if the factor levels have been set correctly
print("Levels of Genus after reordering:")
print(levels(Genus_summary_top10_mandel$Genus))

# Calculate relative abundance before plotting
Genus_summary_top10_mandel <- Genus_summary_top10_mandel %>%
  group_by(Treatment) %>%
  mutate(relative_abundance = total_abundance / sum(total_abundance)) %>%
  ungroup()  # Ungroup after calculations if needed
library(ggplot2)
# Plotting the data
plot <- ggplot(Genus_summary_top10_mandel, aes(x = Treatment, y = relative_abundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  scale_fill_manual(values = color_codes) +  # Use the genus color palette
  labs(
    title = "Top 10 Genera by Abundance in Mandel Cultivar (Excluding Unassigned)",
    x = "Treatment",
    y = "Relative Abundance (%)",
    fill = "Genus"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~ Sample_type, nrow = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines")
  )

# Step 6: Print the plot
print(plot)
# Load the necessary library
library(Cairo)

# Save the plot using CairoPNG
CairoPNG(filename = "top_10_genera_mandel.png", width = 800, height = 600, units = "px", res = 100)

# Print the plot to the graphics device
print(plot)

# Close the graphics device
dev.off()

library(dplyr)
library(ggplot2)
library(scales)

physeq_melted_split_king<- physeq_melted_split_king %>%
  mutate(Treatment = recode(Treatment,
                            "Non-straw" = "Control",
                            "Straw" = "Mulch"))




# Step 1: Summarize total abundance by Genus, Treatment, and Sample_type
Genus_summary_king <- physeq_melted_split_king %>%
  group_by(Genus, Treatment, Sample_type) %>%
  summarise(total_abundance = sum(Abundance), .groups = 'drop')  # Summarizing Abundance
# Step 1: Filter out empty genera
Genus_summary_filtered_king <- Genus_summary_king %>% 
  filter(Genus != "")

# Step 2: Summarize total abundance for each genus and get the top 21 genera
top_Genus_king <- Genus_summary_filtered_king %>%
  group_by(Genus) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop') %>%
  arrange(desc(total_abundance)) %>%
  slice_head(n = 11) %>%  # Select 21 to account for potential exclusion of the empty genus
  pull(Genus)

# Step 3: If there are more than 20 genera, trim to the top 20
if (length(top_Genus_king) > 10) {
  top_Genus_king <- top_Genus_king[1:10]
}

# Step 4: Filter the dataset to include only the top 20 genera
Genus_summary_top10_king <- Genus_summary_filtered_king %>%
  filter(Genus %in% top_Genus_king)

# Step 4: Print unique genera
unique_Genus_king <- unique(Genus_summary_top10_king$Genus)
print(unique_Genus_king)

# Step 5: Define color codes for the genera
# Define the color codes for each genus in unique_Genus_king
color_codes <- c(
  "Acidibacter"                                        = "#FF5733",  # Red-Orange
  "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium" = "#228B22",  # Forest Green
  "Arthrobacter"                                       = "#8A2BE2",  # Blue Violet
  "Bryobacter"                                         = "#DAA520",  # Goldenrod
  "Burkholderia-Caballeronia-Paraburkholderia"         = "#FF4500",  # Orange Red
  "Candidatus Solibacter"                              = "#FF8C00",  # Dark Orange
  "Clostridium sensu stricto 13"                       = "#7C4848",  # Crimson
  "Duganella"                                          = "#40E0D0",  # Turquoise
  "Flavobacterium"                                     = "#44D7A8",  # Medium Sea Green
  "Gemmatimonas"                                       = "#4682B4",  # Steel Blue
  "Granulicella"                                       = "#20B2AA",  # Slate Blue
  "Mucilaginibacter"                                   = "#6A5ACD",  # Light Sea Green
  "Pedobacter"                                         = "#FF6347",  # Tomato
  "Pseudolabrys"                                       = "#E4717A",  # Deep Pink
  "Pseudomonas"                                        = "#FFDAB9",  # Peach Puff
  "Rahnella1"                                          = "#9ACD32",  # Yellow Green
  "Rhodanobacter"                                      = "#32CD32",  # Lime Green
  "Sphingomonas"                                       = "#00BFFF",  # Deep Sky Blue
  "Streptomyces"                                       = "#C71585",  # Medium Violet Red
  "TM7a"                                               = "#FFA07A"   # Light Salmon
)



color_codes <- c(
  "Acidibacter"                                        = "#FF5733",  # Red-Orange
  "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium" = "#228B22",  # Forest Green
  "Arthrobacter"                                       = "#8A2BE2",  # Blue Violet
  "Bryobacter"                                         = "#DAA520",  # Goldenrod
  "Candidatus Solibacter"                              = "#FF8C00",  # Dark Orange
  "Chryseobacterium"                                   = "#FF6347",  # Tomato
  "Clostridium sensu stricto 13"                       = "#7c4848",  # Crimson
  "Flavobacterium"                                     = "#44d7a8",  # Medium Sea Green
  "Gemmatimonas"                                       = "#4682B4",  # Steel Blue
  "Granulicella"                                       = "#6A5ACD",  # Slate Blue
  "Massilia"                                           = "#FFD700",  # Gold
  "Mucilaginibacter"                                   = "#20B2AA",  # Light Sea Green
  "Pedobacter"                                         = "#FF4500",  # Orange Red
  "Pseudolabrys"                                       = "#e4717a",  # Deep Pink
  "Pseudomonas"                                        = "#FFDAB9",  # Peach Puff
  "Rhodanobacter"                                      = "#32CD32",  # Lime Green
  "Sphingobacterium"                                   = "#8B008B",  # Dark Magenta
  "Sphingomonas"                                       = "#00BFFF",  # Deep Sky Blue
  "Streptomyces"                                       = "#C71585",  # Medium Violet Red
  "TM7a"                                               = "#FFA07A"   # Light Salmon
)
# Print the assigned color codes for reference
print(color_codes)

# Step 5: Summarize total abundance for the top 20 genera
Genus_summary_abundance_king <- Genus_summary_top10_king %>%
  group_by(Genus) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop')
print(Genus_summary_abundance_king)

# Step 6: Reorder the genera based on total abundance (from least to most)
Genus_summary_top10_king <- Genus_summary_top10_king %>%
  mutate(Genus = factor(Genus, 
                        levels = Genus_summary_abundance_king$Genus[order(Genus_summary_abundance_king$total_abundance)]))

# Step 7: Calculate the relative abundance for each genus within each treatment
Genus_summary_top10_king <- Genus_summary_top10_king %>%
  group_by(Treatment) %>%
  mutate(relative_abundance = total_abundance / sum(total_abundance)) %>%
  ungroup()  # Ungroup after calculations if needed

# Step 7: Create the plot using relative abundance
plot <- ggplot(Genus_summary_top10_king, aes(x = Treatment, y = relative_abundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  scale_fill_manual(values = color_codes) +  # Use the genus color palette
  labs(
    title = "Top 10 Genera by Abundance in King Cultivar",
    x = "Treatment",
    y = " Relative Abundance (%)",
    fill = "Genus"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~ Sample_type, nrow = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines")
  )

# Step 8: Print the plot
print(plot)
CairoPNG(filename = "top_10_genera_king.png", width = 800, height = 600, units = "px", res = 100)

# Print the plot to the graphics device
print(plot)

# Close the graphics device
dev.off()







#For individual samples
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)

# Assuming Genus_summary_mandel is already defined and available
# Filter for the top 10 genera in "Straw" treatment
top10_genus_soil_straw <- Genus_summary_mandel %>%
  filter(Sample_type == "Soil", Treatment == "Straw", Genus != "") %>%
  arrange(desc(total_abundance)) %>%
  slice_head(n = 10)  # Select top 10 genera

# Print the top 10 genera
print(top10_genus_soil_straw)

# Create a named vector of color codes for the top 10 genera in the "Straw" treatment
genus_colors_straw <- c(
  "Gemmatimonas" = "#3357FF",           # Color for Gemmatimonas
  "Bryobacter" = "#7c4848",             # Color for Bryobacter
  "Sphingomonas" = "#228B22",           # Color for Sphingomonas
  "Candidatus Solibacter" = "#33FFF6",  # Color for Candidatus Solibacter
  "Rhodanobacter" = "#c71585",          # Color for Rhodanobacter
  "Pseudolabrys" = "#20B2AA",           # Color for Pseudolabrys
  "Acidibacter" = "#C80815",            # Color for Acidibacter
  "Oryzihumus" = "#DAA520",              # Color for Oryzihumus
  "Mucilaginibacter" = "#6A5ACD",       # Color for Mucilaginibacter
  "Arthrobacter" = "#8B008B"            # Color for Arthrobacter
)

# Summarise total abundances for each genus
Genus_summary_abundancesoil_straw <- top10_genus_soil_straw %>%
  group_by(Genus) %>%
  summarise(total_abundance = sum(total_abundance), .groups = 'drop')

# Reorder Genus factor levels based on total abundance for straw
top10_genus_soil_straw <- top10_genus_soil_straw %>%
  mutate(Genus = factor(Genus, 
                        levels = Genus_summary_abundancesoil_straw$Genus[order(Genus_summary_abundancesoil_straw$total_abundance)]))

# Calculate relative abundance for straw treatment
top10_genus_soil_straw <- top10_genus_soil_straw %>%
  group_by(Treatment) %>%
  mutate(relative_abundance = total_abundance / sum(total_abundance)) %>%
  ungroup()  # Ungroup after calculations if needed

# Print the updated dataframe with relative abundance
print(top10_genus_soil_straw)

# Create a stacked bar plot for the straw treatment
plot_soil_straw <- ggplot(top10_genus_soil_straw, aes(x = Treatment, y = relative_abundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "fill", width = 0.2) +  # Adjust bar width
  scale_fill_manual(values = genus_colors_straw) +  # Use the genus color palette for straw treatment
  labs(
    title = "Top 10 Genera Soil Straw in Mandel Cultivar",
    x = "Treatment",
    y = "Relative Abundance (%)",
    fill = "Genus"
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  facet_wrap(~ Sample_type, nrow = 1, scales = "free_x") +  # Free x-axis, to avoid empty space
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank(),  # Remove extra axis lines
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Border around the panel
    panel.grid = element_blank(),  # Remove grid lines
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines")  # Remove panel spacing
  )

# Print the plot
print(plot_soil_straw)

# Save the plot as a PNG file with a white background
ggsave("plot_soil_straw.png", plot = plot_soil_straw, width = 8, height = 6, dpi = 300, bg = "white")


Genus_summary_king <- physeq_melted_split_king %>%
  group_by(Genus, Treatment, Sample_type) %>%
  summarise(total_abundance = sum(Abundance), .groups = 'drop')








#For Mandel

# Load required libraries
library(ggplot2)
library(dplyr)

# Define color codes for each genus
color_codes_mandel <- c(
  "Rhodanobacter" = "#c71585",
  "Sphingomonas" = "#228B22",
  "Rahnella1" = "#fb607f",
  "Pseudomonas" = "#FFA07A",
  "Gemmatimonas" = "#3357FF",
  "Bryobacter" = "#7c4848",
  "Candidatus Solibacter" = "#33FFF6",
  "Mucilaginibacter" = "#6A5ACD",
  "Flavobacterium" = "#867e36",
  "Candidatus Udaeobacter" = "#e3f988",
  "Pedobacter" = "#e3ab57",
  "Granulicella" = "#ffb6c1",
  "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium" = "#354230",
  "Streptomyces" = "#778899",
  "Arthrobacter" = "#8B008B",
  "Duganella" = "#ffbcd9",
  "Massilia" = "#c9ffe5",
  "Pseudolabrys" = "#20B2AA",
  "Acidibacter" = "#C80815",
  "Oryzihumus" = "#DAA520",
  "Clostridium sensu stricto 13" = "#FF6347",
  "Dokdonella" = "#32174d",
  "Chishuiella" = "#FFD133",
  "TM7a" = "#bcb88a",
  "Chryseobacterium" = "#9dc209",
  "Microbacterium" = "#997a8d",
  "Asticcacaulis" = "#80DAEB",
  "Sphingobacterium" = "#FF5733"
)

# Plot for Mandel data with legend in a single vertical line on the right side
plot_mandel_single_column_legend <- ggplot(combined_data_mandel, aes(x = Treatment, y = relative_abundance, fill = Genus)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  scale_fill_manual(values = color_codes_mandel, guide = guide_legend(ncol = 1)) +  # Single column legend
  labs(x = "", y = "Relative Abundance (%)") +  # Remove title and X axis label
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~ Sample_type, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),  # Set plot background to white
    plot.background = element_rect(fill = "white"),   # Set overall background to white
    axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "right",  # Position legend on the right side
    legend.text = element_text(size = 8)  # Adjust text size if needed
  )

# Print the plot
print(plot_mandel_single_column_legend)

# Save the plot as a PNG file
ggsave("mandel_plot.png", plot = plot_mandel_single_column_legend, width = 12, height = 8)







For King Edward

# Load required libraries
library(ggplot2)
library(dplyr)

# Define color codes for each genus
color_codes_mandel <- c(
  "Rhodanobacter" = "#c71585",
  "Sphingomonas" = "#228B22",
  "Rahnella1" = "#fb607f",
  "Pseudomonas" = "#FFA07A",
  "Gemmatimonas" = "#3357FF",
  "Bryobacter" = "#7c4848",
  "Candidatus Solibacter" = "#33FFF6",
  "Mucilaginibacter" = "#6A5ACD",
  "Flavobacterium" = "#867e36",
  "Candidatus Udaeobacter" = "#e3f988",
  "Pedobacter" = "#e3ab57",
  "Granulicella" = "#ffb6c1",
  "Allorhizobium-Neorhizobium-Pararhizobium-Rhizobium" = "#354230",
  "Streptomyces" = "#778899",
  "Arthrobacter" = "#8B008B",
  "Duganella" = "#ffbcd9",
  "Massilia" = "#c9ffe5",
  "Pseudolabrys" = "#20B2AA",
  "Acidibacter" = "#C80815",
  "Oryzihumus" = "#FF6347",
  "Clostridium sensu stricto 13" = "#FF6347",
  "Dokdonella" = "#32174d",
  "Chishuiella" = "#FFD133",
  "TM7a" = "#bcb88a",
  "Chryseobacterium" = "#9dc209",
  "Microbacterium" = "#997a8d",
  "Asticcacaulis" = "#80DAEB",
  "Sphingobacterium" = "#FF5733"
)

# Plot for King Edward data with legend included
plotking_with_legend <- ggplot(combined_dataking, aes(x = Treatment, y = relative_abundance, fill = Genus)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.8) +
  scale_fill_manual(values = color_codes_mandel, guide = guide_legend(ncol = 1)) +  # Single column legend
  labs(title = "", x = "", y = "Relative Abundance (%)") +  # Remove plot title and X axis label
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~ Sample_type, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),  # Set plot background to white
    plot.background = element_rect(fill = "white"),   # Set overall background to white
    axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "right",  # Position legend on the right side
    legend.text = element_text(size = 8)  # Adjust legend text size if needed
  )

# Print the King Edward plot with legend
print(plotking_with_legend)

# Save the plot as a PNG file (optional)
ggsave("king_edward_plot.png", plot = plotking_with_legend, width = 12, height = 8)

#For King Edward
# Load necessary library
library(dplyr)

# Filter the data for Rhodanobacter in Straw treatment and Rhizosphere sample type
rhodanobacter_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Rhodanobacter" & Treatment == "Straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Rhodanobacter in Straw and Rhizosphere
total_rhodanobacter_straw_rhizosphere_abundance_king <- rhodanobacter_straw_rhizosphere_data_king %>%
  summarise(total_rhodanobacter_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Rhizosphere sample type
total_abundance_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Rhodanobacter in Straw and Rhizosphere
rhodanobacter_straw_rhizosphere_percentage_king <- total_rhodanobacter_straw_rhizosphere_abundance_king$total_rhodanobacter_straw_rhizosphere / total_abundance_straw_rhizosphere_king$total_all_genera_straw_rhizosphere * 100

# Print the result
print(paste("Rhodanobacter Relative Abundance Percentage in Straw Rhizosphere: ", round(rhodanobacter_straw_rhizosphere_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Rhodanobacter in Non-straw treatment and Rhizosphere sample type
rhodanobacter_non_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Rhodanobacter" & Treatment == "Non-straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Rhodanobacter in Non-straw and Rhizosphere
total_rhodanobacter_non_straw_rhizosphere_abundance_king <- rhodanobacter_non_straw_rhizosphere_data_king %>%
  summarise(total_rhodanobacter_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Rhizosphere sample type
total_abundance_non_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Rhodanobacter in Non-straw and Rhizosphere
rhodanobacter_non_straw_rhizosphere_percentage_king <- total_rhodanobacter_non_straw_rhizosphere_abundance_king$total_rhodanobacter_non_straw_rhizosphere / total_abundance_non_straw_rhizosphere_king$total_all_genera_non_straw_rhizosphere * 100

# Print the result
print(paste("Rhodanobacter Relative Abundance Percentage in Non-straw Rhizosphere: ", round(rhodanobacter_non_straw_rhizosphere_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Mucilaginibacter in Straw treatment and Rhizosphere sample type
mucilaginibacter_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Mucilaginibacter" & Treatment == "Straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Mucilaginibacter in Straw and Rhizosphere
total_mucilaginibacter_straw_rhizosphere_abundance_king <- mucilaginibacter_straw_rhizosphere_data_king %>%
  summarise(total_mucilaginibacter_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Rhizosphere sample type
total_abundance_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in Straw and Rhizosphere
mucilaginibacter_straw_rhizosphere_percentage_king <- total_mucilaginibacter_straw_rhizosphere_abundance_king$total_mucilaginibacter_straw_rhizosphere / total_abundance_straw_rhizosphere_king$total_all_genera_straw_rhizosphere * 100

# Print the result
print(paste("Mucilaginibacter Relative Abundance Percentage in Straw Rhizosphere: ", round(mucilaginibacter_straw_rhizosphere_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Mucilaginibacter in Non-straw treatment and Rhizosphere sample type
mucilaginibacter_non_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Mucilaginibacter" & Treatment == "Non-straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Mucilaginibacter in Non-straw and Rhizosphere
total_mucilaginibacter_non_straw_rhizosphere_abundance_king <- mucilaginibacter_non_straw_rhizosphere_data_king %>%
  summarise(total_mucilaginibacter_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Rhizosphere sample type
total_abundance_non_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in Non-straw and Rhizosphere
mucilaginibacter_non_straw_rhizosphere_percentage_king <- total_mucilaginibacter_non_straw_rhizosphere_abundance_king$total_mucilaginibacter_non_straw_rhizosphere / total_abundance_non_straw_rhizosphere_king$total_all_genera_non_straw_rhizosphere * 100

# Print the result
print(paste("Mucilaginibacter Relative Abundance Percentage in Non-straw Rhizosphere: ", round(mucilaginibacter_non_straw_rhizosphere_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Flavobacterium in Straw treatment and Rhizosphere sample type
flavobacterium_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Flavobacterium" & Treatment == "Straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Flavobacterium in Straw and Rhizosphere
total_flavobacterium_straw_rhizosphere_abundance_king <- flavobacterium_straw_rhizosphere_data_king %>%
  summarise(total_flavobacterium_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Rhizosphere sample type
total_abundance_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in Straw and Rhizosphere
flavobacterium_straw_rhizosphere_percentage_king <- total_flavobacterium_straw_rhizosphere_abundance_king$total_flavobacterium_straw_rhizosphere / total_abundance_straw_rhizosphere_king$total_all_genera_straw_rhizosphere * 100

# Print the result
print(paste("Flavobacterium Relative Abundance Percentage in Straw Rhizosphere: ", round(flavobacterium_straw_rhizosphere_percentage_king, 2), "%"))



library(dplyr)

# Filter the data for Flavobacterium in Non-straw treatment and Rhizosphere sample type
flavobacterium_non_straw_rhizosphere_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Flavobacterium" & Treatment == "Non-straw" & Sample_type == "Rhizosphere")

# Calculate the total relative abundance for Flavobacterium in Non-straw and Rhizosphere
total_flavobacterium_non_straw_rhizosphere_abundance_king <- flavobacterium_non_straw_rhizosphere_data_king %>%
  summarise(total_flavobacterium_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Rhizosphere sample type
total_abundance_non_straw_rhizosphere_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Rhizosphere") %>%
  summarise(total_all_genera_non_straw_rhizosphere = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in Non-straw and Rhizosphere
flavobacterium_non_straw_rhizosphere_percentage_king <- total_flavobacterium_non_straw_rhizosphere_abundance_king$total_flavobacterium_non_straw_rhizosphere / total_abundance_non_straw_rhizosphere_king$total_all_genera_non_straw_rhizosphere * 100

# Print the result
print(paste("Flavobacterium Relative Abundance Percentage in Non-straw Rhizosphere: ", round(flavobacterium_non_straw_rhizosphere_percentage_king, 2), "%"))



library(dplyr)

# Filter the data for Flavobacterium in Non-straw treatment and Root sample type
flavobacterium_non_straw_root_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Flavobacterium" & Treatment == "Non-straw" & Sample_type == "Root")

# Calculate the total relative abundance for Flavobacterium in Non-straw and Root
total_flavobacterium_non_straw_root_abundance_king <- flavobacterium_non_straw_root_data_king %>%
  summarise(total_flavobacterium_non_straw_root = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Root sample type
total_abundance_non_straw_root_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Root") %>%
  summarise(total_all_genera_non_straw_root = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in Non-straw and Root
flavobacterium_non_straw_root_percentage_king <- total_flavobacterium_non_straw_root_abundance_king$total_flavobacterium_non_straw_root / total_abundance_non_straw_root_king$total_all_genera_non_straw_root * 100

# Print the result
print(paste("Flavobacterium Relative Abundance Percentage in Non-straw Root: ", round(flavobacterium_non_straw_root_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Flavobacterium in Straw treatment and Tuber Peel sample type
flavobacterium_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Flavobacterium" & Treatment == "Straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Flavobacterium in Straw and Tuber Peel
total_flavobacterium_straw_tuber_peel_abundance_king <- flavobacterium_straw_tuber_peel_data_king %>%
  summarise(total_flavobacterium_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Tuber Peel sample type
total_abundance_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in Straw and Tuber Peel
flavobacterium_straw_tuber_peel_percentage_king <- total_flavobacterium_straw_tuber_peel_abundance_king$total_flavobacterium_straw_tuber_peel / total_abundance_straw_tuber_peel_king$total_all_genera_straw_tuber_peel * 100

# Print the result
print(paste("Flavobacterium Relative Abundance Percentage in Straw Tuber Peel: ", round(flavobacterium_straw_tuber_peel_percentage_king, 2), "%"))
library(dplyr)

# Filter the data for Flavobacterium in Non-straw treatment and Tuber Peel sample type
flavobacterium_non_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Flavobacterium" & Treatment == "Non-straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Flavobacterium in Non-straw and Tuber Peel
total_flavobacterium_non_straw_tuber_peel_abundance_king <- flavobacterium_non_straw_tuber_peel_data_king %>%
  summarise(total_flavobacterium_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Tuber Peel sample type
total_abundance_non_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in Non-straw and Tuber Peel
flavobacterium_non_straw_tuber_peel_percentage_king <- total_flavobacterium_non_straw_tuber_peel_abundance_king$total_flavobacterium_non_straw_tuber_peel / total_abundance_non_straw_tuber_peel_king$total_all_genera_non_straw_tuber_peel * 100

# Print the result
print(paste("Flavobacterium Relative Abundance Percentage in Non-straw Tuber Peel: ", round(flavobacterium_non_straw_tuber_peel_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Pedobacter in Straw treatment and Tuber Peel sample type
pedobacter_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pedobacter" & Treatment == "Straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Pedobacter in Straw and Tuber Peel
total_pedobacter_straw_tuber_peel_abundance_king <- pedobacter_straw_tuber_peel_data_king %>%
  summarise(total_pedobacter_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Tuber Peel sample type
total_abundance_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in Straw and Tuber Peel
pedobacter_straw_tuber_peel_percentage_king <- total_pedobacter_straw_tuber_peel_abundance_king$total_pedobacter_straw_tuber_peel / total_abundance_straw_tuber_peel_king$total_all_genera_straw_tuber_peel * 100

# Print the result
print(paste("Pedobacter Relative Abundance Percentage in Straw Tuber Peel: ", round(pedobacter_straw_tuber_peel_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Pedobacter in Non-straw treatment and Tuber Peel sample type
pedobacter_non_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pedobacter" & Treatment == "Non-straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Pedobacter in Non-straw and Tuber Peel
total_pedobacter_non_straw_tuber_peel_abundance_king <- pedobacter_non_straw_tuber_peel_data_king %>%
  summarise(total_pedobacter_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Tuber Peel sample type
total_abundance_non_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in Non-straw and Tuber Peel
pedobacter_non_straw_tuber_peel_percentage_king <- total_pedobacter_non_straw_tuber_peel_abundance_king$total_pedobacter_non_straw_tuber_peel / total_abundance_non_straw_tuber_peel_king$total_all_genera_non_straw_tuber_peel * 100

# Print the result
print(paste("Pedobacter Relative Abundance Percentage in Non-straw Tuber Peel: ", round(pedobacter_non_straw_tuber_peel_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Pseudomonas in Straw treatment and Tuber Peel sample type
pseudomonas_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pseudomonas" & Treatment == "Straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Pseudomonas in Straw and Tuber Peel
total_pseudomonas_straw_tuber_peel_abundance_king <- pseudomonas_straw_tuber_peel_data_king %>%
  summarise(total_pseudomonas_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Tuber Peel sample type
total_abundance_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Pseudomonas in Straw and Tuber Peel
pseudomonas_straw_tuber_peel_percentage_king <- total_pseudomonas_straw_tuber_peel_abundance_king$total_pseudomonas_straw_tuber_peel / total_abundance_straw_tuber_peel_king$total_all_genera_straw_tuber_peel * 100

# Print the result
print(paste("Pseudomonas Relative Abundance Percentage in Straw Tuber Peel: ", round(pseudomonas_straw_tuber_peel_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Pseudomonas in Non-straw treatment and Tuber Peel sample type
pseudomonas_non_straw_tuber_peel_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pseudomonas" & Treatment == "Non-straw" & Sample_type == "Tuber Peel")

# Calculate the total relative abundance for Pseudomonas in Non-straw and Tuber Peel
total_pseudomonas_non_straw_tuber_peel_abundance_king <- pseudomonas_non_straw_tuber_peel_data_king %>%
  summarise(total_pseudomonas_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Tuber Peel sample type
total_abundance_non_straw_tuber_peel_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Tuber Peel") %>%
  summarise(total_all_genera_non_straw_tuber_peel = sum(relative_abundance))

# Calculate the percentage relative abundance of Pseudomonas in Non-straw and Tuber Peel
pseudomonas_non_straw_tuber_peel_percentage_king <- total_pseudomonas_non_straw_tuber_peel_abundance_king$total_pseudomonas_non_straw_tuber_peel / total_abundance_non_straw_tuber_peel_king$total_all_genera_non_straw_tuber_peel * 100

# Print the result
print(paste("Pseudomonas Relative Abundance Percentage in Non-straw Tuber Peel: ", round(pseudomonas_non_straw_tuber_peel_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Pedobacter in Straw treatment and Root sample type
pedobacter_straw_root_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pedobacter" & Treatment == "Straw" & Sample_type == "Root")

# Calculate the total relative abundance for Pedobacter in Straw and Root
total_pedobacter_straw_root_abundance_king <- pedobacter_straw_root_data_king %>%
  summarise(total_pedobacter_straw_root = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Root sample type
total_abundance_straw_root_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Root") %>%
  summarise(total_all_genera_straw_root = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in Straw and Root
pedobacter_straw_root_percentage_king <- total_pedobacter_straw_root_abundance_king$total_pedobacter_straw_root / total_abundance_straw_root_king$total_all_genera_straw_root * 100

# Print the result
print(paste("Pedobacter Relative Abundance Percentage in Straw Root: ", round(pedobacter_straw_root_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Pedobacter in Non-straw treatment and Root sample type
pedobacter_non_straw_root_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Pedobacter" & Treatment == "Non-straw" & Sample_type == "Root")

# Calculate the total relative abundance for Pedobacter in Non-straw and Root
total_pedobacter_non_straw_root_abundance_king <- pedobacter_non_straw_root_data_king %>%
  summarise(total_pedobacter_non_straw_root = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Root sample type
total_abundance_non_straw_root_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Root") %>%
  summarise(total_all_genera_non_straw_root = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in Non-straw and Root
pedobacter_non_straw_root_percentage_king <- total_pedobacter_non_straw_root_abundance_king$total_pedobacter_non_straw_root / total_abundance_non_straw_root_king$total_all_genera_non_straw_root * 100

# Print the result
print(paste("Pedobacter Relative Abundance Percentage in Non-straw Root: ", round(pedobacter_non_straw_root_percentage_king, 2), "%"))

library(dplyr)

# Filter the data for Bryobacter in Straw treatment and Soil sample type
bryobacter_straw_soil_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Bryobacter" & Treatment == "Straw" & Sample_type == "Soil")

# Calculate the total relative abundance for Bryobacter in Straw and Soil
total_bryobacter_straw_soil_abundance_king <- bryobacter_straw_soil_data_king %>%
  summarise(total_bryobacter_straw_soil = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Soil sample type
total_abundance_straw_soil_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Soil") %>%
  summarise(total_all_genera_straw_soil = sum(relative_abundance))

# Calculate the percentage relative abundance of Bryobacter in Straw and Soil
bryobacter_straw_soil_percentage_king <- total_bryobacter_straw_soil_abundance_king$total_bryobacter_straw_soil / total_abundance_straw_soil_king$total_all_genera_straw_soil * 100

# Print the result
print(paste("Bryobacter Relative Abundance Percentage in Straw Soil: ", round(bryobacter_straw_soil_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Bryobacter in Non-straw treatment and Soil sample type
bryobacter_non_straw_soil_data_king <- Genus_summary_top10_king %>%
  filter(Genus == "Bryobacter" & Treatment == "Non-straw" & Sample_type == "Soil")

# Calculate the total relative abundance for Bryobacter in Non-straw and Soil
total_bryobacter_non_straw_soil_abundance_king <- bryobacter_non_straw_soil_data_king %>%
  summarise(total_bryobacter_non_straw_soil = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Soil sample type
total_abundance_non_straw_soil_king <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Soil") %>%
  summarise(total_all_genera_non_straw_soil = sum(relative_abundance))

# Calculate the percentage relative abundance of Bryobacter in Non-straw and Soil
bryobacter_non_straw_soil_percentage_king <- total_bryobacter_non_straw_soil_abundance_king$total_bryobacter_non_straw_soil / total_abundance_non_straw_soil_king$total_all_genera_non_straw_soil * 100

# Print the result
print(paste("Bryobacter Relative Abundance Percentage in Non-straw Soil: ", round(bryobacter_non_straw_soil_percentage_king, 2), "%"))


library(dplyr)

# Filter the data for Candidatus Solibacter in Straw treatment and Soil sample type
candidatus_solibacter_straw_soil_data <- Genus_summary_top10_king %>%
  filter(Genus == "Candidatus Solibacter" & Treatment == "Straw" & Sample_type == "Soil")

# Calculate the total relative abundance for Candidatus Solibacter in Straw and Soil
total_candidatus_solibacter_straw_soil_abundance <- candidatus_solibacter_straw_soil_data %>%
  summarise(total_candidatus_solibacter_straw_soil = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Straw treatment and Soil sample type
total_abundance_straw_soil <- Genus_summary_top10_king %>%
  filter(Treatment == "Straw" & Sample_type == "Soil") %>%
  summarise(total_all_genera_straw_soil = sum(relative_abundance))

# Calculate the percentage relative abundance of Candidatus Solibacter in Straw and Soil
candidatus_solibacter_straw_soil_percentage <- total_candidatus_solibacter_straw_soil_abundance$total_candidatus_solibacter_straw_soil / total_abundance_straw_soil$total_all_genera_straw_soil * 100

# Print the result
print(paste("Candidatus Solibacter Relative Abundance Percentage in Straw Soil: ", round(candidatus_solibacter_straw_soil_percentage, 2), "%"))
library(dplyr)

# Filter the data for Candidatus Solibacter in Non-straw treatment and Soil sample type
candidatus_solibacter_non_straw_soil_data <- Genus_summary_top10_king %>%
  filter(Genus == "Candidatus Solibacter" & Treatment == "Non-straw" & Sample_type == "Soil")

# Calculate the total relative abundance for Candidatus Solibacter in Non-straw and Soil
total_candidatus_solibacter_non_straw_soil_abundance <- candidatus_solibacter_non_straw_soil_data %>%
  summarise(total_candidatus_solibacter_non_straw_soil = sum(relative_abundance))

# Calculate the total relative abundance for all genera in Non-straw treatment and Soil sample type
total_abundance_non_straw_soil <- Genus_summary_top10_king %>%
  filter(Treatment == "Non-straw" & Sample_type == "Soil") %>%
  summarise(total_all_genera_non_straw_soil = sum(relative_abundance))

# Calculate the percentage relative abundance of Candidatus Solibacter in Non-straw and Soil
candidatus_solibacter_non_straw_soil_percentage <- total_candidatus_solibacter_non_straw_soil_abundance$total_candidatus_solibacter_non_straw_soil / total_abundance_non_straw_soil$total_all_genera_non_straw_soil * 100

# Print the result
print(paste("Candidatus Solibacter Relative Abundance Percentage in Non-straw Soil: ", round(candidatus_solibacter_non_straw_soil_percentage, 2), "%"))

# For Mandel

library(dplyr)

# Filter the data for Rhodanobacter in the Rhizosphere sample type and Straw treatment
rhodanobacter_rhizosphere_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Rhodanobacter" & Sample_type == "Rhizosphere" & Treatment == "Straw")

# Calculate the total relative abundance for Rhodanobacter in the Rhizosphere for Straw treatment
total_rhodanobacter_rhizosphere_straw_abundance_mandel <- rhodanobacter_rhizosphere_straw_data_mandel %>%
  summarise(total_rhodanobacter_rhizosphere_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Rhizosphere for Straw treatment
total_abundance_rhizosphere_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Rhizosphere" & Treatment == "Straw") %>%
  summarise(total_all_genera_rhizosphere_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Rhodanobacter in the Rhizosphere for Straw treatment
rhodanobacter_rhizosphere_straw_percentage_mandel <- total_rhodanobacter_rhizosphere_straw_abundance_mandel$total_rhodanobacter_rhizosphere_straw_abundance_mandel / total_abundance_rhizosphere_straw_mandel$total_all_genera_rhizosphere_straw * 100

# Print the result for Straw treatment
print(paste("Rhodanobacter Relative Abundance Percentage in Straw Rhizosphere: ", round(rhodanobacter_rhizosphere_straw_percentage_mandel, 2), "%"))

# Filter the data for Rhodanobacter in the Rhizosphere sample type and Non-straw treatment
rhodanobacter_rhizosphere_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Rhodanobacter" & Sample_type == "Rhizosphere" & Treatment == "Non-straw")

# Calculate the total relative abundance for Rhodanobacter in the Rhizosphere for Non-straw treatment
total_rhodanobacter_rhizosphere_non_straw_abundance_mandel <- rhodanobacter_rhizosphere_non_straw_data_mandel %>%
  summarise(total_rhodanobacter_rhizosphere_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Rhizosphere for Non-straw treatment
total_abundance_rhizosphere_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Rhizosphere" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_rhizosphere_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Rhodanobacter in the Rhizosphere for Non-straw treatment
rhodanobacter_rhizosphere_non_straw_percentage_mandel <- total_rhodanobacter_rhizosphere_non_straw_abundance_mandel$total_rhodanobacter_rhizosphere_non_straw_abundance_mandel / total_abundance_rhizosphere_non_straw_mandel$total_all_genera_rhizosphere_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Rhodanobacter Relative Abundance Percentage in Non-straw Rhizosphere: ", round(rhodanobacter_rhizosphere_non_straw_percentage_mandel, 2), "%"))

library(dplyr)

# Filter the data for Flavobacterium in the Root sample type and Straw treatment
flavobacterium_root_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Flavobacterium" & Sample_type == "Root" & Treatment == "Straw")

# Calculate the total relative abundance for Flavobacterium in the Root for Straw treatment
total_flavobacterium_root_straw_abundance_mandel <- flavobacterium_root_straw_data_mandel %>%
  summarise(total_flavobacterium_root_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Root sample type for Straw treatment
total_abundance_root_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Root" & Treatment == "Straw") %>%
  summarise(total_all_genera_root_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in the Root for Straw treatment
flavobacterium_root_straw_percentage_mandel <- total_flavobacterium_root_straw_abundance_mandel$total_flavobacterium_root_straw_abundance_mandel / total_abundance_root_straw_mandel$total_all_genera_root_straw * 100

# Print the result for Straw treatment
print(paste("Flavobacterium Relative Abundance Percentage in Root Straw: ", round(flavobacterium_root_straw_percentage_mandel, 2), "%"))

# Filter the data for Flavobacterium in the Root sample type and Non-straw treatment
flavobacterium_root_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Flavobacterium" & Sample_type == "Root" & Treatment == "Non-straw")

# Calculate the total relative abundance for Flavobacterium in the Root for Non-straw treatment
total_flavobacterium_root_non_straw_abundance_mandel <- flavobacterium_root_non_straw_data_mandel %>%
  summarise(total_flavobacterium_root_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Root sample type for Non-straw treatment
total_abundance_root_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Root" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_root_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Flavobacterium in the Root for Non-straw treatment
flavobacterium_root_non_straw_percentage_mandel <- total_flavobacterium_root_non_straw_abundance_mandel$total_flavobacterium_root_non_straw_abundance_mandel / total_abundance_root_non_straw_mandel$total_all_genera_root_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Flavobacterium Relative Abundance Percentage in Root Non-straw: ", round(flavobacterium_root_non_straw_percentage_mandel, 2), "%"))

library(dplyr)

# Filter the data for Mucilaginibacter in the Root sample type and Straw treatment
mucilaginibacter_root_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Mucilaginibacter" & Sample_type == "Root" & Treatment == "Straw")

# Calculate the total relative abundance for Mucilaginibacter in the Root for Straw treatment
total_mucilaginibacter_root_straw_abundance_mandel <- mucilaginibacter_root_straw_data_mandel %>%
  summarise(total_mucilaginibacter_root_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Root sample type for Straw treatment
total_abundance_root_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Root" & Treatment == "Straw") %>%
  summarise(total_all_genera_root_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in the Root for Straw treatment
mucilaginibacter_root_straw_percentage_mandel <- total_mucilaginibacter_root_straw_abundance_mandel$total_mucilaginibacter_root_straw_abundance_mandel / total_abundance_root_straw_mandel$total_all_genera_root_straw * 100

# Print the result for Straw treatment
print(paste("Mucilaginibacter Relative Abundance Percentage in Root Straw: ", round(mucilaginibacter_root_straw_percentage_mandel, 2), "%"))

# Filter the data for Mucilaginibacter in the Root sample type and Non-straw treatment
mucilaginibacter_root_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Mucilaginibacter" & Sample_type == "Root" & Treatment == "Non-straw")

# Calculate the total relative abundance for Mucilaginibacter in the Root for Non-straw treatment
total_mucilaginibacter_root_non_straw_abundance_mandel <- mucilaginibacter_root_non_straw_data_mandel %>%
  summarise(total_mucilaginibacter_root_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Root sample type for Non-straw treatment
total_abundance_root_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Root" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_root_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in the Root for Non-straw treatment
mucilaginibacter_root_non_straw_percentage_mandel <- total_mucilaginibacter_root_non_straw_abundance_mandel$total_mucilaginibacter_root_non_straw_abundance_mandel / total_abundance_root_non_straw_mandel$total_all_genera_root_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Mucilaginibacter Relative Abundance Percentage in Root Non-straw: ", round(mucilaginibacter_root_non_straw_percentage_mandel, 2), "%"))


library(dplyr)

# Filter the data for Bryobacter in the Soil sample type and Straw treatment
bryobacter_soil_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Bryobacter" & Sample_type == "Soil" & Treatment == "Straw")

# Calculate the total relative abundance for Bryobacter in the Soil for Straw treatment
total_bryobacter_soil_straw_abundance_mandel <- bryobacter_soil_straw_data_mandel %>%
  summarise(total_bryobacter_soil_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Soil sample type for Straw treatment
total_abundance_soil_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Soil" & Treatment == "Straw") %>%
  summarise(total_all_genera_soil_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Bryobacter in the Soil for Straw treatment
bryobacter_soil_straw_percentage_mandel <- total_bryobacter_soil_straw_abundance_mandel$total_bryobacter_soil_straw_abundance_mandel / total_abundance_soil_straw_mandel$total_all_genera_soil_straw * 100

# Print the result for Straw treatment
print(paste("Bryobacter Relative Abundance Percentage in Soil Straw: ", round(bryobacter_soil_straw_percentage_mandel, 2), "%"))

# Filter the data for Bryobacter in the Soil sample type and Non-straw treatment
bryobacter_soil_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Bryobacter" & Sample_type == "Soil" & Treatment == "Non-straw")

# Calculate the total relative abundance for Bryobacter in the Soil for Non-straw treatment
total_bryobacter_soil_non_straw_abundance_mandel <- bryobacter_soil_non_straw_data_mandel %>%
  summarise(total_bryobacter_soil_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Soil sample type for Non-straw treatment
total_abundance_soil_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Soil" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_soil_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Bryobacter in the Soil for Non-straw treatment
bryobacter_soil_non_straw_percentage_mandel <- total_bryobacter_soil_non_straw_abundance_mandel$total_bryobacter_soil_non_straw_abundance_mandel / total_abundance_soil_non_straw_mandel$total_all_genera_soil_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Bryobacter Relative Abundance Percentage in Soil Non-straw: ", round(bryobacter_soil_non_straw_percentage_mandel, 2), "%"))



library(dplyr)

# Filter the data for Pseudomonas in the Tuber Peel sample type and Straw treatment
pseudomonas_tuber_peel_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Pseudomonas" & Sample_type == "Tuber Peel" & Treatment == "Straw")

# Calculate the total relative abundance for Pseudomonas in the Tuber Peel for Straw treatment
total_pseudomonas_tuber_peel_straw_abundance_mandel <- pseudomonas_tuber_peel_straw_data_mandel %>%
  summarise(total_pseudomonas_tuber_peel_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Straw treatment
total_abundance_tuber_peel_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Straw") %>%
  summarise(total_all_genera_tuber_peel_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Pseudomonas in the Tuber Peel for Straw treatment
pseudomonas_tuber_peel_straw_percentage_mandel <- total_pseudomonas_tuber_peel_straw_abundance_mandel$total_pseudomonas_tuber_peel_straw_abundance_mandel / total_abundance_tuber_peel_straw_mandel$total_all_genera_tuber_peel_straw * 100

# Print the result for Straw treatment
print(paste("Pseudomonas Relative Abundance Percentage in Tuber Peel Straw: ", round(pseudomonas_tuber_peel_straw_percentage_mandel, 2), "%"))

# Filter the data for Pseudomonas in the Tuber Peel sample type and Non-straw treatment
pseudomonas_tuber_peel_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Pseudomonas" & Sample_type == "Tuber Peel" & Treatment == "Non-straw")

# Calculate the total relative abundance for Pseudomonas in the Tuber Peel for Non-straw treatment
total_pseudomonas_tuber_peel_non_straw_abundance_mandel <- pseudomonas_tuber_peel_non_straw_data_mandel %>%
  summarise(total_pseudomonas_tuber_peel_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Non-straw treatment
total_abundance_tuber_peel_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_tuber_peel_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Pseudomonas in the Tuber Peel for Non-straw treatment
pseudomonas_tuber_peel_non_straw_percentage_mandel <- total_pseudomonas_tuber_peel_non_straw_abundance_mandel$total_pseudomonas_tuber_peel_non_straw_abundance_mandel / total_abundance_tuber_peel_non_straw_mandel$total_all_genera_tuber_peel_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Pseudomonas Relative Abundance Percentage in Tuber Peel Non-straw: ", round(pseudomonas_tuber_peel_non_straw_percentage_mandel, 2), "%"))


library(dplyr)

# Filter the data for Mucilaginibacter in the Tuber Peel sample type and Straw treatment
mucilaginibacter_tuber_peel_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Mucilaginibacter" & Sample_type == "Tuber Peel" & Treatment == "Straw")

# Calculate the total relative abundance for Mucilaginibacter in the Tuber Peel for Straw treatment
total_mucilaginibacter_tuber_peel_straw_abundance_mandel <- mucilaginibacter_tuber_peel_straw_data_mandel %>%
  summarise(total_mucilaginibacter_tuber_peel_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Straw treatment
total_abundance_tuber_peel_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Straw") %>%
  summarise(total_all_genera_tuber_peel_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in the Tuber Peel for Straw treatment
mucilaginibacter_tuber_peel_straw_percentage_mandel <- total_mucilaginibacter_tuber_peel_straw_abundance_mandel$total_mucilaginibacter_tuber_peel_straw_abundance_mandel / total_abundance_tuber_peel_straw_mandel$total_all_genera_tuber_peel_straw * 100

# Print the result for Straw treatment
print(paste("Mucilaginibacter Relative Abundance Percentage in Tuber Peel Straw: ", round(mucilaginibacter_tuber_peel_straw_percentage_mandel, 2), "%"))

# Filter the data for Mucilaginibacter in the Tuber Peel sample type and Non-straw treatment
mucilaginibacter_tuber_peel_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Mucilaginibacter" & Sample_type == "Tuber Peel" & Treatment == "Non-straw")

# Calculate the total relative abundance for Mucilaginibacter in the Tuber Peel for Non-straw treatment
total_mucilaginibacter_tuber_peel_non_straw_abundance_mandel <- mucilaginibacter_tuber_peel_non_straw_data_mandel %>%
  summarise(total_mucilaginibacter_tuber_peel_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Non-straw treatment
total_abundance_tuber_peel_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_tuber_peel_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Mucilaginibacter in the Tuber Peel for Non-straw treatment
mucilaginibacter_tuber_peel_non_straw_percentage_mandel <- total_mucilaginibacter_tuber_peel_non_straw_abundance_mandel$total_mucilaginibacter_tuber_peel_non_straw_abundance_mandel / total_abundance_tuber_peel_non_straw_mandel$total_all_genera_tuber_peel_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Mucilaginibacter Relative Abundance Percentage in Tuber Peel Non-straw: ", round(mucilaginibacter_tuber_peel_non_straw_percentage_mandel, 2), "%"))


library(dplyr)

# Filter the data for Pedobacter in the Tuber Peel sample type and Straw treatment
pedobacter_tuber_peel_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Pedobacter" & Sample_type == "Tuber Peel" & Treatment == "Straw")

# Calculate the total relative abundance for Pedobacter in the Tuber Peel for Straw treatment
total_pedobacter_tuber_peel_straw_abundance_mandel <- pedobacter_tuber_peel_straw_data_mandel %>%
  summarise(total_pedobacter_tuber_peel_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Straw treatment
total_abundance_tuber_peel_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Straw") %>%
  summarise(total_all_genera_tuber_peel_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in the Tuber Peel for Straw treatment
pedobacter_tuber_peel_straw_percentage_mandel <- total_pedobacter_tuber_peel_straw_abundance_mandel$total_pedobacter_tuber_peel_straw_abundance_mandel / total_abundance_tuber_peel_straw_mandel$total_all_genera_tuber_peel_straw * 100

# Print the result for Straw treatment
print(paste("Pedobacter Relative Abundance Percentage in Tuber Peel Straw: ", round(pedobacter_tuber_peel_straw_percentage_mandel, 2), "%"))

# Filter the data for Pedobacter in the Tuber Peel sample type and Non-straw treatment
pedobacter_tuber_peel_non_straw_data_mandel <- Genus_summary_top10_mandel %>%
  filter(Genus == "Pedobacter" & Sample_type == "Tuber Peel" & Treatment == "Non-straw")

# Calculate the total relative abundance for Pedobacter in the Tuber Peel for Non-straw treatment
total_pedobacter_tuber_peel_non_straw_abundance_mandel <- pedobacter_tuber_peel_non_straw_data_mandel %>%
  summarise(total_pedobacter_tuber_peel_non_straw_abundance_mandel = sum(relative_abundance))

# Calculate the total relative abundance for all genera in the Tuber Peel sample type for Non-straw treatment
total_abundance_tuber_peel_non_straw_mandel <- Genus_summary_top10_mandel %>%
  filter(Sample_type == "Tuber Peel" & Treatment == "Non-straw") %>%
  summarise(total_all_genera_tuber_peel_non_straw = sum(relative_abundance))

# Calculate the percentage relative abundance of Pedobacter in the Tuber Peel for Non-straw treatment
pedobacter_tuber_peel_non_straw_percentage_mandel <- total_pedobacter_tuber_peel_non_straw_abundance_mandel$total_pedobacter_tuber_peel_non_straw_abundance_mandel / total_abundance_tuber_peel_non_straw_mandel$total_all_genera_tuber_peel_non_straw * 100

# Print the result for Non-straw treatment
print(paste("Pedobacter Relative Abundance Percentage in Tuber Peel Non-straw: ", round(pedobacter_tuber_peel_non_straw_percentage_mandel, 2), "%"))
