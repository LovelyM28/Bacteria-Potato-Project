# Load required package
library(readxl)

# Define file paths
file1 <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/Sample_sheet_bacteria.xlsx"
file2 <- "C:/Users/loma0112/OneDrive - Umeå universitet/Documents/Kempe project/Summer field experiment 2023/metagenomics from potato field experiment/Plots and codes/abs-abund-table-6.xlsx"

# Read the Excel files
sample_sheet <- read_excel(file1)
abundance_table <- read_excel(file2)

library(dplyr)

sample_sheet <- sample_sheet %>%
  mutate(Treatment = recode(Treatment,
                            "Non-straw" = "Control",
                            "Straw" = "Mulch"))

# View top rows of each
head(sample_sheet)
head(abundance_table)
library(vegan)
# Remove the OTU ID column and keep the numeric data
abundance_data <- abundance_table[, -1]  # Remove the first column (OTU IDs)
# Remove rows with all-zero counts (i.e., taxa/OTUs not present in any sample)
abundance_data <- abundance_data[rowSums(abundance_data) > 0, ]
# Calculate Bray-Curtis dissimilarity matrix
bray_dist <- vegdist(abundance_data, method = "bray")

# Run NMDS (k = 2 dimensions is default)
nmds_result <- metaMDS(bray_dist, k = 2, trymax = 100)

# Extract NMDS scores for samples
nmds_scores <- as.data.frame(scores(nmds_result))
nmds_scores$Sample <- rownames(nmds_scores)  # Ensure correct sample names
library(dplyr)
library(ggplot2)
nmds_data <- merge(nmds_scores, sample_sheet, by = "Sample")
# Compute mean NMDS1 and NMDS2 for each (Sample_type, Treatment)
nmds_avg <- nmds_data %>%
  group_by(Sample_type, Treatment, Cultivar) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2), .groups = 'drop')

p <- ggplot(nmds_avg, aes(x = NMDS1, y = NMDS2, color = Sample_type, shape = Treatment)) +
  geom_point(size = 5, position = position_jitter(width = 0.02, height = 0.02)) +
  scale_color_manual(values = c("Rhizosphere" = "#c46210", "Root" = "#008000", "Soil" = "#ff0000", "Tuber Peel" = "#0000ff" )) +
  scale_shape_manual(values = c("Control" = 15, "Mulch" = 17)) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "eDNA source",
    shape = "Treatment"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
strip.text = element_text(size = 16, face = "bold")  # <- facet label font
)+
  annotate("text", x = Inf, y = Inf,
           label = paste("Stress =", round(nmds_result$stress, 4)),
           hjust = 1.1, vjust = 2, size = 5, color = "black") +
  guides(
    color = guide_legend(override.aes = list(linetype = 0)),
    shape = guide_legend(override.aes = list(linetype = 0))
  ) +
  facet_wrap(~ Cultivar)
print(p)

# Save the plot as a PNG file
ggsave("nmds_plot.png", plot = p, width = 10, height = 8, dpi = 300)





# 1. Transpose abundance_data so samples become rows
abundance_data_t <- as.data.frame(t(abundance_data))

# 2. Set proper column name for sample IDs
abundance_data_t$Sample <- as.numeric(rownames(abundance_data_t))

# 3. Merge with sample_sheet on the Sample column
metadata <- merge(sample_sheet, abundance_data_t, by = "Sample")


# Assuming `metadata` and `abundance_data` are already merged correctly
library(vegan)

# Ensure samples are rows in abundance data
abundance_data_t <- t(abundance_data)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(abundance_data_t, method = "bray")

# Run PERMANOVA
permanova_result <- adonis2(bray_dist ~ Treatment * Sample_type * Cultivar, data = metadata)
print(permanova_result)

# Main effect of Treatment
treatment_effect <- adonis2(bray_dist ~ Treatment, data = metadata)
print(treatment_effect)

# Main effect of Sample_type
sample_type_effect <- adonis2(bray_dist ~ Sample_type, data = metadata)
print(sample_type_effect)

# Main effect of Cultivar
cultivar_effect <- adonis2(bray_dist ~ Cultivar, data = metadata)
print(cultivar_effect)


cultivartreatment_effect <- adonis2(bray_dist ~ Cultivar * Treatment, data = metadata)
print(cultivartreatment_effect )

treatmentsample_effect <- adonis2(bray_dist ~ Treatment * Sample_type, data = metadata)
print(treatmentsample_effect)



cultivarsample_type_effect <- adonis2(bray_dist ~ Cultivar * Sample_type, data = metadata)
print(cultivarsample_type_effect)

