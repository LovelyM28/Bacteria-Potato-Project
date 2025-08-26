library(readxl)

# Load the Excel file
file_path <- "C:\\Users\\loma0112\\OneDrive - Umeå universitet\\Documents\\Kempe project\\Summer field experiment 2023\\metagenomics from potato field experiment\\Plots and codes\\rarefaction curves\\feature-table_modified.xlsx"

data <- read_excel(file_path)

# Print the first few rows to verify
head(data)
str(data)
rm (metadata)
rm (filtered_data)
# Identify columns where the value in row 46472 (Cultivar row) is 2
cultivar2_columns <- which(data[46472, ] == "2")

# Add the ASVs column (assuming it's the first column)
selected_columns <- c(1, cultivar2_columns)

# Subset the dataset
king_data <- data[, selected_columns]

# View the filtered dataset
head(king_data)
str(king_data)
library(vegan)
rm(metadata)
# Extract metadata (last three rows)
metadataking <- king_data[(nrow(king_data) - 2):nrow(king_data), ]

# Extract ASV count data (all rows except the last three, and all columns except ASVs)
count_dataking <- king_data[1:(nrow(king_data) - 3), -1]

# Set sample names as column names
colnames(count_dataking) <- colnames(king_data)[-1]
# Transpose the count data to make samples as rows and ASVs as columns
count_dataking_t <- t(count_dataking)

# Convert to a data frame
count_dataking_t <- as.data.frame(count_dataking_t)
# Check for non-integer values or negative values
if (any(count_dataking_t < 0) || any(count_dataking_t != round(count_dataking_t))) {
  stop("Count data must contain non-negative integers.")
}
# Create rarefaction curves
rarecurve(count_dataking_t, step = 20, sample = min(rowSums(count_dataking_t)), col = "blue", label = TRUE)

# Check the structure of metadataking
str(metadataking)
# Convert sample_typeking and treatmentking from lists to vectors
sample_typeking <- unlist(metadataking[1, -1])  # Convert Sample_type to vector
treatmentking <- unlist(metadataking[3, -1])    # Convert Treatment to vector

# Check the first few elements of each vector
head(sample_typeking)
head(treatmentking)
# Convert sample_typeking and treatmentking to factors
sample_typeking <- factor(sample_typeking)  # Convert to factor
treatmentking <- factor(treatmentking)      # Convert to factor

# Check the first few elements to confirm the conversion
head(sample_typeking)
head(treatmentking)

# Create a combined factor by interacting Sample_type and Treatment
combined_factorking <- interaction(sample_typeking, treatmentking)

# Check the first few values of the combined factor
head(combined_factorking)

rarecurve(count_dataking_t, step = 20, sample = min(rowSums(count_dataking_t)), col = combined_factorking, label = FALSE)

# Add a legend to indicate the different levels of the combined factor
legend("topright", legend = levels(combined_factorking), col = 1:length(levels(combined_factorking)), pch = 1)

#Create a new data frame where we sum the counts for each combination of Sample_type and Treatment
grouped_countsking <- aggregate(count_dataking_t, by = list(combined_factorking), FUN = sum)

# Plot rarefaction curves for each group
rarecurve(grouped_countsking[, -1], step = 20, sample = min(rowSums(grouped_countsking[, -1])), 
          col = as.factor(grouped_countsking$Group.1), label = FALSE)

# Step 3: Add a legend to the plot to indicate which color corresponds to which group
legend("topright", legend = levels(combined_factorking), col = 1:length(levels(combined_factorking)), pch = 1)


# Create a new data frame where we sum the counts for each combination of Sample_type and Treatment
grouped_countsking <- aggregate(count_dataking_t, by = list(combined_factorking), FUN = sum)

# Define updated custom colors for each group based on the provided order
colors <- c("#ff6347", "#ffbf00", "#66b032", "#1e90ff", "#ff0000", "#c46210", "#008000", "#0000ff")

# Plot rarefaction curves for each group
rarecurve(grouped_countsking[, -1], 
          step = 50, 
          sample = min(rowSums(grouped_countsking[, -1])), 
          col = colors,        # Set the updated colors for each curve
          lwd = 3,             # Set the line width (thickness)
          lty = 1,             # Solid lines
          xlab = "Reads",      # X-axis label
          ylab = "ASVs",       # Y-axis label
          label = FALSE,       # Disable individual labels for curves
          cex.lab = 1.5,       # Increase the size of X and Y axis labels
          font.lab = 2,        # Make axis labels bold
          panel.first = NULL)  # Optional: Remove any custom elements before the plot

# Customize grid lines (optional)
grid(nx = NULL, ny = 8)  # Custom grid lines (8 horizontal lines)

# Add a legend inside the plot box at the bottom right without a title
legend("bottomright", 
       legend = levels(combined_factorking),  # Use the levels of combined_factorking for legend
       col = colors,                         # Set the updated colors for each group in the legend
       pch = 1,                              # Set the point type (1 is a solid circle)
       lwd = 2,                              # Set the line width in the legend
       cex = 1.2,                            # Adjust the font size in the legend
       bty = "o")                            # Add a box around the legend

# Plot rarefaction curves for each group
rarecurve(grouped_countsking[, -1], 
          step = 1000, 
          sample = 46470, 
          col = colors,        # Set the updated colors for each curve
          lwd = 3,             # Set the line width (thickness)
          lty = 1,             # Solid lines
          xlab = "Reads",      # X-axis label
          ylab = "ASVs",       # Y-axis label
          label = FALSE,       # Disable individual labels for curves
          cex.lab = 1.5,       # Increase the size of X and Y axis labels
          font.lab = 2,        # Make axis labels bold
          panel.first = NULL)  # Optional: Remove any custom elements before the plot

# Add a legend inside the plot box at the bottom right without a title
legend("bottomright", 
       legend = levels(combined_factorking),  # Use the levels of combined_factorking for legend
       col = colors,                         # Set the updated colors for each group in the legend
       pch = 1,                              # Set the point type (1 is a solid circle)
       lwd = 2,                              # Set the line width in the legend
       cex = 1.2,                            # Adjust the font size in the legend
       bty = "o")                            # Add a box around the legend



# Plot rarefaction curves for each group
rarecurve(grouped_countsking[, -1], 
          step = 1000, 
          sample = 46470,  # This is the total number of ASVs in your dataset
          col = colors,        # Set the updated colors for each curve
          lwd = 3,             # Set the line width (thickness)
          lty = 1,             # Solid lines
          xlab = "Reads",       # X-axis label 
          ylab = "ASVs",       # Y-axis label
          label = FALSE,       # Disable individual labels for curves
          cex.lab = 1.5,       # Increase the size of X and Y axis labels
          font.lab = 2,        # Make axis labels bold
          panel.first = NULL)  # Remove any custom elements before the plot
      

# Add a legend inside the plot box at the bottom right without a title
legend("bottomright", 
       legend = levels(combined_factorking),  # Use the levels of combined_factorking for legend
       col = colors,                         # Set the updated colors for each group in the legend
       pch = 1,                              # Set the point type (1 is a solid circle)
       lwd = 2,                              # Set the line width in the legend
       cex = 1.2,                            # Adjust the font size in the legend
       bty = "o")                            # Add a box around the legend




