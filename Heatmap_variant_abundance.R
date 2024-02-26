
## Install packages
install.packages("reshape2")
library(reshape2) # to generate heatmap
library(ggplot2)
library(forcats) # to reshape the data
library(dplyr)

linabdata <- read.csv("~/Documents/swab_study/raw_data/Lineage_dist_UCF.csv", header = TRUE)

# Handle missing values by replacing them with 0
linabdata[is.na(linabdata)] <- 0

week_column <- "week"
lineage_columns <- c("BA.1.x", "B.1.617", "BA.2.12.x", "BA.2.x", "BA.4.x", "BA.5.x", "BE.x", "BF.x", "BQ.1.x", "BG.x","XAS")
location_column <- "Source"

## Data manipulation

melted_data <- melt(linabdata, id.vars = c(week_column, location_column), measure.vars = lineage_columns)


# Define the desired order of levels for the 'Source' factor
desired_order <- c("Environmental Swab", "University SHC (Clinical)", "Florida State (Clinical)")  # Add all source levels

# Reorder the 'Source' factor according to the desired order 
melted_data$Source <- factor(melted_data$Source, levels = desired_order)

melted_data[is.na(melted_data)] <- "Florida State (Clinical)"

## Final one after all modication

ggplot(melted_data, aes(x = variable, y = fct_rev(week), fill = value)) +
  geom_tile(width = 1, height = 1) +
  geom_tile(data = subset(melted_data, value > 0), color = "black", size = 0.2) +
  scale_fill_gradient(low = "white", high = "red", name = "Abundance(%)") +
  labs(x = "Detected SARS-CoV-2 lineages", y = "Weeks") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),  # Set text color to black
    axis.text.y = element_text(color = "black"),  # Set y-axis text color to black
    axis.title.x = element_text(color = "black"),  # Set x-axis label color to black
    axis.title.y = element_text(color = "black"),  # Set y-axis label color to black
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  facet_wrap(~Source, ncol = 3) +
  coord_fixed(ratio = 1)
