library(pacman)
library(tidyverse)

#Set the working directory
setwd("~/Documents/swab_study/statistical/")

# import the data
ctvaluegc <- read.csv("ctvalue_gc.csv", header = TRUE)

# convert the viral load data to Log10 value 
ctvaluegc$log10_viral_load <- ifelse(is.na(ctvaluegc$viral_load), NA, log10(ctvaluegc$viral_load))

ggplot(ctvaluegc, aes(x = location, y = log10_viral_load, fill = location)) +
  geom_violin(scale = "width", alpha = 0.5) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.2, color = "black") +
  labs(x = "Sampling Locations",
       y = expression("Environmental surface viral Log10(gc/cm"^" 2"*")")) +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF", "#FFA07A", "#A878EC")) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF", "#FFA07A", "#A878EC")) +
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 13, color = "black"))

## Statistical test to see if physical science is siginificantly lower then others

# Subset data to include only Physical Science location
physical_science <- subset(ctvaluegc, location == "Physical Science")

student_union <- subset(ctvaluegc, location == "Student Union")
Lib <- subset(ctvaluegc, location == "Library")
Gym <- subset(ctvaluegc, location == "Gymnasium")
bbs <- subset(ctvaluegc, location == "Business Building")

# Subset data to exclude Physical Science location
other_locations <- subset(ctvaluegc, location != "Physical Science")

# Perform t-test
t.test(physical_science$log10_viral_load, student_union$log10_viral_load)
t.test(physical_science$log10_viral_load, Lib$log10_viral_load)
t.test(physical_science$log10_viral_load, bbs$log10_viral_load)

# Print t-test result
print(t_test)

# Perform ANOVA
anova_result <- aov(log10_viral_load ~ location, data = ctvaluegc)

# Print ANOVA result
print(summary(anova_result))
tukey_result <- TukeyHSD(anova_result)




