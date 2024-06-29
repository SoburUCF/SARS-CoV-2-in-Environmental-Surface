# Import required libraries

library(tidyverse)


###### Plotting the dots based on the date with color gradient
#Import data
cases_genecopy <- read.csv("~/Documents/swab_study/statistical/cases_genecoyp.csv")
cases_genecopy$date <- as.Date(cases_genecopy$date) # convert date column to date format


# Compute Spearman's rank correlation
correlation <- cor.test(cases_genecopy$covid_cases, df$viral_load, method = "spearman")


# Create a color scale for date of collection 
col_scale <- scale_fill_gradient(low = "blue", high = "red", limits = c(0.98, 1.00))


ggplot(cases_genecopy, aes(x = covid_cases, y = viral_load, colour = as.numeric(date))) +
  geom_point(size = 3) +
  scale_colour_gradient(low = "blue", high = "red", breaks = as.numeric(lab_dates2), labels=lab_dates2) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "black") +
  geom_abline(intercept = -0.2, slope = 0.04, color = "red") +
  geom_text(x = 0.05, y = 1.25, size = 4, color = "black",
            label = paste("Slope, 95% CI: 0.00, 3.83", 
                          "\nIntercept, 95% CI: 0.09, 1.3", 
                          "\nCorrelation Coefficient (rho): 0.63", 
                          "\np-value: <0.001"), 
            hjust = 0, vjust = 1)+
  labs(x = "COVID-19 confirmed cases",
       y = expression("Environmental surface viral gene copy in Log10(gc/cm"^" 2"*")"))+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 13, color = "black"))+
  theme_classic()
