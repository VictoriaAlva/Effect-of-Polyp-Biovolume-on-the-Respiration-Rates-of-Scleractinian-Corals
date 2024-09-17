################################################################################
############ Effect of Polyp Biovolume on the Respiration Rates of #############
############################# Scleractinian Corals #############################
################################################################################

# Install the necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggpmisc")
install.packages("dplyr")
install.packages("stats")
install.packages("dunn.test")
install.packages("car")
install.packages("openxlsx")
install.packages("RColorBrewer")

# Load the necessary packages
library(readxl)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(stats)
library(dunn.test)
library(car)
library(openxlsx)
library(RColorBrewer)

# Load the CSV file for the analyses
data <- read_excel("/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Statistical Analysis/Data_sheet.xlsx")

# Verify the file
head(data)

tail(data)

############################# Statistics analysis #############################

# Summary Statistics 
summary(data)

# Normality Assessment
respiration <- data$Respiration_per_polyp
biovolume <- data$Average_Biovolume

# Shapiro-Wilk Test for Normality and Q-Q plots (For Respiration rates and Biovolume)
## Respiration
shapiro.test(respiration)

ggplot(data, aes(x=Respiration_per_polyp)) + 
  geom_histogram(aes(y=..density..), bins=30, fill="blue", alpha=0.7) + 
  geom_density(color="red") + 
  labs(title="Histogram of Respiration per Polyp", x="Respiration per Polyp", y="Density")

ggplot(data, aes(sample=Respiration_per_polyp)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Q-Q Plot of Respiration per Polyp", x="Theoretical Quantiles", y="Sample Quantiles")

## Biovolume
shapiro.test(biovolume)

ggplot(data, aes(x=Average_Biovolume)) + 
  geom_histogram(aes(y=..density..), bins=30, fill="blue", alpha=0.7) + 
  geom_density(color="red") + 
  labs(title="Histogram of Average Biovolume", x="Average Biovolume", y="Density")

ggplot(data, aes(sample=Average_Biovolume)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Q-Q Plot of Average Biovolume", x="Theoretical Quantiles", y="Sample Quantiles")

# Levene's Test for Homogeneity 
data$Biovolume_group <- cut(data$Average_Biovolume, breaks=4)
leveneTest(Respiration_per_polyp ~ Biovolume_group, data=data)

# Kruskal-Wallis Test for statisticaly significant differences between species "biovolumes"
kruskal_test <- kruskal.test(Log10_Average_Biovolume ~ Species, data = data)
print(kruskal_test)

# Dunn's Test PostHoc Analysis with Bonferroni Correction
dunn_result <- dunn.test(data$Log10_Average_Biovolume, data$Species, method = "bonferroni")
dunn_result_table <- data.frame(
  Comparison = dunn_result$comparisons,
  Z = dunn_result$Z,
  P.unadjusted = dunn_result$P,
  P.adjusted = dunn_result$P.adjusted
)

print(dunn_result_table)

write.xlsx(dunn_result_table, "Dunn_Test_Results_Biovolume.xlsx")

# Kruskal-Wallis Test for statisticaly significant differences between species "respiration rates"
kruskal_test <- kruskal.test(Log10_RR ~ Species, data = data)
print(kruskal_test)

# Dunn's Test PostHoc Analysis with Bonferroni Correction
dunn_result <- dunn.test(data$Log10_RR, data$Species, method = "bonferroni")
dunn_result_table <- data.frame(
  Comparison = dunn_result$comparisons,
  Z = dunn_result$Z,
  P.unadjusted = dunn_result$P,
  P.adjusted = dunn_result$P.adjusted
)

print(dunn_result_table)

write.xlsx(dunn_result_table, "Dunn_Test_Results_RR.xlsx")

# T-test 
# Given data
sample_mean <- 0.95449 # Your observed mean
sem <- 0.04043  # Standard error of the mean
n <- 13  # Sample size

# Calculate the t-statistic
t_statistic <- (sample_mean - 1) / sem

# Calculate the degrees of freedom
df <- n - 1

# Calculate the p-value
p_value <- 2 * pt(-abs(t_statistic), df)

# Print the results
cat("t-statistic:", t_statistic, "\n")
cat("p-value:", p_value, "\n")


########################## Linear Regressions & Plots ##########################


################################################################################
####################### "Respiration rates vs Biovolume" #######################
################################################################################
# Log10 of my data
data$log_biovolume <- log10(data$Average_Biovolume)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

# Linear regression
linear_model <- lm(log_respiration_rates ~ log_biovolume, data = data)

# Create a sequence of biovolume values for a smooth line across the range
biovolume_seq <- data.frame(log_biovolume = seq(min(data$log_biovolume), max(data$log_biovolume), length.out = 100))

# Get predictions with confidence intervals for the new sequence
predictions <- predict(linear_model, newdata = biovolume_seq, interval = "confidence")

# Add predictions and confidence intervals to the new sequence data
biovolume_seq$fit <- predictions[, "fit"]
biovolume_seq$lwr <- predictions[, "lwr"]
biovolume_seq$upr <- predictions[, "upr"]

# Calculate mean biovolume per Species
Species_means <- data %>%
  group_by(Species) %>%
  summarise(MeanBiovolume = mean(Random_Biovolume, na.rm = TRUE)) %>%
  arrange(desc(MeanBiovolume)) %>%
  ungroup()

# Create an ordered factor for the Species based on mean biovolume
data$Species <- factor(data$Species, levels = Species_means$Species)

# Define a set of 13 easily distinguishable shapes
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

# Plot with manually calculated confidence intervals using the new sequence data
p <- ggplot(data, aes(x = log_biovolume, y = log_respiration_rates, colour = Species, shape = Species)) +
  geom_point() +
  # Add the confidence intervals manually using geom_ribbon from the new biovolume_seq data
  geom_ribbon(data = biovolume_seq, aes(x = log_biovolume, ymin = lwr, ymax = upr), alpha = 0.2, fill = "grey5", inherit.aes = FALSE) +
  # Add the fitted line using the new biovolume_seq data
  geom_line(data = biovolume_seq, aes(x = log_biovolume, y = fit), color = "black", size = 1.2, inherit.aes = FALSE) +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color="black", size = 16),
    axis.text.y = element_text(color="black", size = 16), 
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = expression(Log[10]~"(Biovolume)"),  # Subscripted 10 for Log10
       y = expression(Log[10]~"(Respiration Rates)"),  # Subscripted 10 for Log10
       title = "")

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Figure 1.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300,  device = "tiff")

print(p)


################################################################################
######################## "Respiration rates vs Species" ########################
################################################################################

p <- ggplot(data, aes(x = Species, y = Respiration_per_polyp, colour = Species)) +
  geom_boxplot() +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none", size = 16,
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "italic"),
    axis.text.y = element_text(color="black", size = 16), 
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18), # Y-axis title size
    panel.background = element_rect(fill = "white", color = NA), # Set the panel background to white
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(fill = NA, color = "black"), # Keep the border lines
    axis.line = element_line(color = "black") # Keep the axis lines
  ) +
  labs(x = "Species", y = expression(Respiration~Rates~(µmol * h^-1 * polyp^-1)), title = "")

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 4.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300,  device = "tiff", type = "cairo")

print(p)


################################################################################
############################ "Biovolume vs Species" ############################
################################################################################

p <- ggplot(data, aes(x = Species, y = Random_Biovolume, colour = Species)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  theme_light() +  
  theme(
    text = element_text(family = "Arial"),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16, face = "italic"),
    axis.text.y = element_text(color="black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18) # Y-axis title size
  ) + # Adjust text angle for legibility
  labs(x = "Species", y = expression(Biovolume ~(mm^3)), title = "") +
  scale_y_continuous(limits = c(0, 4000)) 

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 3.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300,  device = "tiff", type = "cairo")

print(p)


################################################################################
##################### "Respiration rates vs Surface Area" ######################
################################################################################

# Log10 of my data
data$log_SA <- log10(data$Surface_area_per_polyp)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

# Fit the linear model
model <- lm(log_respiration_rates ~ log_SA, data = data)

# Create a sequence of log_SA values to predict fitted values and confidence intervals
SA_seq <- data.frame(log_SA = seq(min(data$log_SA), max(data$log_SA), length.out = 100))

# Get the predicted values and confidence intervals
predictions <- predict(model, newdata = SA_seq, interval = "confidence")

# Add the predicted values and confidence intervals to the SA_seq data frame
SA_seq$fit <- predictions[, "fit"]
SA_seq$lwr <- predictions[, "lwr"]
SA_seq$upr <- predictions[, "upr"]

# Define shape values
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

# Plot with manually calculated confidence intervals
p <- ggplot(data, aes(x = log_SA, y = log_respiration_rates, colour = Species, size = Surface_area_per_polyp, shape = Species)) +
  geom_point() +
  # Add the manually calculated confidence intervals using geom_ribbon
  geom_ribbon(data = SA_seq, aes(x = log_SA, ymin = lwr, ymax = upr), fill = "grey5", alpha = 0.2, inherit.aes = FALSE) +
  # Add the manually calculated fitted line
  geom_line(data = SA_seq, aes(x = log_SA, y = fit), color = "black", size = 1.2, inherit.aes = FALSE) +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) +
  scale_size_continuous(breaks = c(1, 10, 50, 100, 150),
                        labels = c("1", "10", "50", "100", "150"),
                        range = c(1, 5)) +
  theme_minimal() +  # Start with a minimal theme
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    axis.title.x = element_text(size = 18),  # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  guides(size = guide_legend(override.aes = list(shape = 21, fill = "white"))) +
  labs(x = expression(Log[10]~"(Surface Area)"), y = expression(Log[10]~"(Respiration Rates)"), 
       colour = "Species", size = expression(Surface~Area~(cm^2)))

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Figure 2.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff")

print(p)


################################################################################
########################## "Surface Area vs Biovolume" #########################
################################################################################

# Log10 of my data
data$log_Biovolume <- log10(data$Average_Biovolume)
data$log_SA <- log10(data$Average_Surface_Area)

# Linear regression
model <- lm(log_SA ~ log_Biovolume, data = data)
summary(model)

# Create a sequence of log_Biovolume values for prediction
biovolume_seq <- data.frame(log_Biovolume = seq(min(data$log_Biovolume), max(data$log_Biovolume), length.out = 100))

# Get the predicted values and confidence intervals
predictions <- predict(model, newdata = biovolume_seq, interval = "confidence")

# Add the predicted values and confidence intervals to the biovolume_seq data frame
biovolume_seq$fit <- predictions[, "fit"]
biovolume_seq$lwr <- predictions[, "lwr"]
biovolume_seq$upr <- predictions[, "upr"]

# Define shape values and gray color palette
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)
gray_colors <- c("#1A1A1A", "#4D4D4D", "#808080", "#B3B3B3", "#E6E6E6")

# Plot with manually calculated confidence intervals
p <- ggplot(data, aes(x = log_Biovolume, y = log_SA, colour = Species, shape = Species)) +
  geom_point() +
  # Add the confidence intervals using geom_ribbon
  geom_ribbon(data = biovolume_seq, aes(x = log_Biovolume, ymin = lwr, ymax = upr), fill = "grey5", alpha = 0.2, inherit.aes = FALSE) +
  # Add the fitted line using geom_line
  geom_line(data = biovolume_seq, aes(x = log_Biovolume, y = fit), color = "black", size = 1.2, inherit.aes = FALSE) +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) +
  scale_fill_manual(values = gray_colors[1:length(unique(data$Species))]) +
  theme_light() +  # Starts with a light theme
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black", size = 16),  # Adjust text angle for legibility
    axis.text.y = element_text(color = "black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = expression(Log[10]~"(Surface Area)"), y = expression(Log[10]~"(Biovolume)"), 
       title = "")

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 6.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff")

print(p)


################################################################################
########################## "Predicted vs Exprimental" ##########################
################################################################################

# Fit the linear model
model <- lm(Predicted_RR ~ Log10_RR, data = data)

# Create a sequence of Log10_RR values for prediction
RR_seq <- data.frame(Log10_RR = seq(min(data$Log10_RR), max(data$Log10_RR), length.out = 100))

# Get the predicted values and confidence intervals
predictions <- predict(model, newdata = RR_seq, interval = "confidence")

# Add the predicted values and confidence intervals to the RR_seq data frame
RR_seq$fit <- predictions[, "fit"]
RR_seq$lwr <- predictions[, "lwr"]
RR_seq$upr <- predictions[, "upr"]

# Plot with manually calculated confidence intervals
p <- ggplot(data, aes(x = Log10_RR, y = Predicted_RR, size = Average_Biovolume)) +
  geom_point(shape = 21, fill = "black") +
  # Add the manually calculated confidence intervals using geom_ribbon
  geom_ribbon(data = RR_seq, aes(x = Log10_RR, ymin = lwr, ymax = upr), fill = "grey5", alpha = 0.2, inherit.aes = FALSE) +
  # Add the manually calculated fitted line using geom_line
  geom_line(data = RR_seq, aes(x = Log10_RR, y = fit), color = "black", size = 1.2, inherit.aes = FALSE) +
  scale_size_continuous(breaks = c(10, 100, 150, 300, 500, 1000),
                        labels = c("10", "100", "150", "300", "500", "1,000"),
                        range = c(1, 10)) +
  theme_light() + 
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black", size = 16),  # Adjust text angle for legibility
    axis.text.y = element_text(color = "black", size = 16),
    axis.title.x = element_text(size = 18),  # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = expression(Log[10]~"(Experimental Respiration Rates)"), y = expression(Log[10]~"(Predicted Respiration Rates)"), 
       size = expression(Biovolume ~ (mm^3))) +
  guides(size = guide_legend(override.aes = list(shape = 21, fill = "white", color = "black")))

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Figure 3.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff")

print(p)


################################################################################
##################### "Respiration rates vs Surface Area" ######################
################################################################################

# Log10 of my data
data$log_SA <- log10(data$Average_Surface_Area)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

# Fit the linear model
model <- lm(log_respiration_rates ~ log_SA, data = data)

# Create a sequence of log_SA values to predict fitted values and confidence intervals
SA_seq <- data.frame(log_SA = seq(min(data$log_SA), max(data$log_SA), length.out = 100))

# Get the predicted values and confidence intervals
predictions <- predict(model, newdata = SA_seq, interval = "confidence")

# Add the predicted values and confidence intervals to the SA_seq data frame
SA_seq$fit <- predictions[, "fit"]
SA_seq$lwr <- predictions[, "lwr"]
SA_seq$upr <- predictions[, "upr"]

# Define shape values
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

# Plot with manually calculated confidence intervals
p <- ggplot(data, aes(x = log_SA, y = log_respiration_rates, colour = Species, shape = Species)) +
  geom_point() +
  # Add the manually calculated confidence intervals using geom_ribbon
  geom_ribbon(data = SA_seq, aes(x = log_SA, ymin = lwr, ymax = upr), fill = "grey5", alpha = 0.2, inherit.aes = FALSE) +
  # Add the manually calculated fitted line
  geom_line(data = SA_seq, aes(x = log_SA, y = fit), color = "black", size = 1.2, inherit.aes = FALSE) +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) +
  scale_size_continuous(breaks = c(1, 10, 50, 100, 150),
                        labels = c("1", "10", "50", "100", "150"),
                        range = c(1, 5)) +
  theme_minimal() +  # Start with a minimal theme
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    axis.title.x = element_text(size = 18),  # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  guides(size = guide_legend(override.aes = list(shape = 21, fill = "white"))) +
  labs(x = expression(Log[10]~"(Surface Area)"), y = expression(Log[10]~"(Respiration Rates)"), 
       colour = "Species", size = expression(Surface~Area~(cm^2)))

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 5.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff")

print(p)


################################################################################
########################## "Predicted Coral Trait CW" ##########################
################################################################################
# Assuming your data is in a dataframe called df, similar to the one provided
# Make sure to replace this with your actual dataframe
df <- read_excel("/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/KAUST/Ph.D Program/Ph.D. Fall 2024-2025/Chapter 2/Prediction_CT.xlsx") 

# Filter out missing data if necessary
df <- df %>%
  filter(!is.na(Log10_Median_CW), !is.na(Respiration_rates))

# Fit a linear model to calculate the confidence intervals manually
model <- lm(Respiration_rates ~ Log10_Median_CW, data = df)

# Predict the values and confidence intervals
predictions <- predict(model, interval = "confidence", level = 0.95)

# Add the predictions back to the dataframe
df <- cbind(df, predictions)

# Create the final plot with similar formatting as previous ones
p <- ggplot(df, aes(x = Log10_Median_CW, y = Respiration_rates)) +
  geom_point(size = 5, shape = 21, fill = "#F8766D", color = "black") + # Same style for points
  geom_line(aes(y = fit), color = "black", linewidth = 1) +  # Fitted line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "gray1") + # Confidence interval ribbon
  labs(x = expression(Log[10]~"(Median Corallite Width)"), y = expression(Log[10]~"(Predicted Respiration Rates)")) + # Axis labels
  theme_light() +  # Use the light theme as per your previous examples
  theme(
    text = element_text(family = "Arial"), # Same font family
    legend.position = "none", # Remove legend
    axis.text.x = element_text(angle = 0, hjust = 1, size = 16, color = "black"), # X-axis text
    axis.text.y = element_text(color = "black", size = 16), # Y-axis text
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18), # Y-axis title size
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA), # Set background to white
    panel.border = element_rect(fill = NA, color = "black"), # Add panel border
    axis.line = element_line(color = "black") # Add axis lines
  )

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 8.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

# Save the plot with the same settings
ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff", type = "cairo")

# Print the plot
print(p)


################################################################################
##################### CORRELATION BETWEEN RR AND MEDIAN CW #####################
################################################################################

# Load the CSV file for the analyses
data <- read_excel("/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/KAUST/Ph.D Program/Ph.D. Fall 2024-2025/Chapter 2/Corallite Width Predictions.xlsx")

# Filter out rows where Species is NA
data <- data %>% filter(!is.na(Species))

# Verify the file
head(data)
tail(data)

# Calculate the correlation between Log10_RR and Log10_Median_CW
correlation <- cor(data$Log10_RR, data$Log10_Median_CW)
print(paste("Correlation: ", correlation))

# Fit a linear model
model <- lm(Log10_RR ~ Log10_Median_CW, data = data)

# Get the summary of the linear model
summary(model)

# Extract the coefficients to form the linear equation
intercept <- coef(model)[1]
slope <- coef(model)[2]
print(paste("Linear Equation: Log10_RR = ", round(intercept, 3), " + ", round(slope, 3), " * Log10_Median_CW", sep=""))

# Get the R-squared value
r_squared <- summary(model)$r.squared
print(paste("R-squared: ", r_squared))

shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

# Plot the data and the fitted regression line
p <- ggplot(data, aes(x = Log10_Median_CW, y = Log10_RR , colour=Species, shape = Species)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, aes(group = 1) , color="black") +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) + # Use data instead of filtered_data
  scale_fill_manual(values = gray_colors[1:length(unique(data$Species))]) +
  theme_light() +  # Starts with a light theme
  theme(
    text = element_text(family = "Arial"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black",size = 16),  # Adjust text angle for legibility
    axis.text.y = element_text(color="black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = expression(Log[10]~ "(Median Corallite Width)"), y = expression(Log[10]~"(Respiration Rates)"), 
       title="")

# Specify the directory where you want to save the image
output_directory <- "/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Figures"
output_filename <- "Supplementary Figure 7.tiff"

# Combine the directory and filename
output_path <- file.path(output_directory, output_filename)

# Save the plot with the same settings
ggsave(filename = output_path, plot = p, dpi = 300, device = "tiff", type = "cairo")

# Print the plot
print(p)






















 

 