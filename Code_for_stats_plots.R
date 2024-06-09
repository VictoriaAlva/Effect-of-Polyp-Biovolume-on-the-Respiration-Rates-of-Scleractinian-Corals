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
data <- read_excel("/Users/alvagajv/Library/CloudStorage/OneDrive-KAUST/My Publications/Effects of Biovolume/Data_sheet.xlsx")

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
sample_mean <- 1.2091 # Your observed mean
sem <- 0.3110  # Standard error of the mean
n <- 12  # Sample size

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

###### "Respiration rates vs Biovolume" ######

# Log10 of my data
data$log_biovolume <- log10(data$Average_Biovolume)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

# Linear regression
linear_model <- lm(log_respiration_rates ~ log_biovolume, data = data)
summary (linear_model)

# Calculate mean biovolume per Species
Species_means <- data %>%
  group_by(Species) %>%
  summarise(MeanBiovolume = mean(Random_Biovolume, na.rm = TRUE)) %>%
  arrange(desc(MeanBiovolume)) %>%
  ungroup()

# Create an ordered factor for the Species based on mean biovolume
data$Species <- factor(data$Species, levels = Species_means$Species)

data_summary <- data %>%
  group_by(Species) %>%  # or Species, depending on your data structure
  summarise(
    avg_log_biovolume = mean(log(Average_Biovolume), na.rm = TRUE),
    se_log_biovolume = sd(log(Calculated_Fragment_Biovolume), na.rm = TRUE) / sqrt(n()),
    avg_log_respiration = mean(log(Respiration_per_polyp), na.rm = TRUE),
    se_log_respiration = sd(log(Respiration_per_polyp), na.rm = TRUE) / sqrt(n())
  )

# Define a set of 13 easily distinguishable shapes
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

ggplot(data, aes(x=log_biovolume, y=log_respiration_rates, colour=Species, shape = Species)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, aes(group = 1), color="black") +
  scale_shape_manual(values = shape_values[1:length(unique(data$Species))]) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), # or use element_rect(fill = "white") for a white background
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"), # Keep the axis lines
    axis.text.x = element_text(color="black", size = 16),
    axis.text.y = element_text(color="black", size = 16), 
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18) # Y-axis title size
  ) +
  labs(x = "Log10 (Biovolume)", y = "Log10 (Respiration Rates)", 
       title="")

###### "Respiration rates vs Biovolume SE" ######

# Log10 of my data
data$log_biovolume <- log10(data$Average_Biovolume)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

# Linear regression
linear_model <- lm(log_respiration_rates ~ log_biovolume, data = data)
summary (linear_model)

# Calculate mean biovolume per Species
Species_means <- data %>%
  group_by(Species) %>%
  summarise(MeanBiovolume = mean(Random_Biovolume, na.rm = TRUE)) %>%
  arrange(desc(MeanBiovolume)) %>%
  ungroup()

# Create an ordered factor for the Species based on mean biovolume
data$Species <- factor(data$Species, levels = Species_means$Species)

data_summary <- data %>%
  group_by(Species) %>%  # or Species, depending on your data structure
  summarise(
    avg_log_biovolume = mean(log(Average_Biovolume), na.rm = TRUE),
    se_log_biovolume = sd(log(Calculated_Fragment_Biovolume), na.rm = TRUE) / sqrt(n()),
    avg_log_respiration = mean(log(Respiration_per_polyp), na.rm = TRUE),
    se_log_respiration = sd(log(Respiration_per_polyp), na.rm = TRUE) / sqrt(n())
  )

# Define a set of 13 easily distinguishable shapes
shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

ggplot(data_summary, aes(x = avg_log_biovolume, y = avg_log_respiration, colour = Species, shape = Species)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, aes(group = 1), color="black") +
  geom_errorbar(aes(ymin = avg_log_respiration - se_log_respiration, ymax = avg_log_respiration + se_log_respiration), width = .1) +
  geom_errorbarh(aes(xmin = avg_log_biovolume - se_log_biovolume, xmax = avg_log_biovolume + se_log_biovolume), height = .1) +
  scale_shape_manual(values = shape_values[1:length(unique(filtered_data$Species))]) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.text = element_text(face = "italic", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(color="black", size = 16),
    axis.text.y = element_text(color="black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18), # Y-axis title size
    axis.ticks = element_line(color = "black")
  ) +
  labs(x = "Log10 (Biovolume)", y = "Log10 (Respiration Rates)", title = "")

###### "Respiration rates vs Species" ######

ggplot(data, aes(x = Species, y = Respiration_per_polyp, colour = Species)) +
  geom_boxplot() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none", size = 16,
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "italic"),
    axis.text.y = element_text(color="black", size = 16), 
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18), # Y-axis title size
    panel.background = element_rect(fill = "white"), # Set the panel background to white
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(fill = NA, color = "black"), # Keep the border lines
    axis.line = element_line(color = "black") # Keep the axis lines
  ) +
  labs(x = "Species", y = expression(Respiration~Rates~(umol * h^-1 * polyp^-1)), title = "")

###### "Biovolume vs Species" ######

ggplot(data, aes(x = Species, y = Random_Biovolume, colour = Species)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  theme_light() +  
  theme(
    text = element_text(family = "Times New Roman"),
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

###### "Respiration rates vs Surface Area" ######
# Log10 of my data
data$log_SA <- log10(data$Surface_area_per_polyp)
data$log_respiration_rates <- log10(data$Respiration_per_polyp)

model <- lm(log_respiration_rates ~ log_SA, data=data)

shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

ggplot(data, aes(x=log_SA, y=log_respiration_rates, colour=Species, size = Surface_area_per_polyp, shape = Species)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, aes(group = 1) , color="black") +
  scale_shape_manual(values = shape_values[1:length(unique(filtered_data$Species))]) +
  scale_size_continuous(breaks = c(1, 10, 50, 100, 150),
                        labels = c("1", "10", "50", "100", "150"),
                        range = c(1, 5)) +
  theme_minimal() +  # Starts with a light theme
  theme(
    text = element_text(family = "Times New Roman"),
    legend.text = element_text(face = "italic", size = 16),
    legend.title = element_text(size = 16),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 16),
    axis.text.y = element_text(color="black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = "Log10 (Surface Area)", y = "Log10 (Respiration Rates)", 
       colour = "Species", size = expression(Surface~Area~(cm^2)))

###### "Surface Area vs Biovolume" ######

data$log_Biovolume <- log10(data$Average_Biovolume)
data$log_SA <- log10(data$Average_Surface_Area)

model <- lm(log_SA ~ log_Biovolume, data=data)

shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 3, 4, 8)

ggplot(data, aes(x=log_SA, y=log_Biovolume, colour=Species, shape = Species)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, aes(group = 1) , color="black") +
  scale_shape_manual(values = shape_values[1:length(unique(filtered_data$Species))]) +
  theme_light() +  # Starts with a light theme
  theme(
    text = element_text(family = "Times New Roman"),
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
  labs(x = "Log10 (Surface Area)", y = "Log10 (Biovolume)", 
       title="")

###### "Predicted vs Exprimental" ######

ggplot(data, aes(x = Log10_RR, y = Predicted_RR, size = Average_Biovolume)) +
  geom_point(shape = 21, fill = "black") +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black") +
  scale_size_continuous(breaks = c(10, 100, 150, 300, 500, 1000),
                        labels = c("10", "100", "150", "300", "500", "1,000"),
                        range = c(1, 10)) +
  theme_light() + 
  theme(
    text = element_text(family = "Times New Roman"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = NA),  # Set background to white, remove border
    panel.border = element_rect(fill = NA, color = "black"),
    axis.line = element_line(colour = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black", size = 16),  # Adjust text angle for legibility
    axis.text.y = element_text(color="black", size = 16),
    axis.title.x = element_text(size = 18), # X-axis title size
    axis.title.y = element_text(size = 18)
  ) +
  labs(x = "Log10 (Experimental Respiration Rates)", y = "Log10 (Predicted Respiration Rates)", 
       size = expression(Biovolume ~ (mm^3))) +
  guides(size = guide_legend(override.aes = list(shape = 21, fill = "white", color = "black")))
