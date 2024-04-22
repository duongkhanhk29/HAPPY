# Load necessary libraries
library(readxl)       # For reading Excel files
library(recipes)      # For data preprocessing
library(summarytools) # For generating summary statistics
library(dplyr)        # For data manipulation
library(grf)          # For Generalized Random Forest
library(ggplot2)      # For data visualization
library(gridExtra)    # For arranging plots
library(DiagrammeR)   # For plotting a tree

# Load the raw data set
data_raw <- read_excel("Research_data.xlsx")

# Convert specified variables to factors
factor_vars <- c("Country", "Code", "Year", "culture_cluster")
data_raw[factor_vars] <- lapply(data_raw[factor_vars], as.factor)


##### DATA CLEANING
# Create a recipe to clean the data by removing rows with missing values
recipe <- recipe(Happy ~ ., data = data_raw) %>%
  step_naomit(all_predictors())

# Apply the recipe to clean the data
data_cleaned <- recipe %>%
  prep() %>%
  bake(new_data = NULL)

# Display summary statistics of the cleaned data
view(dfSummary(data_cleaned, 
               plain.ascii = FALSE, 
               style = "grid", 
               graph.magnif = 0.75, 
               graph.col = FALSE,
               valid.col = FALSE))

# Calculate summary statistics for happiness by cultural groups
stats <- by(data = data_cleaned$Happy, INDICES = data_cleaned$culture_cluster, FUN = descr)
view(stats, style = "grid")

##### GROUPING VARIABLES AND SAMLE WEIGHTS
data <- data_cleaned

cluster_size <- table(data$culture_cluster)
data$weight <- 1 / cluster_size[data$culture_cluster]
summary_data <- data %>%
  group_by(culture_cluster) %>%
  summarise(n_obs = n(), n_countries = n_distinct(Country), total_weight = sum(weight))
write.csv(summary_data, file = "summary_data.csv")

outcome <- "Happy"
policies <- c("Support", "Health", "Freedom", "Generosity", "Corruption")
moods <- c("Positive", "Negative")
covariates <- c("Income", "Support", "Health", "Freedom", "Generosity", "Corruption",
                "Positive", "Negative", "UA", "FO", "PD", "IC1", "HO", "PO", "IC2", "GE", "AS")
culture <- c("UA", "FO", "PD", "IC1", "HO", "PO", "IC2", "GE", "AS")



##### CORRELATION NETWORK
library(qgraph)
dt_adj <- data_cleaned[c(outcome,covariates)]
colors <- ifelse(names(dt_adj) %in% outcome, "pink",
                 ifelse(names(dt_adj) %in% culture, "darkslategray3",
                        ifelse(names(dt_adj) %in% moods,"darksalmon","darkseagreen")))
qgraph(
  abs(cor(dt_adj)), # Compute the absolute value of the correlation of dt_adj
  node.width = 1, # Set the width of the nodes
  labels = names(dt_adj), vsize = 8, # Set the labels
  layout = "spring", # Use the 'spring' layout
  repulsion = 0.6, # Set the repulsion parameter to 0.6
  threshold = 0.5, # Only display correlations greater than 0.5
  color = colors # Set the node colors
)


##### GENERALIZED RANDOM FOREST
xx <- as.matrix(data[covariates])
yy <- as.matrix(data[outcome])
cluster_code <- as.numeric(data$culture_cluster)

set.seed(407) # Required to replicate sample split
rf <- regression_forest(xx, yy, num.trees = 5000, 
                        clusters = cluster_code,
                        sample.weights = data$weight,
                        tune.parameters = "all")
test_calibration(rf) # Evaluate the calibration of the model


##### WEIHGTED MEAN BY CLUSTER
weighted_means <- list()
for (X in covariates) {
  # Calculate the weighted mean for each covariate
  weighted_mean_X <- sum(data[[X]] * data$weight) / sum(data$weight)
  # Store the weighted mean for each covariate in a list
  weighted_means[[X]] <- weighted_mean_X
}
# Print the weighted means
print(weighted_means)


##### PREDICT NEW HAPPINES VALUES
# Replace original variables with their respective weighted means
data_copy <- data[covariates]
for (X in covariates) {
  # Replace original variable values with their respective weighted means
  data_copy[[X]] <- weighted_means[[X]]
}

# Predict happiness using the fitted model
Happy_predicted <- list()
# Predict happiness using the mean values of covariates
Happy_predicted[["H_mean"]] <- predict(rf, data_copy)$predictions

# Predict happiness using income-adjusted values
data_copy[["Income"]] <- data[["Income"]]
Happy_predicted[["H_income"]] <- predict(rf, data_copy)$predictions

# Predict happiness using income-adjusted values with constrained moods
data_copy <- data[covariates]
for (X in moods) {
  # Replace moods with their respective weighted means
  data_copy[[X]] <- weighted_means[[X]]
}
Happy_predicted[["H_income_policies_culture"]] <- predict(rf, data_copy)$predictions

# Predict happiness using income-adjusted values and culture
Predict_income_happiness_1 <- function(variables) {
  data_copy <- data[covariates]
  for (X in covariates) {
    # Replace covariates with their respective weighted means
    data_copy[[X]] <- weighted_means[[X]]
  }
  # Add income variable
  data_copy[["Income"]] <- data[["Income"]]
  for (i in variables) {
    # Add additional variables
    data_copy[[i]] <- data[[i]]
  }
  # Predict happiness using the specified variables
  predict(rf, data_copy)$predictions
}
# Predict happiness using income-adjusted values and culture
Happy_predicted[["H_income_culture"]] <- Predict_income_happiness_1(culture)
# Predict happiness using income-adjusted values and moods
Happy_predicted[["H_income_moods"]] <- Predict_income_happiness_1(moods)
# Predict happiness using income-adjusted values and policies
Happy_predicted[["H_income_policies"]] <- Predict_income_happiness_1(policies)

# Create a data frame with predicted happiness values
Happy_predicted_df <- as.data.frame(do.call(cbind, Happy_predicted))
# Add real values of happiness, income, code, and weight to the dataframe
Happy_predicted_df$H_real <- data$Happy
Happy_predicted_df$Income <- data$Income
Happy_predicted_df$Code <- data$Code
Happy_predicted_df$weight <- data$weight
# Write the data frame to a CSV file
write.csv(Happy_predicted_df, "Happy_predicted_df.csv", row.names = FALSE)


##### DEVIATION with income-adjusted happiness
calculate_deviation <- function(variable) {
  # Calculate deviation between predicted and income-adjusted happiness
  sqrt(sum((Happy_predicted_df[[paste0("H_income_", variable)]] - Happy_predicted_df$H_income)^2 * Happy_predicted_df$weight / sum(Happy_predicted_df$weight)))
}

# Calculate deviation for different variables
deviation_culture <- calculate_deviation("culture")
deviation_policies <- calculate_deviation("policies")
deviation_moods <- calculate_deviation("moods")
deviation_policies_culture <- calculate_deviation("policies_culture")
deviation_real <- sqrt(sum((Happy_predicted_df[["H_real"]] - Happy_predicted_df$H_income)^2 * Happy_predicted_df$weight / sum(Happy_predicted_df$weight)))


##### PLOTTING RESULTS
# Plot the true relationship between income and happiness
ggplot(data = Happy_predicted_df) + 
  theme_classic() +
  # Plot real values of happiness
  geom_line(aes(x = Income, y = H_real, color = "Real values")) +
  # Plot mean values of happiness
  geom_line(aes(x = Income, y = H_mean, color = "Mean values")) +
  # Plot income-adjusted values of happiness
  geom_line(aes(x = Income, y = H_income, color = "Income-adjusted values")) +
  # Add a vertical line
  geom_vline(aes(xintercept = 11), linetype="dashed", color = "red") +
  labs(x = "Income", y = "Happiness") + 
  theme(legend.position="top") +
  scale_color_manual(name = "",
                     values = c("Real values" = "grey", 
                                "Income-adjusted values" = "black", 
                                "Mean values" = "red"))
# Print mean, minimum, and maximum values of mean happiness
print(mean(Happy_predicted_df$H_mean))
print(min(Happy_predicted_df$H_income))
print(max(Happy_predicted_df$H_income))

# The effects of culture, moods, and policies

# Define a function to generate the individual plots
generate_plot <- function(variable, title, color) {
  ggplot(data = Happy_predicted_df, aes(x = Income)) +
    geom_line(aes_string(y = variable), color = color) +
    geom_line(aes(y = H_income), color = "black") +
    labs(x = "Income", y = "Happiness") + 
    theme_classic() + ggtitle(title) + ylim(4.7, 6.9)
}

# Create plots using the function
plot_culture <- generate_plot("H_income_culture", "B. Culture", "darkslategray3")
plot_policies <- generate_plot("H_income_policies", "C. Policies", "darkseagreen")
plot_moods <- generate_plot("H_income_moods", "D. Moods", "darksalmon")
plot_real <- generate_plot("H_real", "Policies x Culture x Moods", "grey")
plot_policies_culture <- generate_plot("H_income_policies_culture", "Policies x Culture", "cyan2")

# Add deviation to the plot titles
plot_culture <- plot_culture + 
  labs(subtitle = paste("Deviation:", round(deviation_culture, 3)))
plot_policies <- plot_policies + labs(subtitle = paste("Deviation:", round(deviation_policies, 3)))
plot_moods <- plot_moods + labs(subtitle = paste("Deviation:", round(deviation_moods, 3)))
plot_real <- plot_real + labs(subtitle = paste("Deviation:", round(deviation_real, 3)))
plot_policies_culture <- plot_policies_culture + labs(subtitle = paste("Deviation:", round(deviation_policies_culture, 3)))

##### FEATURE IMPORTANCE
# Create a data frame containing variable names and their importance scores
imp <- data.frame(variable = colnames(xx), importance = variable_importance(rf))


# Create grouping variable based on column names in xx
imp$grouping <- ifelse(colnames(xx) %in% culture, "Culture",
                       ifelse(colnames(xx) %in% moods, "Moods",
                              ifelse(colnames(xx) %in% policies, "Policies", "Income")))

# Plot the importance of variables
plot_imp<-ggplot(imp, aes(x = grouping, y = importance, fill = grouping)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend
  theme_classic() +
  labs(title = "A. Importance", x = "", y = "Importance", subtitle = "Times of split x depth of trees") +
  scale_fill_manual(values = c("darkslategray3", "pink", "darksalmon", "darkseagreen"))

grid.arrange(plot_imp, plot_culture, plot_policies, plot_moods, ncol = 2)





