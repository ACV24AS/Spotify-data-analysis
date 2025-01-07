# Load required libraries
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)
library(VIM)

# Step 1: Load the dataset
spotify_data <- read.csv("D:\\Introduction to Data Science\\assessment\\dataset.csv")

# Step 2: Initial exploration
summary(spotify_data)  # Summary statistics
str(spotify_data)      # Structure of the dataset

# Step 3: Check for missing or invalid values
colSums(is.na(spotify_data))  # Check for missing values
spotify_data %>%
  summarise_all(~ sum(. == 0))  # Check for zero values in numeric columns

# Step 4: Handle missing or invalid values
# Replace 0 in `duration_ms` with its median
spotify_data$duration_ms <- ifelse(
  spotify_data$duration_ms == 0,
  median(spotify_data$duration_ms[spotify_data$duration_ms > 0], na.rm = TRUE),
  spotify_data$duration_ms
)

# Replace invalid `time_signature` values (outside 3 to 7) with the mode
spotify_data$time_signature <- ifelse(
  spotify_data$time_signature < 3 | spotify_data$time_signature > 7,
  NA,
  spotify_data$time_signature
)

# Perform KNN imputation for `time_signature`
spotify_data <- kNN(spotify_data, variable = "time_signature", k = 5)

# Step 5: Handle duplicates
sum(duplicated(spotify_data))  # Check for duplicate rows

# Step 6: Handle outliers
# Function to detect and cap outliers
cap_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column <- ifelse(column < lower_bound, lower_bound, column)
  column <- ifelse(column > upper_bound, upper_bound, column)
  return(column)
}

# Apply outlier treatment
spotify_data$loudness <- cap_outliers(spotify_data$loudness)
spotify_data$tempo <- cap_outliers(spotify_data$tempo)
spotify_data$duration_ms <- cap_outliers(spotify_data$duration_ms)

# Step 7: Normalize/Standardize numerical columns
spotify_data <- spotify_data %>%
  mutate(across(
    c(popularity, duration_ms, tempo, loudness),
    ~ scale(.)  # Standardize using z-score normalization
  ))

# Step 8: Convert categorical variables
spotify_data$explicit <- as.factor(spotify_data$explicit)
spotify_data$track_genre <- as.factor(spotify_data$track_genre)

# Step 9: Boxplots for outlier visualization
# Define a helper function for boxplot visualization
plot_boxplot <- function(data, variable, title, ylabel) {
  ggplot(data, aes(x = "", y = .data[[variable]])) +
    geom_boxplot(outlier.color = "red", outlier.size = 2) +
    theme_minimal() +
    labs(title = title, x = "", y = ylabel)
}

# Generate boxplots for numeric columns
plot_boxplot(spotify_data, "popularity", "Outlier Detection for Popularity", "Popularity")
plot_boxplot(spotify_data, "duration_ms", "Outlier Detection for Duration (ms)", "Duration (ms)")
plot_boxplot(spotify_data, "danceability", "Outlier Detection for Danceability", "Danceability")
plot_boxplot(spotify_data, "energy", "Outlier Detection for Energy", "Energy")
plot_boxplot(spotify_data, "key", "Outlier Detection for Key", "Key")
plot_boxplot(spotify_data, "loudness", "Outlier Detection for Loudness", "Loudness")
plot_boxplot(spotify_data, "speechiness", "Outlier Detection for Speechiness", "Speechiness")
plot_boxplot(spotify_data, "acousticness", "Outlier Detection for Acousticness", "Acousticness")
plot_boxplot(spotify_data, "instrumentalness", "Outlier Detection for Instrumentalness", "Instrumentalness")
plot_boxplot(spotify_data, "liveness", "Outlier Detection for Liveness", "Liveness")
plot_boxplot(spotify_data, "valence", "Outlier Detection for Valence", "Valence")
plot_boxplot(spotify_data, "tempo", "Outlier Detection for Tempo", "Tempo")
plot_boxplot(spotify_data, "time_signature", "Outlier Detection for Time Signature", "Time Signature")

# Drop the column 'X' if it exists
spotify_data <- spotify_data %>% select(-X)

# Verify the column has been dropped
colnames(spotify_data)


# Step 10: Final Review
head(spotify_data)  # Preview cleaned data

# Basic statistics
summary(spotify_data)

# Check for correlations
correlations <- cor(spotify_data[, sapply(spotify_data, is.numeric)]) # Numeric columns only
print(correlations)

library(ggcorrplot)

# Compute correlation matrix
correlation_matrix <- cor(spotify_data[, sapply(spotify_data, is.numeric)])

# Plot heatmap
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           title = "Correlation Heatmap of Spotify Features")

# Linear Regression Model
# Select only numeric columns and remove NAs
spotify_numeric <- spotify_data[, sapply(spotify_data, is.numeric)]
spotify_numeric <- na.omit(spotify_numeric)  # Remove rows with NA values
install.packages("caret")
library(caret)
set.seed(42)  # For reproducibility

# Create training and testing sets
train_index <- createDataPartition(spotify_numeric$popularity, p = 0.8, list = FALSE)
train_data <- spotify_numeric[train_index, ]
test_data <- spotify_numeric[-train_index, ]
# Train Linear Regression Model
lm_model <- lm(popularity ~ ., data = train_data)

# View model summary (coefficients, p-values, R^2)
summary(lm_model)
# Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
library(Metrics)

# Calculate R-squared
r_squared <- cor(test_data$popularity, predictions)^2

# Calculate RMSE
rmse_value <- rmse(test_data$popularity, predictions)

# Print evaluation metrics
cat("R-squared:", r_squared, "\n")
cat("RMSE:", rmse_value, "\n")


install.packages("randomForest")
library(randomForest)
# Fit the Random Forest Model
set.seed(42)  # For reproducibility
rf_model <- randomForest(popularity ~ ., data = train_data, ntree = 500, importance = TRUE)

# View the model summary
print(rf_model)
# Get feature importance
importance_rf <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[, "%IncMSE"])

# Sort by importance
importance_df <- importance_df[order(-importance_df$Importance), ]
print(importance_df)
# Bar Plot of Feature Importance
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Features", y = "Importance")


# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate Random Forest
library(Metrics)
r_squared_rf <- cor(test_data$popularity, rf_predictions)^2
rmse_rf <- rmse(test_data$popularity, rf_predictions)

# Print metrics
cat("Random Forest - R-squared:", r_squared_rf, "\n")
cat("Random Forest - RMSE:", rmse_rf, "\n")
