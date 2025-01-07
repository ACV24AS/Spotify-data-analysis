#Loading required libraries
library(tidyverse)
#Loading the dataset
spotify_data<-read.csv("D:\\Introduction to Data Science\\assessment\\dataset.csv")
View(spotify_data)
#Learning about the dataset
summary(spotify_data)
str(spotify_data)
# Check for missing or invalid values in all columns
colSums(is.na(spotify_data))  
# Check for zero values in numeric columns
spotify_data %>%
  summarise_all(~ sum(. == 0))
# Replace invalid values in `duration_ms` with median
spotify_data$duration_ms <- ifelse(spotify_data$duration_ms == 0, 
                                   median(spotify_data$duration_ms[spotify_data$duration_ms > 0], na.rm = TRUE), 
                                   spotify_data$duration_ms)



install.packages("VIM")
library(VIM)
# Replace 0 with NA for missing data
spotify_data$time_signature <- ifelse(spotify_data$time_signature == 0, NA, spotify_data$time_signature)

# Perform KNN imputation
spotify_data <- kNN(spotify_data, variable = "time_signature", k = 5)

# Check for duplicate rows
sum(duplicated(spotify_data))
head(spotify_data)
# Normalize numerical columns
spotify_data <- spotify_data %>%
  mutate(across(c(popularity, duration_ms, tempo, loudness), ~scale(.)))
# Convert character columns to factors
spotify_data$explicit <- as.factor(spotify_data$explicit)
spotify_data$track_genre <- as.factor(spotify_data$track_genre)
# Drop the column 'X'
spotify_data <- spotify_data %>% select(-X)

# Function to detect outliers in a column
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outliers <- x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
  return(outliers)
}

# Apply the function to numeric columns
numeric_columns <- spotify_data %>% select(where(is.numeric))

# Detect outliers for each numeric column
outliers_list <- lapply(numeric_columns, detect_outliers)

# Display the outliers for each column
outliers_list

# Count the number of outliers in each column
outlier_counts <- sapply(outliers_list, length)

# Display counts
outlier_counts

# Select numeric columns
numeric_columns <- spotify_data %>% select(where(is.numeric))

# Create boxplots for each numeric column
library(ggplot2)



# Boxplot for Popularity
ggplot(spotify_data, aes(x = "", y = popularity)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Popularity", x = "", y = "Popularity")

# Boxplot for Duration (in ms)
ggplot(spotify_data, aes(x = "", y = duration_ms)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Duration (ms)", x = "", y = "Duration (ms)")

Q1 <- quantile(spotify_data$duration_ms, 0.25, na.rm = TRUE)
Q3 <- quantile(spotify_data$duration_ms, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Display the bounds
lower_bound
upper_bound

# Filter for songs (reasonable durations)
songs <- spotify_data %>%
  filter(duration_ms >= 120000 & duration_ms <= 600000) # 2 to 10 minutes

# Separate podcasts (longer durations)
podcasts <- spotify_data %>%
  filter(duration_ms > 600000)

# Combine datasets if needed
spotify_data_cleaned <- bind_rows(songs, podcasts)

spotify_data$duration_ms <- ifelse(spotify_data$duration_ms < lower_bound, lower_bound,
                                   ifelse(spotify_data$duration_ms > upper_bound, upper_bound,
                                          spotify_data$duration_ms))


# Boxplot for Danceability
ggplot(spotify_data, aes(x = "", y = danceability)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Danceability", x = "", y = "Danceability")

S# Boxplot for Energy
ggplot(spotify_data, aes(x = "", y = energy)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Energy", x = "", y = "Energy")

# Boxplot for Key
ggplot(spotify_data, aes(x = "", y = key)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Key", x = "", y = "Key")

# Boxplot for Loudness
ggplot(spotify_data, aes(x = "", y = loudness)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Loudness", x = "", y = "Loudness")

# Boxplot for Speechiness
ggplot(spotify_data, aes(x = "", y = speechiness)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Speechiness", x = "", y = "Speechiness")

# Boxplot for Acousticness
ggplot(spotify_data, aes(x = "", y = acousticness)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Acousticness", x = "", y = "Acousticness")

# Boxplot for Instrumentalness
ggplot(spotify_data, aes(x = "", y = instrumentalness)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Instrumentalness", x = "", y = "Instrumentalness")

# Boxplot for Liveness
ggplot(spotify_data, aes(x = "", y = liveness)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Liveness", x = "", y = "Liveness")

# Boxplot for Valence
ggplot(spotify_data, aes(x = "", y = valence)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Valence", x = "", y = "Valence")

# Boxplot for Tempo
ggplot(spotify_data, aes(x = "", y = tempo)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Tempo", x = "", y = "Tempo")

# Boxplot for Time Signature
ggplot(spotify_data, aes(x = "", y = time_signature)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Outlier Detection for Time Signature", x = "", y = "Time Signature")
spotify_data <- spotify_data %>%
  filter(time_signature >= 3 & time_signature <= 7)



