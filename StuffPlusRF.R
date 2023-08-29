setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Data/Statcast Data")

# Load Required Libraries
library(tidyverse)
library(randomForest)
# install.packages("caTools")
library(caTools)

# Read Data (might take several seconds)
data <- read.csv('Statcast_2022.csv')

# Filter for Relevant Pitch Types
pitch_types <- c('4-Seam Fastball', 'Sinker', 'Cutter', 'Slider', 'Curveball', 'Changeup', 'Split-Finger')
data_filtered <- data %>% filter(pitch_name %in% pitch_types)

# Define Pitch Groups
data_filtered$pitch_group <- ifelse(data_filtered$pitch_name %in% c('4-Seam Fastball', 'Sinker'), 'Fastballs',
                             ifelse(data_filtered$pitch_name %in% c('Cutter', 'Slider', 'Curveball'), 'Breaking Balls', 'Offspeed'))
data_filtered$pitch_group <- as.factor(data_filtered$pitch_group)

data_filtered$p_throws <- ifelse(data_filtered$p_throws=="R", 1, 0)
data_filtered$stand <- ifelse(data_filtered$stand=="R", 1, 0)
# Derive Arm Angle
# data_filtered$arm_angle <- atan2(data_filtered$release_pos_z, data_filtered$release_pos_x)

# Define Features and Target Variable
features <- c('p_throws', 'stand', 'release_speed', 'pfx_z', 'pfx_x', 'release_pos_x', 'release_pos_z', 'release_extension')
target <- 'delta_run_exp'

# Remove Rows with Missing Values in Selected Features and Target Variable
data_filtered <- data_filtered %>% 
  select(all_of(features), all_of(target), pitch_group) %>% 
  mutate(p_throws = as.factor(p_throws),
         stand = as.factor(stand)) %>% 
  drop_na()

summary(data_filtered)

# Scale Numeric Features 
data_scaled <- data_filtered %>% mutate(across(where(is.numeric), scale))

summary(data_scaled)

# Train Random Forest Model for Each Pitch Group
pitch_groups <- unique(data_filtered$pitch_group)
models <- list()

for (group in pitch_groups) {
  group_data <- data_filtered %>% filter(pitch_group == group)
  
  # Split Data into Training and Testing Sets
  set.seed(42)
  split <- sample.split(group_data[, target], SplitRatio = 0.8)
  train_data <- subset(group_data, split == TRUE)
  test_data <- subset(group_data, split == FALSE)
  
  # Train Random Forest Model
  model <- randomForest(as.formula(paste(target, paste(features, collapse = ' + '), sep = ' ~ ')), data = train_data)
  
  # Predict on Test Data
  predictions <- predict(model, test_data)
  
  # Evaluate Model
  mse <- mean((test_data[, target] - predictions)^2)
  cat('Mean Squared Error for', group, 'group:', mse, '\n')
  
  # Store Model
  models[[group]] <- model
}

# If needed, here's the function to train the model for a certain group 
train_model <- function(group) {

  group_data <- data_filtered %>% filter(pitch_group == group)
  
  # Split Data into Training and Testing Sets
  set.seed(42)
  split <- sample.split(group_data[, target], SplitRatio = 0.8)
  train_data <- subset(group_data, split == TRUE)
  test_data <- subset(group_data, split == FALSE)
  
  # Train Random Forest Model
  model <- randomForest(as.formula(paste(target, paste(features, collapse = ' + '), sep = ' ~ ')), data = train_data)
  
  # Predict on Test Data
  predictions <- predict(model, test_data)
  
  # Evaluate Model
  mse <- mean((test_data[, target] - predictions)^2)
  cat('Mean Squared Error for', group, 'group:', mse, '\n')
  
  # Store Model
  models[[group]] <- model
}
train_model("Fastballs")
# The trained Random Forest models are stored in the 'models' list
models$`Breaking Balls`$importance
