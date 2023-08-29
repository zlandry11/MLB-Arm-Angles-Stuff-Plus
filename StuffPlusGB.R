setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Data/Statcast Data")

# Load necessary libraries
# install.packages("gbm")
library(gbm)
library(tidyverse)
install.packages("xgboost")
library(xgboost)

# Read the data
data <- read.csv("Statcast_2022.csv")

# Filter Fastballs (4-Seam Fastball), Remove missing and infinity values
fastballs_data <- data %>% 
  filter(pitch_type %in% c("FF"), type %in% c("B", "S", "X")) %>% 
  select(player_name, p_throws, release_speed, pfx_x, pfx_z, release_pos_x, release_pos_z, release_extension, delta_run_exp) %>% 
  drop_na()

pitcher_data <- fastballs_data %>%
  group_by(player_name) %>%
  summarise(
    n = n(),
    p_throws = first(p_throws), # Assuming p_throws is the same for each pitcher
    release_speed = mean(release_speed),
    pfx_x = mean(pfx_x),
    pfx_z = mean(pfx_z),
    release_pos_x = mean(release_pos_x),
    release_pos_z = mean(release_pos_z),
    release_extension = mean(release_extension),
    run_value = sum(delta_run_exp)
  ) %>% 
  filter(n > 100) %>% 
  mutate(rv100 = (run_value / n)*100)


# arm angle calculation using tangent of release_pos_z divided by release_pos_x
# fastballs_data$arm_angle <- (180*atan(fastballs_data$release_pos_z / fastballs_data$release_pos_x)) / pi

# Split by handedness
rh_pitchers <- pitcher_data %>% filter(p_throws == "R")
lh_pitchers <- pitcher_data %>% filter(p_throws == "L")

# Fit a linear model
lm_model <- lm(rv100 ~ release_speed + pfx_x + pfx_z + release_pos_x + release_pos_z + release_extension, data = rh_pitchers)
summary(lm_model)

# Select relevant features and target
features <- c('release_speed', 'pfx_x','pfx_z', 'release_pos_x', 'release_pos_z', 'release_extension')
target <- 'rv100'

# Scale the feature and target variables
rh_pitchers[features] <- scale(rh_pitchers[features])
rh_pitchers[target] <- scale(rh_pitchers[target])

lh_pitchers[features] <- scale(lh_pitchers[features])
lh_pitchers[target] <- scale(lh_pitchers[target])

summary(rh_pitchers)
hist(rh_pitchers$rv100)

# Split training and testing data for right-handed pitchers
set.seed(42)
train_index <- sample(1:nrow(rh_pitchers), 0.8 * nrow(rh_pitchers))
train_rh <- rh_pitchers[train_index, ]
test_rh <- rh_pitchers[-train_index, ]

# Convert the training features and target to matrices
train_features <- as.matrix(train_rh[features])
train_target <- as.vector(train_rh$rv100)
test_features <- as.matrix(test_rh[features])
test_target <- as.vector(test_rh$rv100)

# Set parameters for the XGBoost model
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  eval_metric = "rmse"
)

# Train the XGBoost model
xgb_train <- xgb.DMatrix(data = train_features, label = train_target)
xgb_model <- xgb.train(params = params, data = xgb_train, nrounds = 100)

# Predict on the test data
xgb_test <- xgb.DMatrix(data = test_features)
predictions <- predict(xgb_model, xgb_test)

# Evaluate the model using R-squared
actual <- test_target
r_squared <- 1 - (sum((actual - predictions)^2) / sum((actual - mean(actual))^2))
print(paste("R-squared value:", r_squared))

plot(actual, predictions)

### GRADIENT BOOSTING
# 
# # Split training and testing data for right-handed pitchers
# set.seed(42)
# train_index <- sample(1:nrow(rh_pitchers), 0.8 * nrow(rh_pitchers))
# train_rh <- rh_pitchers[train_index, ]
# test_rh <- rh_pitchers[-train_index, ]
# 
# # Train Gradient Boosting Model for right-handed pitchers
# gbm_model_rh <- gbm(
#   formula = as.formula(paste(target, "~", paste(features, collapse = " + "))),
#   distribution = "gaussian",
#   data = train_rh,
#   n.trees = 100,
#   interaction.depth = 3
# )
# 
# # Predict and evaluate on test data for right-handed pitchers
# pred_rh <- predict(gbm_model_rh, test_rh, n.trees = 100)
# mse_rh <- mean((test_rh[[target]] - pred_rh)^2)
# 
# # Print the mean squared error for right-handed pitchers
# print(paste("MSE for Right-Handed Pitchers:", mse_rh))
# 
# # Scatter Plot of True vs. Predicted Values
# ggplot(test_rh, aes(x = delta_run_exp, y = pred_rh)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = 'lm', color = 'red', linetype = 'dashed') +
#   labs(x = 'True Values', y = 'Predicted Values', title = 'True vs Predicted Values for Right-Handed Pitchers') +
#   theme_minimal()
# 
# # Calculate the total sum of squares
# tss <- sum((test_rh$delta_run_exp - mean(test_rh$delta_run_exp))^2)
# 
# # Calculate the residual sum of squares
# rss <- sum((test_rh$delta_run_exp - pred_rh)^2)
# 
# # Calculate the R-squared value
# r_squared <- 1 - (rss / tss)
# 
# # Feature Importance
# importance_rh <- summary(gbm_model_rh)
# importance_df <- data.frame(Feature = rownames(importance_rh), Importance = importance_rh[, 1])
# ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
#   geom_bar(stat = 'identity') +
#   coord_flip() +
#   labs(x = 'Feature', y = 'Importance', title = 'Feature Importance for Right-Handed Pitchers') +
#   theme_minimal()
# 
