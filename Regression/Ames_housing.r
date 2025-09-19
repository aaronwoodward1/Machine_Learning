# Ames housing analysis
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RKaggle)
library(tidyverse)
library(modeldata)
library(GGally)
library(ggcorrplot)
library(paletteer)

# Load the Ames housing dataset
ames_df <- modeldata::ames

# Explore the dataset
glimpse(ames_df)
summary(ames_df)
colnames(ames_df)

# Add index column to give unique identifier for observations
ames_df <- cbind(Index = 1:nrow(ames_df), ames_df)
glimpse(ames_df)

# Visualization Sale Price Distribution
# Histogram of Sale Price
hist(ames_df$Sale_Price,
    main = "Histogram of Sale Price", 
    xlab = "Sale Price",
    ylab = "Frequency",
    col = "#66eac9",
    border = "black"
)
# Boxplot of Sale Price
boxplot(ames_df$Sale_Price,
    main = "Boxplot of Sale Price",
    ylab = "Sale Price",
    col = "#ff9999",
    border = "black"
)

# Handling numerical variables
# Select relevant numerical variables
df1 <- ames_df |>
    select(Index, Sale_Price, Gr_Liv_Area, Lot_Area, Bedroom_AbvGr, Full_Bath, Garage_Cars)
glimpse(df1)

# Pair plot and correlation matrix
ggpairs(df1)

corr_matrix <- round(cor(df1),1)
ggcorrplot(corr_matrix, method = "square", hc.order =TRUE, 
           type ="lower", lab =TRUE)

# Handling categorical variable
# Select relevant categorical variables
df2 <- ames_df |>
    select(Index, Sale_Price, Neighborhood, Bldg_Type, House_Style, Central_Air, Overall_Cond)
glimpse(df2)

# Convert categorical variables to factors
df2$Neighborhood <- as.factor(df2$Neighborhood)
df2$Bldg_Type <- as.factor(df2$Bldg_Type)
df2$House_Style <- as.factor(df2$House_Style)
df2$Central_Air <- as.factor(df2$Central_Air)
df2$Overall_Cond <- as.factor(df2$Overall_Cond)
summary(df2)

# Boxplot of Sale Price by Neighborhood
ggplot(df2, aes(x = Neighborhood, y = Sale_Price, fill = Neighborhood)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Boxplot of Sale Price by Neighborhood", x = "Neighborhood", y = "Sale Price") +
    scale_fill_paletteer_d("MetBrewer::Signac")

# Merging categorical and numerical variables
df <- full_join(df1,df2, by="Index")
glimpse(df)
df <- select(df, -Sale_Price.y)
df <- rename(df, Sale_Price = Sale_Price.x)
glimpse(df)

# Assessing missing values
library(naniar)
gg_miss_upset(df)
# Visualizing missing values
vis_miss(df)

# No missing values, time to build model
# Do train-test split
set.seed(123)
train_indices <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]


# Linear regression model
## Train the model
linmod <- lm(Sale_Price ~ Gr_Liv_Area + Lot_Area + Bedroom_AbvGr + Full_Bath + Garage_Cars +
                Neighborhood + Bldg_Type + House_Style + Central_Air + Overall_Cond, data = train_data)

## Predict on test data
predictions <- predict(linmod, newdata = test_data)

## Evaluate the model
mse <- mean((test_data$Sale_Price - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$Sale_Price - predictions))
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
summary(linmod)
# Plot actual vs predicted
ggplot(data = test_data, aes(x = Sale_Price, y = predictions)) +
    geom_point(color = "#66eac9") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Actual vs Predicted Sale Price",
         x = "Actual Sale Price",
         y = "Predicted Sale Price") +
    theme_minimal()
# Save the model
saveRDS(linmod, file = "ames_housing_model.rds")

# Test linmod for overfitting
train_predictions <- predict(linmod, newdata = train_data)
train_mse <- mean((train_data$Sale_Price - train_predictions)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(train_data$Sale_Price - train_predictions))

# Compare train and test metrics
cat("Train MSE:", train_mse, "\n")
cat("Train RMSE:", train_rmse, "\n")
cat("Train MAE:", train_mae, "\n")

cat("Test MSE:", mse, "\n")
cat("Test RMSE:", rmse, "\n")
cat("Test MAE:", mae, "\n")

# Comparing r-squared values
train_r_squared <- summary(linmod)$r.squared
test_r_squared <- 1 - sum((test_data$Sale_Price - predictions)^2) / sum((test_data$Sale_Price - mean(test_data$Sale_Price))^2)
cat("Train R-squared:", train_r_squared, "\n")
cat("Test R-squared:", test_r_squared, "\n")

# If train and test metrics are similar, the model is not overfitting
# If train metrics are significantly better than test metrics, the model may be overfitting
# In this case, the model shows similar performance on both train and test sets, indicating it is not overfitting

# Example of saving a plot
ggplot(df1, aes(x = Gr_Liv_Area, y = Sale_Price)) +
    geom_point(color = "#66eac9") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Sale Price vs. Above Grade Living Area",
         x = "Above Grade Living Area (sq ft)",
         y = "Sale Price") +
    theme_minimal()
ggsave("sale_price_vs_living_area.png")

# Random forest model
library(randomForest)
set.seed(123)
rf_model <- randomForest(Sale_Price ~ Gr_Liv_Area + Lot_Area + Bedroom_AbvGr + Full_Bath + Garage_Cars +
                             Neighborhood + Bldg_Type + House_Style + Central_Air + Overall_Cond, data = train_data, ntree = 100)
rf_predictions <- predict(rf_model, newdata = test_data)
rf_mse <- mean((test_data$Sale_Price - rf_predictions)^2)
rf_rmse <- sqrt(rf_mse)
rf_mae <- mean(abs(test_data$Sale_Price - rf_predictions))
cat("Random Forest MSE:", rf_mse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest MAE:", rf_mae, "\n")
summary(rf_model)

# Plot actual vs predicted for random forest
ggplot(data = test_data, aes(x = Sale_Price, y = rf_predictions)) +
    geom_point(color = "#66eac9") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Actual vs Predicted Sale Price (Random Forest)",
         x = "Actual Sale Price",
         y = "Predicted Sale Price") +
    theme_minimal()

# Save the random forest model
saveRDS(rf_model, file = "ames_housing_rf_model.rds")

# Overfitting check for random forest
train_rf_predictions <- predict(rf_model, newdata = train_data)
train_rf_mse <- mean((train_data$Sale_Price - train_rf_predictions)^2)
train_rf_rmse <- sqrt(train_rf_mse)
train_rf_mae <- mean(abs(train_data$Sale_Price - train_rf_predictions))
cat("Train Random Forest MSE:", train_rf_mse, "\n")
cat("Train Random Forest RMSE:", train_rf_rmse, "\n")
cat("Train Random Forest MAE:", train_rf_mae, "\n")

# Compare train and test metrics for random forest
cat("Test Random Forest MSE:", rf_mse, "\n")
cat("Test Random Forest RMSE:", rf_rmse, "\n")
cat("Test Random Forest MAE:", rf_mae, "\n")

# Comparing r-squared values for random forest
train_rf_r_squared <- 1 - sum((train_data$Sale_Price - train_rf_predictions)^2) / sum((train_data$Sale_Price - mean(train_data$Sale_Price))^2)
test_rf_r_squared <- 1 - sum((test_data$Sale_Price - rf_predictions)^2) / sum((test_data$Sale_Price - mean(test_data$Sale_Price))^2)
cat("Train Random Forest R-squared:", train_rf_r_squared, "\n")
cat("Test Random Forest R-squared:", test_rf_r_squared, "\n")
# End of random forest model

# Comparing linear regression and random forest
cat("Linear Regression Test RMSE:", rmse, "\n")
cat("Random Forest Test RMSE:", rf_rmse, "\n")

cat("Linear Regression Test MAE:", mae, "\n")
cat("Random Forest Test MAE:", rf_mae, "\n")

cat("Linear Regression Test R-squared:", test_r_squared, "\n")
cat("Random Forest Test R-squared:", test_rf_r_squared, "\n")
# In this case, the random forest model shows better performance on the test set compared to the linear regression model, indicating it may be a better choice for this dataset.

# End of Ames housing analysis


