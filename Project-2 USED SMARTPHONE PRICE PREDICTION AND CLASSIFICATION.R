

# *********************Project 2 - USED SMARTPHONE PRICE PREDICTION AND CLASSIFICATION ************************

# Load required libraries
library(dplyr)
#install.packages("mice", repos = "https://cran.r-project.org")
library(mice)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(MASS)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)

# Load the dataset file into a dataframe
Used_Mobiles = read.csv("used_device_data.csv", header = TRUE)
dim(Used_Mobiles)
colnames(Used_Mobiles)
str(Used_Mobiles)

##############################################################################################
# ***********************Data Preprocessing and Exploration***************************

# Select numerical attributes
numerical_attributes = Used_Mobiles %>%
  select_if(is.numeric)

# Select categorical attributes
categorical_attributes <- Used_Mobiles %>%
  select_if(is.character)

# Display the selected attributes
head(numerical_attributes)
head(categorical_attributes)

##################################################################################################
# *********************Rename columns******************************

Used_Mobiles <- Used_Mobiles %>%
  rename(`supports_4G` = X4g, `supports_5G` = X5g)

# Check the updated column names
colnames(Used_Mobiles)

#######################################################################################################
# *********************************Check for missing values*********************************************************

# Calculate the number of missing values for each attribute
missing_values_per_attribute <- sapply(Used_Mobiles, function(x) sum(is.na(x)))

# Create a data frame for better display
missing_values_df <- data.frame(Missing_Values = missing_values_per_attribute)

# Display the data frame
print(missing_values_df)

####################################################################################################
# ***********************Handle these missing values with MICE package ****************************************************

imp = mice(Used_Mobiles, seed = 123)
fit <- with(imp, lm(normalized_used_price ~ screen_size + rear_camera_mp + front_camera_mp + internal_memory + ram + battery + weight + release_year + days_used))
pooled = pool(fit)
summary(pooled)

imp$imp$rear_camera_mp
Used_Mobiles = complete(imp, action = 3)


# Recheck if the missing values are handled or not
missing_values_per_attribute <- sapply(Used_Mobiles, function(x) sum(is.na(x)))
missing_values_df <- data.frame(Missing_Values = missing_values_per_attribute)
print(missing_values_df)

# update the numerical and categorical attributes
# Select numerical attributes
numerical_attributes = Used_Mobiles %>%
  select_if(is.numeric)

# Select categorical attributes
categorical_attributes <- Used_Mobiles %>%
  select_if(is.character)

########################################################################################################
# ***********************************check for zeros*****************************************************

# Calculate the number of zeros for each attribute
zeros_per_attribute <- sapply(Used_Mobiles, function(x) sum(x == 0, na.rm = TRUE))

# Create a data frame for better display
zeros_df <- data.frame(Zeros_Count = zeros_per_attribute)
print(zeros_df)

# Check the distinct values for categorical attributes
unique_values = sapply(categorical_attributes, unique)
print(unique_values)

# Summary stats for numerical attributes
summary(numerical_attributes)

##########################################################################################################
# ********************************check for the Outliers using boxplots**************************************

# Function to create a box plot for a given attribute
plot_boxplot <- function(attribute) {
  ggplot(Used_Mobiles, aes(x = 1, y = !!sym(attribute))) +
    geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = paste("Box Plot for", attribute),
         x = NULL,
         y = attribute) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

# Create box plots
boxplots_row1 <- lapply(c("screen_size", "rear_camera_mp", "front_camera_mp", "internal_memory"), plot_boxplot)
boxplots_row2 <- lapply(c("ram", "weight", "release_year", "days_used"), plot_boxplot)

# Display the box plots in a 2x4 grid and a 3x6 grid
grid.arrange(grobs = boxplots_row1, ncol = 4)
grid.arrange(grobs = boxplots_row2, ncol = 4)

#####################################################################################################
# ****************************Distributions of each attribute************************************************************

# **************Frequency of device brand**************
# Calculate the frequency of each device brand
brand_frequency <- table(Used_Mobiles$device_brand)

# Sort the frequency table in descending order
sorted_brand_frequency <- sort(brand_frequency, decreasing = TRUE)
bar_colors <- rainbow(length(sorted_brand_frequency))

# Create a bar plot with sorted frequencies
barplot(sorted_brand_frequency, 
        main = "Device Brand Frequency",
        xlab = "Device Brand",
        ylab = "Frequency",
        col = bar_colors,
        las = 2)  # Rotate x-axis labels for better visibility if needed


# ************OS (Operating System):***************************************************************************************
# Calculate the frequency and percentage for each operating system
os_summary <- data.frame(table(Used_Mobiles$os))
os_summary$percentage <- os_summary$Freq / sum(os_summary$Freq) * 100

# Create a bar plot for OS distribution with percentage labels
ggplot(os_summary, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 4, color = "black") +  # Add percentage labels
  labs(title = "Operating System Distribution",
       x = "Operating System",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, color = "black"),  # Adjust font size and color for x-axis labels
        axis.text.y = element_text(size = 12, color = "black"))




# **************Network connectivity 4G and 5G***********************************************************************

# Create a new data frame for visualization
compatibility_df <- data.frame(
  Compatibility = c("4G Only", "5G Only", "4G and 5G", "No 4G or 5G"),
  Count = c(
    sum(Used_Mobiles$supports_4G == "yes" & Used_Mobiles$supports_5G == "no"),
    sum(Used_Mobiles$supports_4G == "no" & Used_Mobiles$supports_5G == "yes"),
    sum(Used_Mobiles$supports_4G == "yes" & Used_Mobiles$supports_5G == "yes"),
    sum(Used_Mobiles$supports_4G == "no" & Used_Mobiles$supports_5G == "no")
  )
)

# Calculate percentages
compatibility_df$Percentage <- (compatibility_df$Count / sum(compatibility_df$Count)) * 100

# Create a bar plot for 4G and 5G compatibility with percentages
ggplot(compatibility_df, aes(x = Compatibility, y = Count, fill = Compatibility)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 6, color = "black") +
  labs(title = "4G and 5G Compatibility",
       x = "Compatibility",
       y = "Count") +
  theme_minimal()


# ********************Histograms for numercial attributes*********************************

# Function to plot a histogram for a given attribute
plot_histogram <- function(attribute, binwidth = NULL) {
  ggplot(numerical_attributes, aes(x = !!sym(attribute))) +
    geom_histogram(binwidth = binwidth, fill = "orange", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", attribute),
         x = attribute,
         y = "Frequency") +
    theme_minimal()
}

# Plot histograms for selected attributes in a 4x4 grid
plots <- lapply(c("rear_camera_mp", "front_camera_mp", "internal_memory", "ram"), function(attribute) {
  plot_histogram(attribute, binwidth = 2)
})

# Arrange the plots in a 4x4 grid
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)


# *******************Plot histograms for each attribute************************************************************************
# Adjust binwidth as needed

plot_histogram("days_used", binwidth = 100)
plot_histogram("battery", binwidth = 500)
plot_histogram("weight", binwidth = 50)
plot_histogram("normalized_used_price", binwidth = 0.2)
plot_histogram("normalized_new_price", binwidth = 0.2)

# ***************************************Data Analysis***********************************************************************

# Create a scatter plot for battery and weight
ggplot(numerical_attributes, aes(x = battery, y = weight)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Relationship Between Battery and Weight",
       x = "Battery Capacity",
       y = "Weight") +
  theme_minimal()

# Scatter plot for days_used vs. normalized_used_price
ggplot(Used_Mobiles, aes(x = days_used, y = normalized_used_price)) +
  geom_point() +
  labs(title = "Scatter Plot: Days Used vs. Used Price",
       x = "Days Used",
       y = "normalized_used_price") +
  theme_minimal()

# ************************ "Classifying Used Mobile Devices: Lower vs. Higher Number of Used Days" ***********************
Used_Mobiles$categorical_days_used <- cut(Used_Mobiles$days_used,
                                          breaks = c(0, 600, 1200),
                                          labels = c("Lower No of Used Days", "Higher No of Used Days"),
                                          include.lowest = TRUE)

head(Used_Mobiles)


# Create a bar plot
ggplot(Used_Mobiles, aes(x = categorical_days_used, y = normalized_used_price, fill = categorical_days_used)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Normalized Used Price by Categories of Used Days",
       x = "Categorical Days Used",
       y = "Average Used Price") +
  theme_minimal()


# ***************************** Top 10 device brands by average new price **********************************

# Calculate the average normalized new price for each device brand
average_prices <- aggregate(normalized_new_price ~ device_brand, data = Used_Mobiles, FUN = mean)

# Sort the brands in descending order based on average price
average_prices <- average_prices[order(average_prices$normalized_new_price, decreasing = TRUE), ]

# Select the top 10 brands
top_10_brands <- head(average_prices, 10)

# Create a bar plot with labels
ggplot(top_10_brands, aes(x = reorder(device_brand, -normalized_new_price), y = normalized_new_price, fill = device_brand)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(normalized_new_price, 2)), vjust = -0.5, size = 3) +  # Add text labels for average price
  labs(title = "Top 10 Device Brands by Average Normalized New Price", x = "Device Brand", y = "Average Normalized New Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# **********************Top 10 device brands by average used price***************************

# Calculate the average price for each device brand
average_prices <- aggregate(normalized_used_price ~ device_brand, data = Used_Mobiles, FUN = mean)

# Sort the brands in descending order based on average price
average_prices <- average_prices[order(average_prices$normalized_used_price, decreasing = TRUE), ]

# Select the top 10 brands
top_10_brands <- head(average_prices, 10)

# Create a bar plot with labels
ggplot(top_10_brands, aes(x = reorder(device_brand, -normalized_used_price), y = normalized_used_price, fill = device_brand)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(normalized_used_price, 2)), vjust = -0.5, size = 3) +  # Add text labels for average price
  labs(title = "Top 10 Device Brands by Average Normalized Used Price", x = "Device Brand", y = "Average Normalized Used Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


########################################################################################################
# *************************Data Transformation ***************************************

# Create a new column 'network_connectivity' based on 'supports_4G' and 'supports_5G'
Used_Mobiles$network_connectivity <- ifelse(Used_Mobiles$supports_4G == "yes" & Used_Mobiles$supports_5G == "yes", "5G",
                                            ifelse(Used_Mobiles$supports_4G == "yes", "4G",
                                                   ifelse(Used_Mobiles$supports_5G == "yes", "5G", "other")))

# Reorder the columns to place 'network_connectivity' in the same position
Used_Mobiles <- Used_Mobiles[, c(names(Used_Mobiles)[1:3], "network_connectivity", names(Used_Mobiles)[4:ncol(Used_Mobiles)])]

# Drop the original 'supports_4G' and 'supports_5G' columns
Used_Mobiles <- Used_Mobiles[, !(names(Used_Mobiles) %in% c("supports_4G", "supports_5G"))]

# Remove the 'network_generation.1' column (if it exists)
Used_Mobiles <- Used_Mobiles[, !(names(Used_Mobiles) %in% c("network_connectivity.1"))]

# Print the first few rows of the updated data frame
head(Used_Mobiles)
unique(Used_Mobiles$network_connectivity)

# Creating dummies for categorical variables
library(fastDummies)
Used_Mobiles = dummy_cols(Used_Mobiles, select_columns = "os",remove_selected_columns = TRUE)

Used_Mobiles = dummy_cols(Used_Mobiles, select_columns = "network_connectivity",remove_selected_columns = TRUE)



  
######################################################################################################
# ******************************Correlation Analysis*****************************************************

# Calculate the correlation matrix for numerical attributes
correlation_matrix <- cor(numerical_attributes)
correlation_matrix <- cor(numerical_attributes, method = "spearman")

# Create a correlation plot
corrplot(correlation_matrix, method = "number", type = "upper", order = "hclust", tl.cex = 0.7)
# Print the correlation coefficients
print(correlation_matrix)



# ********************************Feature Selection*********************************************************
# Stepwise 
set.seed(2024)
Linear.Model = glm(normalized_used_price~. -categorical_days_used, data = Used_Mobiles, family = "gaussian")
# Use stepwise backward selection
step_backward = step(Linear.Model, direction = "backward", trace = 1)


# This line takes atleast 10-15 mins to execute,  Uncomment this to see the result 
'
# *******************************Recursive Feature Selection********************************************************
set.seed(2024)
control<-rfeControl(functions = rfFuncs, method = "cv", number=10)
colnames(Used_Mobiles)
rf.train<-rfe(Used_Mobiles[, -c(1, 12, 13)], Used_Mobiles[, 11], sizes=c(5,8,10,12,15), rfeControl=control)
rf.train
plot(rf.train, type=c("g", "o"), cex=1, col=1:5)
'


# ********************************Data Partitioning**************************************

# Train, Validation & Test:
set.seed(2024)
# Randomly sample row IDs for training
train_rows = sample(rownames(Used_Mobiles), nrow(Used_Mobiles) * 0.4)

# Sample row IDs for validation from records not in the training set
validation_rows = sample(setdiff(rownames(Used_Mobiles), train_rows),
                         nrow(Used_Mobiles) * 0.35)

# Remaining row IDs serve as holdout
holdout_rows = setdiff(rownames(Used_Mobiles), union(train_rows, validation_rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train_data1 = Used_Mobiles[train_rows,]
Validation_data1 = Used_Mobiles[validation_rows,]
holdout_data1 = Used_Mobiles[holdout_rows,]

# Verify the sizes of each set
nrow(train_data1)  
nrow(Validation_data1)
nrow(holdout_data1)

# ******** Visualize the data partitions**************
# Combine the data
set_sizes <- c(train = nrow(train_data1), validation = nrow(Validation_data1), holdout = nrow(holdout_data1))

# Create a bar plot
barplot(set_sizes, names.arg = names(set_sizes), col = c("blue", "red", "orange"), main = "Sizes of Data Sets", ylab = "Number of Rows", ylim = c(0, max(set_sizes) + 200))

# Add text labels
text(x = 1:3, y = set_sizes + 50, labels = set_sizes, col = "black", pos = 3)


# ************************* Model Selection *******************************

# ************************For goal-1 Prediction ************************************

# *********************** 1. Linear Regression ******************************
Linear.model = lm(normalized_used_price~. -device_brand-categorical_days_used, data = train_data1)
options(scipen = 999)
summary(Linear.model)

Linear_Predict = predict(Linear.model, newdata = Validation_data1)

# Calculate RMSE 
Linear_RMSE = RMSE(Validation_data1$normalized_used_price, Linear_Predict)
print(Linear_RMSE)
# MAE
Linear_MAE = MAE(Linear_Predict, Validation_data1$normalized_used_price)
print(Linear_MAE)

# ************** Using Step function *****************
StepWise = step(Linear.model, direction = "backward")

Linear_model_Stepwise = lm(normalized_used_price ~ screen_size + rear_camera_mp + front_camera_mp + 
                             ram + weight + release_year + normalized_new_price + network_connectivity_4G, data = train_data1)

summary(Linear_model_Stepwise)

Linear_Step_Predict = predict(Linear_model_Stepwise, newdata = Validation_data1)

# Calculate RMSE 
Linear_Step_RMSE = RMSE(Validation_data1$normalized_used_price, Linear_Step_Predict)
print(Linear_Step_RMSE)
# MAE
Linear_Step_MAE = MAE(Linear_Step_Predict, Validation_data1$normalized_used_price)
print(Linear_Step_MAE)


# **************************2. Regression Tree *************************************************

# Fit the Regression tree model on train data 
Regression.model = rpart(normalized_used_price~. -device_brand-categorical_days_used, data = train_data1)
summary(Regression.model)

rpart.plot(Regression.model, extra=1, fallen.leaves=FALSE)

printcp(Regression.model)

# ****************** Prune the regression tree******************************************
pruned_model <- prune(Regression.model, cp = 0.01)  # use the cp parameter where the error is low

# Plot the pruned tree
rpart.plot(pruned_model, extra = 1, fallen.leaves = FALSE, cex = 0.8)

# Make predictions on the validation data using the pruned tree
pruned_predictions <- predict(pruned_model, newdata = Validation_data1)

# Measuring performance with Mean Absolute Error
MAE(Validation_data1$normalized_used_price, pruned_predictions)

# RMSE 
RMSE(pruned_predictions, Validation_data1$normalized_used_price)




# **************************** For Goal-2 Classification *************************************
# To build classification model first create a categorical variable for normalized_used_price

average_used_price = mean(Used_Mobiles$normalized_used_price)
print(average_used_price)

# Create a new categorical variable 'price_category'
Used_Mobiles$Used_price_category <- ifelse(Used_Mobiles$normalized_used_price > average_used_price, 'High', 'Low')

# Display the first few rows of the updated dataset
head(Used_Mobiles)

# Now considering this Used_price_category variable as target build classification models

Used_Mobiles$Used_price_category = as.factor(Used_Mobiles$Used_price_category)

# ********************************Data Partitioning -2**************************************

# Train, Validation & Test:
set.seed(2024)
# Randomly sample row IDs for training
train_rows1 = sample(rownames(Used_Mobiles), nrow(Used_Mobiles) * 0.4)

# Sample row IDs for validation from records not in the training set
validation_rows1 = sample(setdiff(rownames(Used_Mobiles), train_rows),
                         nrow(Used_Mobiles) * 0.35)

# Remaining row IDs serve as holdout
holdout_rows1 = setdiff(rownames(Used_Mobiles), union(train_rows1, validation_rows1))

# create the 3 data frames by collecting all columns from the appropriate rows
train_data2 = Used_Mobiles[train_rows1,]
Validation_data2 = Used_Mobiles[validation_rows1,]
holdout_data2 = Used_Mobiles[holdout_rows1,]

# Verify the sizes of each set
nrow(train_data2)  
nrow(Validation_data2)
nrow(holdout_data2)

head(train_data2)
table(Used_Mobiles$Used_price_category)

# ****************** 1. Classification Tree *********************************************

# fit the classification tree usinbg rpart
Classification.model = rpart(Used_price_category~. -device_brand-categorical_days_used-normalized_used_price, data = train_data2, method = "class")
rpart.plot(Classification.model, extra=1, fallen.leaves=FALSE)
summary(Classification.model)

# prune the tree 
prune_Classification.model = prune(Classification.model, cp = 0.01)
rpart.plot(prune_Classification.model, extra=1, fallen.leaves=FALSE)

# predict the used price category using validation set
Classification.model_predict = predict(Classification.model, newdata = Validation_data2, type = "class")
levels(Classification.model_predict)
Classification.model_predict = factor(Classification.model_predict, levels = c("High","Low"), labels = c("1","0"))

levels(Classification.model_predict)

levels(Validation_data2$Used_price_category)
Validation_data2$Used_price_category = factor(Validation_data2$Used_price_category, levels = c("High","Low"), labels = c("1","0"))
levels(Validation_data2$Used_price_category)


confusion_matrix_Classification.model = confusionMatrix(Classification.model_predict, Validation_data2$Used_price_category, positive = "1")
print(confusion_matrix_Classification.model)


# ************************2. KNN Model *******************************************
# To select the best k-value perform cross-validation

# Define the training control with 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Create a data frame to store results
k_values <- data.frame(k = numeric(), Accuracy = numeric())

# Loop through different 'k' values (e.g., 1 to 10)
for (k in 1:10) {
  knn_model <- train(
    Used_price_category ~ screen_size + rear_camera_mp + front_camera_mp + 
      ram + weight + release_year + normalized_new_price + network_connectivity_4G,
    data = train_data2,
    method = "knn",
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(k = k),
    trControl = ctrl
  )
  
  # Store results
  k_values <- rbind(k_values, data.frame(k = k, Accuracy = knn_model$results$Accuracy))
}

# Find the 'k' value with the highest accuracy
best_k <- k_values[which.max(k_values$Accuracy), ]

# Display the results
print(k_values)

# Display the best 'k' value
print(best_k)

# Train k-NN model 
knn.model <- train(
  Used_price_category ~ . -device_brand-categorical_days_used-normalized_used_price,
  data = train_data2,
  method = "knn", 
  preProcess = c("center", "scale"),  # normalize data
  tuneGrid = expand.grid(k = 9),
  trControl = trainControl(method = "none")
)

# Display the trained k-NN model
knn.model

# Prediction on Validation_data2 set
knn.model_Predict = predict(knn.model, newdata = Validation_data2)

knn.model_Predict = factor(knn.model_Predict, levels = c("High", "Low"), labels = c("1","0"))
levels(knn.model_Predict)

levels(Validation_data2$Used_price_category)
# Confusion Matrix for Predictions on Validation_data2 set
Confusion_Matrix_knn.model = confusionMatrix(knn.model_Predict, Validation_data2$Used_price_category, positive = "1")

# Display the confusion matrix
Confusion_Matrix_knn.model


# ********* KNN With important predictors that we seen in linear regression stepwise *********************

knn.model_Step = train(
    Used_price_category ~ screen_size + rear_camera_mp + front_camera_mp + 
      ram + weight + release_year + normalized_new_price + network_connectivity_4G,
    data = train_data2,
    method = "knn", 
    preProcess = c("center", "scale"),  # normalize data
    tuneGrid = expand.grid(k = 9),
    trControl = trainControl(method = "none")
  )

knn.model_Step
# Prediction on Validation_data2 set
knn.model_Step_Predict = predict(knn.model_Step, newdata = Validation_data2)

knn.model_Step_Predict = factor(knn.model_Step_Predict, levels = c("High", "Low"), labels = c("1","0"))
levels(knn.model_Step_Predict)

levels(Validation_data2$Used_price_category)
# Confusion Matrix for Predictions on Validation_data2 set
Confusion_Matrix_knn.model_Step = confusionMatrix(knn.model_Step_Predict, Validation_data2$Used_price_category, positive = "1")

# Display the confusion matrix
Confusion_Matrix_knn.model_Step



  
# **************** Model performance on holdout data ********************************
# ********* Using Linear Model For regression ******************

# Predictions on holdout data
Linear_model_predict_hold = predict(Linear.model, newdata = holdout_data1)

# Calculate RMSE 
RMSE(holdout_data1$normalized_used_price, Linear_model_predict_hold)

#  Calculate MAE
MAE(Linear_model_predict_hold, holdout_data1$normalized_used_price)


# ************** Using KNN Model for Classification ******************

knn.model_Predict_hold = predict(knn.model, newdata = holdout_data2)
levels(knn.model_Predict_hold)
knn.model_Predict_hold = factor(knn.model_Predict_hold, levels = c("High", "Low"), labels = c("1","0"))
levels(holdout_data2$Used_price_category)
holdout_data2$Used_price_category = factor(holdout_data2$Used_price_category, levels = c("High", "Low"), labels = c("1","0"))
levels(holdout_data2$Used_price_category)

confusionMatrix(knn.model_Predict_hold, holdout_data2$Used_price_category, positive = "1")



# *****************************ROC Curve *****************************************************

# Convert predictions and actual values to numeric
knn_predictions <- as.numeric(knn.model_Predict_hold)
actual_values <- as.numeric(holdout_data2$Used_price_category)

# Compute ROC curve
roc_curve <- roc(actual_values, knn_predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for kNN Model",
     col = "blue", lwd = 2,
     print.auc = TRUE, print.thres = TRUE)


