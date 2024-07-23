# North-Point Software Production Company 

# Libraries used for this project
library(caret)
library(ggplot2)
library(MASS) # For stepAIC function
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
# Read the North-Point dataset file
NorthPoint.df = read.csv("North-Point List.csv")

colnames(NorthPoint.df)
# Explore the dataset
head(NorthPoint.df)
# Dimesion of the data
dim(NorthPoint.df)
# Structure of the dataset
str(NorthPoint.df)

# Defining the correct column names 

# Change  "X1st_update_days_ago" name to "Days_Since_First_Update" 
colnames(NorthPoint.df)[colnames(NorthPoint.df) == "X1st_update_days_ago"] <- "Days_Since_First_Update"
# Change "last_update_days_ago" to "Days_Since_Last_Update"
colnames(NorthPoint.df)[colnames(NorthPoint.df) == "last_update_days_ago"] <- "Days_Since_Last_Update"
# Change "Freq" to FrequentNumTransactions
colnames(NorthPoint.df)[colnames(NorthPoint.df) == "Freq"] <- "Frequent_Num_Transactions"

# Statistics for numerical attributes
Numerical_Attributes = NorthPoint.df[,c(18, 19, 20, 25)]
summary(Numerical_Attributes)


# ***********Check for missing values************************
missing_values = sum(is.na(NorthPoint.df))
print(paste("Number of missing values are:", missing_values))  
# There are no missing values to handle.

# ****************Check for Zeros****************************
columns_to_check = c("Days_Since_Last_Update", "Days_Since_First_Update")
zero_counts = sapply(NorthPoint.df[columns_to_check], function(col) {
  sum(col == 0, na.rm = TRUE)
})
print(zero_counts)

# Check for zeros in the Spending_Amount attribute for rows where Purchase is equal to 1
# Filter rows where Purchase is equal to 1
purchase_1_data = subset(NorthPoint.df, Purchase == 1)
zero_counts_spending = sum(purchase_1_data$Spending == 0, na.rm = TRUE)
print(zero_counts_spending)

# Filter rows where Purchase is equal to 0
purchase_0_data = subset(NorthPoint.df, Purchase == 0)
# Count rows where Spending is not equal to 0 when purchase is equal to 0
non_zero_counts_spending = sum(purchase_0_data$Spending != 0, na.rm = TRUE)
print(non_zero_counts_spending)

# We can remove this record because the spending amount is $1.

# Identify the row(s) where Purchase is equal to 0 and Spending is not equal to 0
rows_to_replace <- which(NorthPoint.df$Purchase == 0 & NorthPoint.df$Spending != 0)

# Replace the Spending values with 0 for the identified row(s)
NorthPoint.df$Spending[rows_to_replace] <- 0

# Check if the replacement is successful
print(NorthPoint.df[rows_to_replace, c("Purchase", "Spending")])



#######################  Predictors Analysis  ##########################################

# *****************Check the distribution of spending variable*********************************


# Histogram for Spending
ggplot(NorthPoint.df, aes(x = Spending)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Spending",
       x = "Spending", y = "Count") +
  theme_minimal()

# The distribution of Spending is right-skewed.

# Density plot for Spending
ggplot(NorthPoint.df, aes(x = Spending)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Spending",
       x = "Spending", y = "Density") +
  theme_minimal()


# *******************Frequency of each Source*************************
# Get the names of source columns
source_columns <- c("source_a", "source_c", "source_b", "source_d", "source_e", "source_m",
                    "source_o", "source_h", "source_r", "source_s", "source_t", "source_u",
                    "source_p", "source_x", "source_w")

# Count the frequency of each source when it is equal to 1
source_frequency <- sapply(NorthPoint.df[source_columns], function(col) sum(col == 1))
print(source_frequency)

# Create a data frame for plotting
plot_data <- data.frame(Source = names(source_frequency), Frequency = source_frequency)

# Bar plot for frequency of each source when source == 1
ggplot(plot_data, aes(x = reorder(Source, -Frequency), y = Frequency, fill = Source)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of each source",
       x = "Sources", y = "Frequency", fill = "Sources") +
  theme_minimal()

# *************************Frequent_Num_Transactions attribute********************************
# Create a bar plot for the distribution of Frequent_Num_Transactions
ggplot(NorthPoint.df, aes(x = factor(Frequent_Num_Transactions))) +
  geom_bar() +
  labs(title = "Distribution of Frequency",
       x = "Frequent_Num_Transactions",
       y = "Count")

# ***************Web Order & Purchase**********************************
# Check the distribution of web order when purchase is made
# Filter the data for Purchase = 1
filtered_data <- subset(NorthPoint.df, Purchase == 1)

# Calculate the percentage of each category
percentage_data <- data.frame(table(filtered_data$Web.order) / nrow(filtered_data) * 100)

# Bar plot for Web Order when Purchase is 1 with percentages
ggplot(percentage_data, aes(x = Var1, y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Web Order when Purchase is 1",
       x = "Web Order", y = "Percentage", fill = "Web Order") +
  theme_minimal()



# ******************Scatter plot******************************
colors <- rainbow(length(unique(NorthPoint.df$Frequent_Num_Transactions)))
plot(NorthPoint.df$Frequent_Num_Transactions, NorthPoint.df$Spending,
     col = colors[as.numeric(factor(NorthPoint.df$Frequent_Num_Transactions))],
     xlab = "FrequentNumberTransactions", ylab = "Spending",
     main = "Scatter Plot b/w Frequent and Spending")



# *************************Predictors/Feature Importance ***************************

# ***********************Build the logistic regression model
  
# Fit the logistic regression model
logit.model <- glm(Purchase ~. -Spending-sequence_number, data = NorthPoint.df, family = "binomial")

# Use stepwise backward selection
step_backward <- stepAIC(logit.model, direction = "backward", trace = 1)
# Display the summary of the model after stepwise backward selection
summary(step_backward)

# ********************************Dimension Reductions***************************************
# Remove irrelevant attributes
NorthPoint.df = NorthPoint.df[, !(names(NorthPoint.df) %in% c("sequence_number"))]
colnames(NorthPoint.df)

# **********************Correlation matrix ********************************
# It will provide correlation between each attribute, but it only allows for numeric variables
Numerical_cols = data.frame(NorthPoint.df[c("Frequent_Num_Transactions", "Days_Since_Last_Update", "Days_Since_First_Update")])
correlation_matrix = cor(Numerical_cols)
correlation_matrix

# Set up the plot size
par(mar = c(4, 4, 3, 3))

# Create the correlation matrix plot
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", tl.cex = 1.2)
# Add a title
title(main = "Correlation Matrix", line = 0.5)


# ***************************Classification Models****************************************

# **********************************Data partitioning methods**************************** 

# Train, Validation & Test:

set.seed(20)
# Randomly sample row IDs for training
train_rows = sample(rownames(NorthPoint.df), nrow(NorthPoint.df) * 0.4)

# Sample row IDs for validation from records not in the training set
validation_rows = sample(setdiff(rownames(NorthPoint.df), train_rows),
                         nrow(NorthPoint.df) * 0.35)

# Remaining row IDs serve as holdout
holdout_rows = setdiff(rownames(NorthPoint.df), union(train_rows, validation_rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train_data1 = NorthPoint.df[train_rows,]
Validation_data1 = NorthPoint.df[validation_rows,]
holdout_data1 = NorthPoint.df[holdout_rows,]

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


# *********************1. Logistic Regression**********************

# Training a Model on the Data
Logit_Model<- glm(Purchase~ . -Spending, data=train_data1, family = "binomial")
Logit_Model
summary(Logit_Model)

Purchase_predict1 = predict(Logit_Model, newdata =Validation_data1, type = "response")
Purchase_predict1 = ifelse(Purchase_predict1 >= 0.5, "1", "0")

# Set the levels of Purchase_predict1 to match the order in Validation_data1$Purchase
Purchase_predict1 <- factor(Purchase_predict1,levels = c("1","0"))
Validation_data1$Purchase = factor(Validation_data1$Purchase, c("1","0"))
levels(Validation_data1$Purchase)
levels(Purchase_predict1)

#  Evaluating Model Performance
confusion_matrix = confusionMatrix(Purchase_predict1, Validation_data1$Purchase)

print(confusion_matrix)

# Improving Model Performance using step function and the direction is backward
Logit_Model_Step = step(Logit_Model, direction = "backward")

# After removing insignificant attributes
Logit_Model_Improved = glm(Purchase ~ source_a + source_e + source_h + source_r + source_s + 
                             source_t + source_u + source_p + source_x + source_w + Frequent_Num_Transactions + 
                             Days_Since_Last_Update + Web.order + Address_is_res, family = "binomial", data = train_data1)

summary(Logit_Model_Improved)

Purchase_predict2 = predict(Logit_Model_Improved, newdata = Validation_data1, type = "response")
Purchase_predict2 = ifelse(Purchase_predict2 >= 0.5, "1", "0")

# Convert predict_validation as factor
Purchase_predict2 <- factor(Purchase_predict2, levels = c("1","0"))
levels(Purchase_predict2)
levels(Validation_data1$Purchase)
confusion_matrix1 = confusionMatrix(Purchase_predict2, Validation_data1$Purchase, positive = "1")

print(confusion_matrix1)


# ************2. Classification tree *****************************

Class_Model <- rpart(Purchase ~. -Spending, data = train_data1, method="class")
rpart.plot(Class_Model, extra=1, fallen.leaves=FALSE)

summary(Class_Model)

Purchase_predict3 = predict(Class_Model, newdata = Validation_data1, type = "class")
Purchase_predict3 = factor(Purchase_predict3, levels = c("1","0"))

levels(Purchase_predict3)
levels(Validation_data1$Purchase)
confusion_matrix2 = confusionMatrix(Purchase_predict3, Validation_data1$Purchase, positive = "1")
print(confusion_matrix2)

# Try classification tree with the important predictors that shows in logistic regression
Class_Model_2 <- rpart(Purchase ~ source_a + source_e + source_h + source_r + source_s + 
                         source_t + source_u + source_p + source_x + source_w + Frequent_Num_Transactions + 
                         Days_Since_Last_Update + Web.order + Address_is_res, data = train_data1, method = "class")
rpart.plot(Class_Model_2, extra = 1, fallen.leaves = FALSE)
summary(Class_Model_2)

# Evaluate model 
Purchase_predict4 = predict(Class_Model_2, newdata = Validation_data1, type = "class")
Purchase_predict4 = factor(Purchase_predict4, levels = c("1","0"))

levels(Purchase_predict4)
levels(Validation_data1$Purchase)
confusion_matrix3 = confusionMatrix(Purchase_predict4, Validation_data1$Purchase, positive = "1")
print(confusion_matrix3)


# ****************************** For Goal - 2 *****************
# For this model we have to use only 1000 observations and it should be only purchasers
# Filter train_data1 for Purchase == 1
NorthPoint <- subset(NorthPoint.df, Purchase == 1)

# Train, Validation & Test:

set.seed(2024)
# Randomly sample row IDs for training
train_rows1 = sample(rownames(NorthPoint), nrow(NorthPoint) * 0.4)

# Sample row IDs for validation from records not in the training set
validation_rows1 = sample(setdiff(rownames(NorthPoint), train_rows1),
                         nrow(NorthPoint) * 0.35)

# Remaining row IDs serve as holdout
holdout_rows = setdiff(rownames(NorthPoint), union(train_rows1, validation_rows1))

# create the 3 data frames by collecting all columns from the appropriate rows
train_data_purchase_1 = NorthPoint[train_rows1,]
Validation_data_purchase_1 = NorthPoint[validation_rows1,]
holdout_data_purchase_1 = NorthPoint[holdout_rows,]


# Verify the sizes of each set
nrow(train_data_purchase_1)  
nrow(Validation_data_purchase_1)
nrow(holdout_data_purchase_1)

# ******************************** 1. Linear regression ****************************************
Linear_Regression = lm(Spending ~. -Purchase, data = train_data_purchase_1)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(Linear_Regression)

# Evaluate the model performance 
Spending_predict3 = predict(Linear_Regression, newdata = Validation_data_purchase_1)

# Calculate RMSE 
RMSE_3 = RMSE(Validation_data_purchase_1$Spending, Spending_predict3)
print(RMSE_3)
# MAE
MAE_3 = MAE(Spending_predict3, Validation_data_purchase_1$Spending)
print(MAE_3)

# ************** Using Step function *****************
StepWise = step(Linear_Regression, direction = "backward")

Linear_Regression2 = lm(Spending ~ US + source_a + source_h + source_t + source_u + Frequent_Num_Transactions + 
                          Days_Since_Last_Update + Address_is_res, data = train_data_purchase_1)

summary(Linear_Regression2)
# Evaluate the model performance 
Spending_predict4 = predict(Linear_Regression2, newdata = Validation_data_purchase_1)
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
# Calculate RMSE 
RMSE_4 = RMSE(Validation_data_purchase_1$Spending, Spending_predict4)
print(RMSE_4)
# MAE
MAE_4 = MAE(Spending_predict4, Validation_data_purchase_1$Spending)
print(MAE_4)

# *******************************2. Regression Tree model *******************************

# Fit the Regression tree model on train data 
Regression_model1 = rpart(Spending~. -Purchase, data = train_data_purchase_1, control = rpart.control(maxdepth=5))
summary(Regression_model1)
rpart.plot(Regression_model1, extra=1, fallen.leaves=FALSE)

Spending_predict1 = predict(Regression_model1, newdata = Validation_data_purchase_1)
# Measuring Performance with Mean Absolute Error
MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(Validation_data_purchase_1$Spending, Spending_predict1)
# RMSE 
RootMeanSquaredError = RMSE(Spending_predict1, Validation_data_purchase_1$Spending)
print(RootMeanSquaredError)

# **************Random forest****************

# To improve model performance i want to use random forest approach to see how the important predictors effect the model.
RandomForest_Model = randomForest(Spending ~. -Purchase, data = train_data_purchase_1, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
## variable importance plot
varImpPlot(RandomForest_Model, type=1)

Spending_predict2 = predict(RandomForest_Model, newdata = Validation_data_purchase_1)

# RMSE 
RMSE_2 = RMSE(Spending_predict2, Validation_data_purchase_1$Spending)
print(RMSE_2)
# MAE
MAE_2 = MAE(Validation_data_purchase_1$Spending, Spending_predict2)
print(MAE_2)

#                                                 Business Requirements
#-------------------------------------------------------------------------------------------------------------------------------------------
# *****************Gross Profit*************
G.Profit <- 180000 * (0.053*mean(NorthPoint.df$Spending[NorthPoint.df$Purchase==1])-2)
G.Profit 

# 4.	Considering the original holdout data (the one was built in step 2) then, 
# a.	Add a column to the data frame with the predicted probability of purchase from your selected machine learning model in step 2.

holdout_data1$Prob_Predict_Purchase = predict(Logit_Model_Improved, newdata = holdout_data1, type = "response")
head(holdout_data1)

# b.	Add another column with predicted spending value from the work in step 3.
holdout_data1$Predicted_Spending = predict(Linear_Regression2, newdata = holdout_data1)
head(holdout_data1)


# c.  Add a column for “adjusted probability of purchase” to adjust for oversampling the purchaser (see #1). 
#  This can be done by multiplying “predicted probability of purchase” by original purchase rate (1065/20000/0.50=0.1065). This is to adjust for over-sampling the purchases. 
holdout_data1$Adjusted_Prob_Purchase = holdout_data1$Prob_Predict_Purchase*0.1065
head(holdout_data1)

# d.	Add another column for expected spending. This should be the adjusted spending – adjusting predicted spending of 4b by adjusted probability of 4c.
holdout_data1$Expected_Spending = holdout_data1$Predicted_Spending*holdout_data1$Adjusted_Prob_Purchase
head(holdout_data1)

# *************Cumulative Gain chart using stepwise regression************************

Gain = gains(holdout_data1$Spending, holdout_data1$Expected_Spending)
# cumulative lift chart
# we will compute the gain relative to price
gains.df <- data.frame(
  ncases=c(0, Gain$cume.obs),
  cumExpectedSpending=c(0, Gain$cume.pct.of.total * sum(holdout_data1$Expected_Spending))
)
ggplot(gains.df, aes(x=ncases, y=cumExpectedSpending)) +
  geom_line(color = "black", size = 1.5) +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout_data1)), cumExpectedSpending=c(0, sum(holdout_data1$Expected_Spending))),
            color="darkgrey", linetype=2) + # adds baseline
  labs(x="Observations", y="ExpectedSpending", title="Cumulative gains chart using Stepwise regression") +
  scale_y_continuous(labels = scales::comma)


# *************Cumulative Gain chart using Random forest************************

# predict spending with random forest model
holdout_data1$Predicted_Spending.RF = predict(RandomForest_Model, newdata = holdout_data1)
head(holdout_data1)

holdout_data1$Expected_Spending.RF = holdout_data1$Predicted_Spending.RF*holdout_data1$Adjusted_Prob_Purchase
head(holdout_data1)


Gain1 = gains(holdout_data1$Spending, holdout_data1$Expected_Spending.RF)
# cumulative lift chart
# we will compute the gain relative to price
gains.df1 <- data.frame(
  ncases=c(0, Gain1$cume.obs),
  cumExpectedSpending.RF=c(0, Gain$cume.pct.of.total * sum(holdout_data1$Expected_Spending.RF))
)
ggplot(gains.df1, aes(x=ncases, y=cumExpectedSpending.RF)) +
  geom_line(color = "black", size = 1.5) +
  geom_line(data=data.frame(ncases=c(0, nrow(holdout_data1)), cumExpectedSpending.RF=c(0, sum(holdout_data1$Expected_Spending.RF))),
            color="darkgrey", linetype=2) + # adds baseline
  labs(x="Observations", y="Expected_Spending.RF", title="Cumulative gains chart using random forest") +
  scale_y_continuous(labels = scales::comma)

