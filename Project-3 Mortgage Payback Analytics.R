
# ***************** Project-3: Mortgage Payback Analytics*********************************

#Load Required libraries
library(caret)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(fastDummies)
library(dplyr)
library(rpart)
# Load the Mortgage file
Mortgage = read.csv("Mortgage.csv")

head(Mortgage)

# Selecting numerical attributes
numerical_attributes = Mortgage[, -c(12,13,14,15,21,22,23)]

# Selecting categorical attributes
categorical_attributes = Mortgage[, c(12,13,14,15,21,22,23)]
head(numerical_attributes)
head(categorical_attributes)

summary(numerical_attributes)
# Explore dataset
str(Mortgage)


########################################################################################################
# *************************Check for missing values*****************************************************
# Calculate the number of missing values for each attribute
missing_values_per_attribute <- sapply(Mortgage, function(x) sum(is.na(x)))

# Create a data frame for better display
missing_values_df <- data.frame(Missing_Values = missing_values_per_attribute)
print(missing_values_df)

# Identify rows with missing values
missing_rows <- which(rowSums(is.na(Mortgage)) > 0)

# Extract unique borrower IDs from the subset
borrower_ids_with_missing <- unique(Mortgage$id[missing_rows])
print(borrower_ids_with_missing)


# Get the balance_orig_time to calculate LTV_time

# Identify rows with missing values in LTV_time
missing_ltv_rows <- which(is.na(Mortgage$LTV_time))

# Get unique borrower IDs with missing LTV_time
missing_ids <- unique(Mortgage$id[missing_ltv_rows])

# Initialize vectors to store information
id_vector <- character(length = length(missing_ids))
ltv_missing_vector <- character(length = length(missing_ids))
balance_zero_vector <- character(length = length(missing_ids))

# Loop through each ID
for (i in 1:length(missing_ids)) {
  id <- missing_ids[i]
  id_vector[i] <- id
  
  # Check if LTV_time is missing for this ID
  ltv_missing <- ifelse(id %in% missing_ids, "Yes", "No")
  ltv_missing_vector[i] <- ltv_missing
  
  # Check if balance_orig_time is zero for this ID
  balance_zero <- ifelse(sum(Mortgage$id == id & Mortgage$balance_orig_time == 0) > 0, "Yes", "No")
  balance_zero_vector[i] <- balance_zero
}

# Create the data frame
count_table <- data.frame(
  ID = id_vector,
  LTV_time_missing = ltv_missing_vector,
  Balance_time_zero = balance_zero_vector
)

# Display the table
print(count_table)



# Replace Missing values in Ltv_time with zero
Mortgage$LTV_time[missing_ltv_rows] <- 0

# Recheck missing values in Ltv_time
missing_ltv_count <- sum(is.na(Mortgage$LTV_time))
print(missing_ltv_count)


########################################################################################################
# ***********************************check for zeros*****************************************************

# Calculate the number of zeros for each attribute
zeros_per_attribute <- sapply(Mortgage[,-c(12,13,14,15,21,22,23)], function(x) sum(x == 0, na.rm = TRUE))

# Create a data frame for better display
zeros_df <- data.frame(Zeros_Count = zeros_per_attribute)
print(zeros_df)

# Histogram
hist(Mortgage$orig_time)
hist(Mortgage$interest_rate_time)

# Find ids having zero interest rate
ids_with_zero_interest <- Mortgage$id[Mortgage$interest_rate_time == 0]
ids_with_zero_interest

# check the balance_time for these ids
# Subset the Mortgage dataset for IDs with zero interest rate
zero_interest_data <- Mortgage[Mortgage$id %in% ids_with_zero_interest, ]

# Check the balance_time for these IDs
balance_time_zero <- zero_interest_data$balance_time
balance_time_zero

# **************************** Data Transformation *******************************************
#__________________________ Time stamp Variable_____________________________
# Calculate the number of transactions for each ID
transactions_counts <- count(Mortgage, id)
# Extract top 10 and bottom 10 IDs
top_10 <- head(transactions_counts[order(-transactions_counts$n),], 10)
bottom_10 <- head(transactions_counts[order(transactions_counts$n),], 10)

# Create plots
top_10_plot <- ggplot(top_10, aes(x = reorder(factor(id), -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 IDs with Highest Number of Transactions",
       x = "ID",
       y = "Number of Transactions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom_10_plot <- ggplot(bottom_10, aes(x = reorder(factor(id), n), y = n)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Bottom 10 IDs with Lowest Number of Transactions",
       x = "ID",
       y = "Number of Transactions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
grid.arrange(top_10_plot, bottom_10_plot, ncol = 1)
# Calculate the number of transactions for each ID and filter out IDs with one transaction and status_time equal to 1 or 2
Mortgage <- Mortgage %>%
  group_by(id) %>%
  mutate(
    number_of_transactions = n(),
    # Filter out IDs with one transaction and status_time equal to 1 or 2
    keep_row = !(n() == 1 & any(status_time %in% c(1, 2)))
  ) %>%
  filter(keep_row)

# Create a new dataframe with unique IDs and the number_of_transactions column
Mortgage_transformed <- Mortgage %>%
  distinct(id, .keep_all = TRUE) %>%
  dplyr::select(id, number_of_transactions)


# View the updated dataframe
head(Mortgage_transformed)
nrow(Mortgage_transformed)

# Unique number of IDs in Mortgage
unique_ids_mortgage <- n_distinct(Mortgage$id)
print(unique_ids_mortgage)

#____________________________Orig_time_ & first_time___________________________________
var(Mortgage$orig_time)
unique(Mortgage$first_time)
var(Mortgage$first_time)
hist(Mortgage$first_time) 


#__________________________________Mat_time and orig_time will tell us loan Period ____________________________________________
# Calculate the total number of years of the loan for each ID
years_of_loan <- aggregate(list(Loan_Period = (Mortgage$mat_time - Mortgage$orig_time) / 4),
                           by = list(ID = Mortgage$id),
                           FUN = function(x) unique(x))

# Display the loan period for each unique ID
print(years_of_loan)



################## Combining 3 columns into property type column ##################################
# Create a new column to represent the combined property type 
Mortgage <- Mortgage %>%
  mutate(Property_Type = ifelse(
    REtype_CO_orig_time == 1, "Condominium",
    ifelse(
      REtype_PU_orig_time == 1, "Planned Urban Development",
      ifelse(
        REtype_SF_orig_time == 1, "Single-Family Home",
        "Other"
      )
    )
  ))

# Update Mortgage_transformed with Property_Type column
Mortgage_transformed <- Mortgage_transformed %>%
  left_join(dplyr::select(distinct(Mortgage, id, Property_Type), id, Property_Type), by = "id")

# View the updated dataframe
head(Mortgage_transformed)
unique(Mortgage_transformed$Property_Type)



#______________________Balance time_______________________________________

# Calculate the difference between each row in 'balance_time' for each ID in the original "Mortgage" dataframe
Balance_time_MP <- Mortgage %>%
  arrange(id, time) %>%
  group_by(id) %>%
  mutate(Missing_Payments = sum(balance_time - lag(balance_time) == 0, na.rm = TRUE))

# Update the results in the "Mortgage_transformed" dataframe based on the 'id' column
Mortgage_transformed <- Mortgage_transformed %>%
  left_join(dplyr::select(distinct(Balance_time_MP, id, Missing_Payments), id, Missing_Payments), by = "id")

# View the updated dataframe
head(Mortgage_transformed)

#________________________________LTV_time_________________________________________________
# Calculate the average LTV for each ID in the original "Mortgage" dataframe
LTV_time_Avg <- Mortgage %>%
  group_by(id) %>%
  mutate(avg_LTV = mean(LTV_time, na.rm = TRUE))

# Update the "Mortgage_transformed" dataframe with the average LTV values
Mortgage_transformed <- left_join(Mortgage_transformed, 
                                  distinct(dplyr::select(LTV_time_Avg, id, avg_LTV)),
                                  by = "id")

# View the updated dataframe
head(Mortgage_transformed)

#______________________interest_rate_time_________________________________________________
# Threshold to determine if interest rate is fixed or variable
threshold <- 0.01

# Calculate the standard deviation of interest rate for each borrower in the original "Mortgage" dataframe
std_dev_interest <- Mortgage %>%
  group_by(id) %>%
  summarise(std_dev_interest = sd(interest_rate_time, na.rm = TRUE)) %>%
  mutate(interest_rate_type = ifelse(std_dev_interest < threshold, "Fixed", "Variable")) %>%
  dplyr::select(id, interest_rate_type)

# Update the "Mortgage_transformed" dataframe with the interest rate type based on standard deviation
Mortgage_transformed <- Mortgage_transformed %>%
  left_join(std_dev_interest, by = "id")

# View the updated dataframe
head(Mortgage_transformed)

#____________________________ Data collection from original dataframe______________________________
# Retain only the last row for each unique id in the "Mortgage" dataframe
last_rows <- Mortgage %>%
  group_by(id) %>%
  slice_tail(n = 1)
nrow(last_rows)
# Select specified columns
selected_columns <- dplyr::select(last_rows, id, orig_time, first_time, balance_time, interest_rate_time, hpi_time, gdp_time, uer_time, investor_orig_time, 
                           balance_orig_time, FICO_orig_time, LTV_orig_time, Interest_Rate_orig_time, hpi_orig_time, status_time)

# Update the corresponding columns in "Mortgage_transformed" dataframe with the last row values
Mortgage_transformed <- Mortgage_transformed %>%
  left_join(selected_columns, by = "id", suffix = c("_transformed", "_last"))
nrow(Mortgage_transformed)

# Remove duplicate rows based on the id column
Mortgage_transformed <- distinct(Mortgage_transformed, id, .keep_all = TRUE)
nrow(Mortgage_transformed)

# View the updated dataframe
head(Mortgage_transformed)

# View the updated dataframe
Mortgage_transformed = as.data.frame(Mortgage_transformed)
head(Mortgage_transformed)
colnames(Mortgage_transformed)
###########################################################################################################
# _____________________________Data Analysis for Numerical Attributes____________________________________
# Selecting numerical attributes
str(Mortgage_transformed)
Updated_numerical_attributes = Mortgage_transformed[, -c(1,3,6,14,20)]
head(Updated_numerical_attributes)
# Selecting categorical attributes
Updated_categorical_attributes = Mortgage_transformed[, c(9,15,17,20)]
# Histograms
par(mfrow=c(2,2)) # Set up a grid of plots
for (i in 2:ncol(Updated_numerical_attributes)) {
  hist(Updated_numerical_attributes[, i], main=colnames(Updated_numerical_attributes)[i], xlab="")
}
par(mfrow = c(1,1))


###################Interest rate##########################################
# Count the frequency of each interest rate type
interest_rate_counts <- table(std_dev_interest$interest_rate_type)

# Calculate percentages
total <- sum(interest_rate_counts)
percentage_fixed <- (interest_rate_counts["Fixed"] / total) * 100
percentage_variable <- (interest_rate_counts["Variable"] / total) * 100

# Create a barplot
bp3 <- barplot(interest_rate_counts, main = "Distribution of Interest Rate Types", ylim = c(0,40000),
              xlab = "Interest Rate Type", ylab = "Frequency", col = "skyblue",
              names.arg = c("Fixed", "Variable"))

# Add percentages above each bar
text(bp3, interest_rate_counts + 0.05 * max(interest_rate_counts), labels = paste(round(c(percentage_fixed, percentage_variable), 2), "%"), pos = 3, cex = 0.9)

###########################################################################################################
# __________________________Data Analysis for Categorical Attributes______________________________________

################## Bar plot for Property Type of Borrowers ############################ 
# Calculate the frequency of each property type for each unique borrower ID
property_freq <- aggregate(Property_Type ~ id, data = Mortgage_transformed, FUN = function(x) table(factor(x, levels = c("Condominium", "Planned Urban Development", "Single-Family Home", "Other"))))

# Sum up the frequency of each property type across all borrower IDs
total_counts <- colSums(property_freq$Property_Type)

# Create a barplot
bp = barplot(total_counts, main = "Property Types of Borrowers", 
             xlab = "Property Type", ylab = "Frequency", ylim = c(0, 31000), col = "skyblue",
             names.arg = c("Condominium", "Planned Urban Development", "Single-Family Home", "Other"))

# Calculate and add percentages to the bars
bar_percentage <- round(total_counts / sum(total_counts) * 100, 1)
text(x = bp, y = total_counts + 500, labels = paste0(bar_percentage, "%"), pos = 3, cex = 0.8)

################  Investor_orig_time: Investor borrower at origination time ######################

barplot(table(Mortgage_transformed$investor_orig_time), main = "Investor borrower at origin time", col = "skyblue")

######################################################################################
# ______________________________Target variable______________________________________
status_table <- table(Mortgage_transformed$status_time)

# Plot the bar plot
barplot(status_table, 
        main = "Status of borrowers",
        xlab = "Status",
        ylab = "Frequency",
        col = "skyblue",
        ylim = c(0, max(status_table) * 1.2),
        names.arg = c("0", "1", "2"))
# Add labels
text(x = 1:3, y = status_table, labels = status_table, pos = 3, cex = 0.8, col = "black")




#######################################################################################
# ******************************Correlation Analysis*****************************************************
# Calculate the correlation matrix for numerical attributes
#correlation_matrix <- cor(Numerical_attributes)
correlation_matrix <- cor(Updated_numerical_attributes[,-1], method = "spearman")

# Create a correlation plot
corrplot(correlation_matrix, method = "number", type = "upper", order = "hclust", tl.cex = 0.5)
# Print the correlation coefficients
print(correlation_matrix)


########################################################################################################
# ********************************Feature Selection using Stepwise**************************************
 
set.seed(2024)
Logistic <- glm(status_time ~ . - id, data = Mortgage_transformed, family = "gaussian")

# Use stepwise backward selection
step_backward = step(Logistic, direction = "backward", trace = 1)



###########################################################################################
# Creating dummies for categorical variables
Mortgage_transformed = dummy_cols(Mortgage_transformed, select_columns = "Property_Type", remove_selected_columns = TRUE)
Mortgage_transformed$interest_rate_type = factor(Mortgage_transformed$interest_rate_type)

# Separate the ids having status_time paidoff or default
paidoff_default <- Mortgage_transformed %>%
  filter(status_time %in% c(1, 2))

# Remaining ids go to Holdout.df
Holdout.df <- Mortgage_transformed %>%
  filter(!status_time %in% c(1, 2))

# View the two dataframes
head(paidoff_default)
head(Holdout.df)

table(paidoff_default$status_time)
table(Holdout.df$status_time)

paidoff_default$status_time <- ifelse(paidoff_default$status_time == 2, 1, ifelse(paidoff_default$status_time == 1, 0, paidoff_default$status_time))

# Verify the changes
table(paidoff_default$status_time)


####################################################################################

## Data Partitioning
# Train, Validation & Test:
set.seed(2024)
# Randomly sample row IDs for training
train_rows = sample(rownames(paidoff_default), nrow(paidoff_default) * 0.6)

# Sample row IDs for validation from records not in the training set
validation_rows = sample(setdiff(rownames(paidoff_default), train_rows),
                         nrow(paidoff_default) * 0.4)

# Remaining row IDs serve as holdout
head(Holdout.df)

# create the 3 data frames by collecting all columns from the appropriate rows
train_data1 = paidoff_default[train_rows,]
Validation_data1 = paidoff_default[validation_rows,]

# Verify the sizes of each set
nrow(train_data1)  
nrow(Validation_data1)
nrow(Holdout.df)

# ******** Visualize the data partitions**************
# Combine the data
set_sizes <- c(train = nrow(train_data1), validation = nrow(Validation_data1), holdout = nrow(Holdout.df))

# Create a bar plot
bp1 = barplot(set_sizes, names.arg = names(set_sizes), col = c("blue", "red", "orange"), main = "Sizes of Data Sets", ylab = "Number of Rows", ylim = c(0, max(set_sizes) + 5000))

# Add text labels
text(x = bp1, y = set_sizes + 500, labels = set_sizes, col = "black", pos = 3)


# Target Variable
table(paidoff_default$status_time)
table(train_data1$status_time)
table(Validation_data1$status_time)
table(Holdout.df$status_time)

##############################################################################################
# __________________________________Model Building_______________________________________

# _________________Model-1: Logistic regression_________________________________

#####################################################################################

# Fit logistic regression model
Logistic_Model = glm(status_time ~ . -id, data = train_data1, family = binomial)

summary(Logistic_Model)

# Predict the outcome using the logistic regression model
Logistic_predict = predict(Logistic_Model, newdata = Validation_data1, type = "response")
# Convert predicted probabilities to predicted class labels (0 or 1)
predicted_labels = as.factor(ifelse(Logistic_predict > 0.5, 1, 0))
levels(predicted_labels)
predicted_labels = factor(predicted_labels, levels = c("0","1"), labels = c("Default","Paidoff"))
Validation_data1$status_time = factor(Validation_data1$status_time, levels = c("0","1"), labels = c("Default","Paidoff"))
levels(Validation_data1$status_time)
levels(predicted_labels)

confusionMatrix(Validation_data1$status_time, predicted_labels, positive = "Default")


##########################################################################################
# classification Tree  

Classification.model = rpart(status_time ~ . -id, data = train_data1, method = "class")
summary(Classification.model)

Classification_predict = predict(Classification.model, newdata = Validation_data1, type = "class")
Classification_predict = factor(Classification_predict, levels = c("0","1"), labels = c("Default","Paidoff"))
levels(Validation_data1$status_time)
confusionMatrix(Validation_data1$status_time, Classification_predict, positive = "Default")



######################################################################
# Data preparation for goal -2
str(paidoff_default)

paidoff_default_update = paidoff_default[, -c(1,2,3,4,5,6,7,8,9,10)]
head(as.data.frame(paidoff_default_update))
Data_partition = createDataPartition(paidoff_default_update$status_time, p =0.7, list = FALSE)
train_data2 = paidoff_default_update[Data_partition, ]
Validation_data2 = paidoff_default_update[-Data_partition, ]

head(train_data2)
head(Validation_data2)
table(train_data2$status_time)
table(Validation_data2$status_time)



#########################################################################################
# Logistic for goal-2
# Fit logistic regression model
Logistic_Model_2 = glm(status_time ~ ., data = train_data2, family = binomial)

summary(Logistic_Model_2)

# Predict the outcome using the logistic regression model
Logistic_predict_2 = predict(Logistic_Model_2, newdata = Validation_data2, type = "response")

# Convert predicted probabilities to predicted class labels (0 or 1)
predicted_labels_2 = as.factor(ifelse(Logistic_predict_2 > 0.5, 1, 0))

predicted_labels_2 = factor(predicted_labels_2, levels = c("0","1"), labels = c("Default","Paidoff"))

Validation_data2$status_time = factor(Validation_data2$status_time, levels = c("0","1"), labels = c("Default","Paidoff"))
levels(Validation_data2$status_time)
levels(predicted_labels_2)

confusionMatrix(Validation_data2$status_time, predicted_labels_2, positive = "Paidoff")


#######################################################################################################

# classification Tree for goal-2

Classification.model_2 = rpart(status_time ~ ., data = train_data2, method = "class")
summary(Classification.model_2)

Classification_predict_2 = predict(Classification.model_2, newdata = Validation_data2, type = "class")
Classification_predict_2 = factor(Classification_predict_2, levels = c("0","1"), labels = c("Default","Paidoff"))
levels(Validation_data2$status_time)
levels(Classification_predict_2)

confusionMatrix(Validation_data2$status_time, Classification_predict_2, positive = "Paidoff")




############################################################################################################

# Prediction of active borrowers for goal-1 using holdout set

Holdout_predict = predict(Logistic_Model, newdata = Holdout.df, type = "response")
# Convert predicted probabilities to predicted class labels (0 or 1)
Holdout_predict = as.factor(ifelse(Holdout_predict > 0.5, 1, 0))
Holdout_predict = factor(Holdout_predict, levels = c("0","1"), labels = c("Default","Paidoff"))

table(Holdout_predict)
# Create the bar plot
bp2 = barplot(table(Holdout_predict), ylim = c(0, max(table(Holdout_predict)) + 500), col = "lightblue",
        main = "Holdout Predictions", xlab = "Categories", ylab = "Frequency")

# Add text on top of each bar
text(x = bp2, y = table(Holdout_predict) + 200, labels = table(Holdout_predict), cex = 1.0)


#############################################################################################################

# Prediction of new customers for goal-2 using holdout set
holdout_data2 = Holdout.df[,-c(1,2,3,4,5,6,7,8,9,10)]
head(holdout_data2)

Holdout_predict_2 = predict(Logistic_Model_2, newdata = holdout_data2, type = "response")

Holdout_predict_2 = as.factor(ifelse(Holdout_predict_2 > 0.5, 1, 0))

Holdout_predict_2 = factor(Holdout_predict_2, levels = c("0","1"), labels = c("Default","Paidoff"))
table(Holdout_predict_2)
# Create the bar plot
bp3 = barplot(table(Holdout_predict_2), ylim = c(0, max(table(Holdout_predict_2)) + 500), col = "lightblue",
              main = "Holdout Predictions", xlab = "Categories", ylab = "Frequency")

# Add text on top of each bar
text(x = bp3, y = table(Holdout_predict_2) + 200, labels = table(Holdout_predict_2), cex = 1.0)
