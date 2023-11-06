# Kevin Ryan
# Victory as a Service 
# JHU, Fall '23

# Install packages
library(readxl)
library(dplyr)
library(lubridate)
library(plotly)
library(caret)
library(yardstick)
library(purrr)
library(pillar)
library(cli)
library(reticulate)
library(ggplot2)
library(ggthemes)



use_python("/Users/test/opt/anaconda3/bin/python3")

Sys.which("python")

################################################################################

# Define the file path 
excel_file_path <- "/Users/test/Desktop/Capstone/ua_aid_revised.xlsx"

# Read in the Excel file
aid <- read_excel(excel_file_path)

# View head
head(aid)

# Convert "date" to Date
aid$date <- as.Date(aid$date)

# Check the data types
data_types_in_aid <- sapply(aid, class)
print(data_types_in_aid)

################################################################################

# Define the file path
second_file_path <- "/Users/test/Desktop/Capstone/russia_losses_personnel.csv"

# Read in the CSV 
rkia <- read.csv(second_file_path)

# View head
head(rkia)

# Convert the "date" column 
rkia$date <- as.Date(rkia$date)

# Check the data types 
rkia_data_types <- sapply(rkia, class)
print(rkia_data_types)

################################################################################

# Define a range from 2/25/22 to 10/08/23
date_range <- seq(from = ymd("2022-02-25"), to = ymd("2023-10-08"), by = "days")

# Create the "skeleton" 
skeleton <- data.frame(date = date_range)

################################################################################

# Left join "aid" and "skeleton" on "date" 
merged_data <- left_join(skeleton, aid, by = "date")

# Left join "rkia" to "merged_data" on "date" 
final_merged_data <- left_join(merged_data, rkia, by = "date")

# Add the "campaign" column 
final_merged_data <- final_merged_data %>%
  mutate(campaign = case_when(
    date >= as.Date("2022-02-24") & date <= as.Date("2022-04-07") ~ "Initial Invasion",
    date >= as.Date("2022-04-08") & date <= as.Date("2022-08-28") ~ "Southeastern Front",
    date >= as.Date("2022-08-29") & date <= as.Date("2022-11-11") ~ "Counteroffensive",
    date >= as.Date("2022-11-12") & date <= as.Date("2023-03-28") ~ "First Stalemate",
    date >= as.Date("2023-03-29") & date <= as.Date("2023-10-08") ~ "Second Stalemate",
    TRUE ~ NA_character_
  ))

# Make all the NAs 0
final_merged_data <- final_merged_data %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Force date as.date
#final_merged_data$date <- as.Date(final_merged_data$date)

# Seriously?
final_merged_data$date <- as.Date(final_merged_data$date, origin = "1970-01-01")

################################################################################

# Overall linear regression
regression_model <- lm(daily_total ~ total, data = final_merged_data)
summary(regression_model)

# Subset "Initial Invasion" and perform LR
initial_invasion_data <- final_merged_data %>% filter(campaign == "Initial Invasion")
initial_invasion_regression <- lm(daily_total ~ total, data = initial_invasion_data)
summary(initial_invasion_regression)

# Subset "Southeastern Front" and perform LR
southeastern_data <- final_merged_data %>%
  filter(campaign == "Southeastern Front")
regression_model_southeastern <- lm(daily_total ~ total, data = southeastern_data)
summary(regression_model_southeastern)

# Subset "Counteroffensive" and perform LR
counteroffensive_data <- final_merged_data %>% filter(campaign == "Counteroffensive")
regression_model_counteroffensive <- lm(daily_total ~ total, data = counteroffensive_data)
summary(regression_model_counteroffensive)

# Subset "First Stalemate" and perform LR
filtered_data <- final_merged_data %>%
  filter(campaign == "First Stalemate")
regression_model_fs <- lm(daily_total ~ total, data = filtered_data)
summary(regression_model_fs)

# Subset "Second Stalemate" and perform LR
second_stalemate_data <- final_merged_data %>%
  filter(campaign == "Second Stalemate")
regression_model_second_stalemate <- lm(daily_total ~ total, data = second_stalemate_data)
summary(regression_model_second_stalemate)

################################################################################

# Plot daily Russian KIA
rkia_time <- plot_ly(data = final_merged_data, x = ~date, y = ~daily_total, color = ~campaign, type = 'scatter', mode = 'lines')

# Customize the layout
rkia_time <- layout(rkia_time, title = "Daily Russian KIA Over Time by Campaign", xaxis = list(title = "Date"), yaxis = list(title = "Daily KIA"))
rkia_time

# Plot "mil_val_delivered" and "hum_aid_delivered"
aid_time <- plot_ly(data = final_merged_data, x = ~date) %>%
  add_trace(y = ~mil_val_delivered, name = "Military Aid Delivered", line = list(color = "blue"), marker = list(color = "blue")) %>%
  add_trace(y = ~hum_aid_delivered, name = "Humanitarian Aid Delivered", line = list(color = "green"), marker = list(color = "green")) %>%
  layout(title = "Aid Over Time - Military and Humanitarian",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Aid Delivered", range = c(0, 1e10)))

# Customize layout and limit Y
aid_time <- aid_time %>%
  layout(legend = list(x = 0.8, y = 1),
         showlegend = TRUE)
aid_time

# Plot both on the same visualization
combined_plot <- plot_ly(data = final_merged_data, x = ~date) %>%
  # Russian KIA
  add_trace(y = ~daily_total, name = "Russian KIA", color = ~campaign, type = 'scatter', mode = 'lines') %>%
  # Military Aid Delivered
  add_trace(y = ~mil_val_delivered, name = "Military Aid Delivered", line = list(color = "blue"), marker = list(color = "blue")) %>%
  # Humanitarian Aid Delivered
  add_trace(y = ~hum_aid_delivered, name = "Humanitarian Aid Delivered", line = list(color = "green"), marker = list(color = "green")) %>%
  layout(title = "Russian KIA and Aid Over Time", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Counts/Delivered Aid", range = c(0, 1e10))) %>%
  layout(legend = list(x = 0.8, y = 1, showlegend = TRUE))

combined_plot         

################################################################################

# Define a list of campaign categories
campaign_categories <- c("Initial Invasion", "Southeastern Front", "Counteroffensive", "First Stalemate", "Second Stalemate")

# Create an empty list to store regression results
monthly_regression_results <- list()

# Loop through each campaign category
for (campaign in campaign_categories) {
  # Subset the data for the current campaign
  campaign_data <- final_merged_data %>% filter(campaign == campaign)
  
  # Create an empty list to store monthly regression results for the current campaign
  campaign_monthly_results <- list()
  
  # Loop through each month
  for (month in unique(format(campaign_data$date, "%Y-%m"))) {
    # Subset the data for the current month and campaign
    month_campaign_data <- campaign_data %>% filter(format(date, "%Y-%m") == month)
    
    # Check if there are enough data points for regression
    if (nrow(month_campaign_data) >= 2) {  # You may adjust the minimum number of data points needed
      # Perform a simple linear regression for that month and campaign
      regression_model <- lm(daily_total ~ total, data = month_campaign_data)
      
      # Store the regression results for the current month
      campaign_monthly_results[[month]] <- summary(regression_model)
    }
  }
  
  # Store the monthly results for the current campaign
  monthly_regression_results[[campaign]] <- campaign_monthly_results
}

################################################################################


# Define a list of campaign categories
campaign_categories <- c("Overall", "Initial Invasion", "Southeastern Front", "Counteroffensive", "First Stalemate", "Second Stalemate")

# Create an empty list to store regression results by campaign
campaign_regression_results <- list()

# Loop through each campaign category
for (campaign in campaign_categories) {
  # Subset the data for the current campaign
  if (campaign == "Overall") {
    campaign_data <- final_merged_data
  } else {
    campaign_data <- final_merged_data %>% filter(campaign == campaign)
  }
  
  # Perform a simple linear regression for the current campaign
  regression_model <- lm(daily_total ~ total, data = campaign_data)
  
  # Store the regression results in the list
  campaign_regression_results[[campaign]] <- summary(regression_model)
}

# Print the results
for (campaign in campaign_categories) {
  cat("Campaign:", campaign, "\n")
  print(campaign_regression_results[[campaign]])
  cat("\n")
}

################################################################################

library(dplyr)

# Replace NAs with 0s in specified columns
final_merged_data <- final_merged_data %>%
  mutate(
    mil_val_delivered = ifelse(is.na(mil_val_delivered), 0, mil_val_delivered),
    hum_aid_delivered = ifelse(is.na(hum_aid_delivered), 0, hum_aid_delivered),
    total = ifelse(is.na(total), 0, total)
  )

# Group the data by month
final_merged_data <- final_merged_data %>%
  group_by(month = format(date, "%Y-%m")) %>%
  ungroup()

# Create an empty list to store regression results
monthly_regression_results <- list()

# Perform linear regressions by month
unique_months <- unique(final_merged_data$month)
for (month in unique_months) {
  # Subset the data for the current month
  month_data <- final_merged_data %>% filter(month == month)
  
  # Perform a simple linear regression for the current month
  regression_model <- lm(daily_total ~ total, data = month_data)
  
  # Store the regression results for the current month
  monthly_regression_results[[month]] <- summary(regression_model)
}

# Print the results for each month
for (month in unique_months) {
  cat("Month:", month, "\n")
  print(monthly_regression_results[[month]])
  cat("\n")
}

################################################################################

# Filter the training data for the first 4 campaign phases
training_data <- final_merged_data %>%
  filter(date >= "2022-02-24" & date <= "2023-03-28") %>%
  select(-campaign)  # Exclude the "campaign" column

# Filter the test data for Second Stalemate
test_data <- final_merged_data %>%
  filter(date >= "2023-03-29" & date <= "2023-10-08") %>%
  select(-campaign)  # Exclude the "campaign" column

#formula
formula <- daily_total ~ .  

# Train the model using Random Forest
model <- train(formula, data = training_data, method = "rf")

# Abracadabra
predictions <- predict(model, newdata = test_data)

# Calculate performance metrics
rmse <- RMSE(predictions, test_data$daily_total)
mae <- MAE(predictions, test_data$daily_total)
rsquared <- R2(predictions, test_data$daily_total)

# View metrics
print(rmse)
print(mae)
print(rsquared)

# Visualize Results
plot(predictions, test_data$target_variable, main = "Predicted vs. Actual")

################################################################################

# Define the file paths
civ_cas_file_path <- "/Users/test/Desktop/Capstone/ua_civ_cas.xlsx"
gain_loss_file_path <- "/Users/test/Desktop/Capstone/ua_gain_loss.xlsx"

# Read in the Excel
ua_civ_cas <- read_excel(civ_cas_file_path)
ua_gain_loss <- read_excel(gain_loss_file_path)

# Check data types
data_types_in_civ_cas <- sapply(ua_civ_cas, class)
print(data_types_in_civ_cas)

data_types_in_gain_loss <- sapply(ua_gain_loss, class)
print(data_types_in_gain_loss)

# Force dates
ua_civ_cas$date <- as.Date(ua_civ_cas$date)
ua_gain_loss$date <- as.Date(ua_gain_loss$date)

################################################################################

# Left join "ua_civ_cas" to "final_merged_data" on "date"
final_merged_data <- left_join(final_merged_data, ua_civ_cas, by = "date")

# Left join "ua_gain_loss" to "final_merged_data" on "date"
final_merged_data <- left_join(final_merged_data, ua_gain_loss, by = "date")

# NA-NA, NA-NANAAAAAA...
final_merged_data <- final_merged_data %>%
  mutate(civ_kill = ifelse(is.na(civ_kill), 0, civ_kill),
         r_gain = ifelse(is.na(r_gain), 0, r_gain),
         r_loss = ifelse(is.na(r_loss), 0, r_loss))

################################################################################

# Fit a linear regression model
reg_1 <- lm(civ_kill ~ total, data = final_merged_data)

# Display the summary 
summary(reg_1)

################################################################################

# Create the fmd_month dataframe by grouping and summarizing by month
fmd_month <- final_merged_data %>%
  group_by(month = format(date, "%Y-%m")) %>%
  summarise(
    total = sum(total),
    daily_total = sum(daily_total),
    mil_val_delivered = sum(mil_val_delivered),
    hum_aid_delivered = sum(hum_aid_delivered),
    civ_kill = sum(civ_kill),
    r_gain = sum(r_gain),
    r_loss = sum(r_loss)
  )

# Check it
head(fmd_month)

data_types_in_fmd_month <- sapply(fmd_month, class)
print(data_types_in_fmd_month)

# Fix the formats and NAs
fmd_month$month <- as.Date(paste0(fmd_month$month, "-01"), format = "%Y-%m-%d")

int_columns <- c("total", "daily_total", "mil_val_delivered", "hum_aid_delivered", "civ_kill", "r_gain", "r_loss")
fmd_month[int_columns] <- lapply(fmd_month[int_columns], as.integer)

fmd_month$total[is.na(fmd_month$total)] <- 0
fmd_month$mil_val_delivered[is.na(fmd_month$mil_val_delivered)] <- 0
fmd_month$hum_aid_delivered[is.na(fmd_month$hum_aid_delivered)] <- 0

data_types_in_fmd_month <- sapply(fmd_month, class)
print(data_types_in_fmd_month)

################################################################################

# Total Aid on Civilians Killed
reg_2 <- lm(civ_kill ~ total, data = fmd_month)
summary(reg_2)

# Total Aid on Russian KIA
reg_3 <- lm(daily_total ~ total, data = fmd_month)
summary(reg_3)

# Total Aid on Russian Territorial Losses
reg_4 <- lm(r_loss ~ total, data = fmd_month)
summary(reg_4)

# Russian KIA on Russian Territorial Losses
reg_5 <- lm(r_loss ~ daily_total, data = fmd_month)
summary(reg_5)

################################################################################

# Plot mil_val_delivered over time
ggplot(data = fmd_month, aes(x = month, y = mil_val_delivered)) +
  geom_line(color = "green") +
  labs(title = "Military Aid Delivered Over Time",
       x = "Month",
       y = "Military Aid Delivered") +
  theme_economist()

# Plot hum_aid_delivered over time
ggplot(data = fmd_month, aes(x = month, y = hum_aid_delivered)) +
  geom_line(color = "orange") +
  labs(title = "Monetery and Humanitarian Aid Delivered Over Time",
       x = "Month",
       y = "Monetary and Humanitarian Aid Delivered") +
  theme_economist()

# Plot daily_total over time
ggplot(data = fmd_month, aes(x = month, y = daily_total)) +
  geom_line(color = "red") +
  labs(title = "Russian Service Members Killed in Action (KIA) Over Time",
       x = "Month",
       y = "KIA") +
  theme_economist()

# Load necessary libraries
library(ggplot2)

# Plot hum_aid_delivered, mil_aid_delivered, and daily_total on the same chart
ggplot(data = fmd_month, aes(x = month)) +
  geom_line(aes(y = hum_aid_delivered, color = "Monetary and Humanitarian Aid"), size = 1) +
  geom_line(aes(y = mil_val_delivered, color = "Military Aid"), size = 1) +
  geom_line(aes(y = daily_total, color = "Russian KIA"), size = 1) +
  labs(title = "Aid and Russian KIA Over Time",
       x = "Month") +
  scale_y_continuous(name = "Humanitarian Aid Delivered",
                     sec.axis = sec_axis(~., name = "Military Aid Delivered")) +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal() +
  theme(legend.position = "top")