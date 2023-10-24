# Kevin Ryan
# Capstone - VaaS 
# JHU, Fall '23

# Install packages
library(readxl)
library(dplyr)
library(lubridate)
library(plotly)

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

# Plotly "mil_val_delivered" and "hum_aid_delivered"
aid_time <- plot_ly(data = final_merged_data, x = ~date) %>%
  add_trace(y = ~mil_val_delivered, name = "Military Aid Delivered", line = list(color = "blue")) %>%
  add_trace(y = ~hum_aid_delivered, name = "Humanitarian Aid Delivered", line = list(color = "green")) %>%
  layout(title = "Aid Over Time - Military and Humanitarian",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Aid Delivered", range = c(0, 1e10)))
# Customize layout and limit Y
aid_time <- aid_time %>%
  layout(legend = list(x = 0.8, y = 1),
         showlegend = TRUE)
aid_time






