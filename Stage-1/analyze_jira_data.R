library(dplyr)
library(lubridate)
library(knitr)
library(readr)

# Assuming your CSV files are in a subdirectory named "data" within your project directory
csv_dir <- file.path("data") 

# Check if the directory exists
if (!dir.exists(csv_dir)) {
  stop("Directory '", csv_dir, "' does not exist.")
}

# List files in the directory
csv_files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Check if any CSV files are found
if (length(csv_files) == 0) {
  stop("No CSV files found in the directory.")
}
# Select the first CSV file
csv_file <- csv_files[1]
print(paste("Loading CSV file:", csv_file))

# Load the selected CSV file
ticket_data <- read.csv(csv_file)

# Display the content of the CSV file
print("Loaded CSV data:")
print(head(ticket_data))

# Step-1: Load the data from the CSV file
# ticket_data <- read.csv("jira_analytics_data.csv")

#Renaming the respective columns
colnames(ticket_data)[2]="Issue_key"
colnames(ticket_data)[21]="month_opened"
colnames(ticket_data)[24]="month_closed"

# Create a new column with the month_opened
ticket_data <- ticket_data %>%
  mutate(
    created_at_new = 
      paste(
        day(as.Date(month_opened, format = "%d/%b/%y")), 
        month(as.Date(month_opened, format = "%d/%b/%y")), 
        year(as.Date(month_opened, format = "%d/%b/%y")), 
        sep = "/"
      )
  )

# Create a new column with the desired format
ticket_data <- ticket_data %>%
  mutate(
    closed_at = 
      paste(
        day(as.Date(month_closed, format = "%d/%b/%y")), 
        month(as.Date(month_closed, format = "%d/%b/%y")), 
        year(as.Date(month_closed, format = "%d/%b/%y")), 
        sep = "/"
      )
  )
  
  print(ticket_data$created_at_new)
  
  # Step-4: Convert date/time columns to Date objects
ticket_data <- ticket_data %>%
  mutate(
    month_opened = as.Date(ticket_data$created_at_new, format = "%d/%m/%Y"),
    month_closed = as.Date(ticket_data$closed_at, format = "%d/%m/%Y")
  )

# Step-4: Tickets opened per month
tickets_opened_per_month <- ticket_data %>%
  mutate(open_month = floor_date(month_opened, "month")) %>%  # Group by full months
  group_by(open_month) %>%
  summarize(opened_tickets = n(), .groups = "drop")
  
# Step-5: Tickets closed per month
tickets_closed_per_month <- ticket_data %>%
  mutate(close_month = floor_date(month_closed, "month")) %>%  # Group by full months
  group_by(close_month) %>%
  summarize(closed_tickets = n(), .groups = "drop")
   
#Tickets opened
kable(tickets_opened_per_month, 
      col.names = c("Month", "Number of Tickets Opened"), 
      align = "c", 
      caption = "Tickets Opened Per Month")
      
#Tickets closed
kable(tickets_closed_per_month, 
      col.names = c("Month", "Number of Tickets Closed"), 
      align = "c", 
      caption = "Tickets Closed Per Month")     
 
 # Step-6: Calculate Resolution Time in Days
ticket_data <- ticket_data %>%
mutate(
    month_opened = as.Date(month_opened, format = "%d/%m/%Y"), 
    month_closed = as.Date(month_closed, format = "%d/%m/%Y"),
    resolution_time = as.numeric(difftime(month_closed, month_opened, units = "days")) 
  ) 
 
 #printing resolution time for each ticket
 print("Resolution time in days:")
 print(ticket_data$resolution_time)
 
# Step-7: Filter invalid cases where close_date is earlier than open_date
tickets <- ticket_data %>%
  filter(resolution_time >= 0)
  
# Create a new data frame with relevant columns
ticket_resolution_data <- ticket_data %>%
  select(Summary, Issue_key, month_opened, month_closed, resolution_time)
  
# Write the data to a new CSV file
write.csv(ticket_resolution_data, "ticket_resolution_statistics.csv", row.names = FALSE)

# Step-8: Summary of Resolution Times
resolution_summary <- tickets %>%
  summarise(
    total_tickets = n(),
    avg_resolution_time = mean(resolution_time, na.rm = TRUE),
    median_resolution_time = median(resolution_time, na.rm = TRUE),
    min_resolution_time = min(resolution_time, na.rm = TRUE),
    max_resolution_time = max(resolution_time, na.rm = TRUE)
  )

kable(
  resolution_summary, 
  col.names = c("Total Tickets", "Average Resolution Time (Days)", 
                "Median Resolution Time (Days)", "Minimum Resolution Time (Days)", 
                "Maximum Resolution Time (Days)"),
  align = "c",
  caption = "Summary of Resolution Times"
)
  
#Tickets that were excluded from resolution calculation
# Extract ticket_id for excluded rows
excluded_ticket_ids <- ticket_data %>%
  mutate(
    resolution_time = as.numeric(month_closed - month_opened)
  ) %>%
  filter(
    is.na(resolution_time) | resolution_time < 0  # Identify invalid rows
  ) %>%
  select(Issue_key)  # Select only the ticket_id column

# Check if there are excluded tickets
if (nrow(excluded_ticket_ids) > 0) {
  # Print the message and the excluded ticket IDs
  print("Following tickets have been excluded due to invalid values for calculating resolution time:")
  print(excluded_ticket_ids)
} else {
  # Message if no tickets are excluded
  print("No tickets were excluded; all tickets have valid resolution time data.")
}
