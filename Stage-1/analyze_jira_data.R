library(dplyr)
library(lubridate)
library(knitr)
# Step-1: Load the data from the CSV file
ticket_data <- read.csv("jira_analytics_data.csv")
# Step-2: Check for missing values (optional)
print(sum(is.na(ticket_data$month_opened)))  # Check for missing values in month_opened
# Step-3: Convert date/time columns to POSIXct objects
month_opened <- as.Date(ticket_data$month_opened, format = "%Y/%m/%d")
month_closed <- as.Date(ticket_data$month_closed, format = "%Y/%m/%d")
# 1. Tickets opened per month
tickets_opened_per_month <- ticket_data %>%
  mutate(month_opened = month(month_opened)) %>%  # Extract month as a number (1-12)
  group_by(month_opened) %>%
  summarize(opened_tickets = n(), .groups = "drop")
 
# 2. Tickets closed per month
tickets_closed_per_month <- ticket_data %>%
  mutate(month_closed = month(month_closed)) %>%  # Extract month as a number (1-12)
  group_by(month_closed) %>%
  summarize(closed_tickets = n(), .groups = "drop")  
#print date/time conversion for opened tickets
print(month_opened)
# Create a table using kable() to track opened tickets
kable(tickets_opened_per_month, 
      col.names = c("Month", "Number of Tickets Opened"), 
      align = "c", 
      caption = "Tickets Opened Per Month") 
      
#print date/time conversion for closed tickets
print(month_closed)
# Create a table using kable() to track closed tickets
kable(tickets_closed_per_month, 
      col.names = c("Month", "Number of Tickets Closed"), 
      align = "c", 
      caption = "Tickets Closed Per Month")
