library(dplyr)
library(lubridate)
library(knitr)
library(readr)
library(ggplot2)
library(shiny)

# Step-1: Load the data from the CSV file
ticket_data <- read.csv("jira_analytics_data.csv")

# Step-2: Check for missing values in the columns tickets_opened and ticket_closed
print(sum(is.na(ticket_data$month_opened)))  # Check for missing values in month_opened
print(sum(is.na(ticket_data$month_closed)))  # Check for missing values in month_closed

str(ticket_data)

# Step-3: Convert date/time columns to Date objects
ticket_data <- ticket_data %>%
  mutate(
    month_opened = as.Date(month_opened, format = "%d/%m/%Y"),
    month_closed = as.Date(month_closed, format = "%d/%m/%Y")
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
  select(month_opened, month_closed, resolution_time)

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

# Step-9: Visualization: Distribution of Resolution Times
ggplot(tickets, aes(x = resolution_time)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Ticket Resolution Times",
    x = "Resolution Time (Days)",
    y = "Number of Tickets"
  ) +
  theme_minimal()

# Visualization: Resolution Time per Ticket ID
ggplot(tickets, aes(x = ticket_id, y = resolution_time)) +
  geom_point(color = "darkred", alpha = 0.6) +
  labs(
    title = "Resolution Time for Each Ticket",
    x = "Ticket ID",
    y = "Resolution Time (Days)"
  ) +
  theme_minimal()
  
#We will want to understand the tickets that were excluded from resolution calculation
# Extract ticket_id for excluded rows
excluded_ticket_ids <- ticket_data %>%
  mutate(
    resolution_time = as.numeric(month_closed - month_opened)
  ) %>%
  filter(
    is.na(resolution_time) | resolution_time < 0  # Identify invalid rows
  ) %>%
  select(ticket_id)  # Select only the ticket_id column

# Check if there are excluded tickets
if (nrow(excluded_ticket_ids) > 0) {
  # Print the message and the excluded ticket IDs
  print("Following tickets have been excluded due to invalid values for calculating resolution time:")
  print(excluded_ticket_ids)
} else {
  # Message if no tickets are excluded
  print("No tickets were excluded; all tickets have valid resolution time data.")
}
