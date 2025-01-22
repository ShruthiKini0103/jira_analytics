library(dplyr)
library(lubridate)

# Load your data (replace "your_file.csv" with the actual file path)
ticket_data <- read.csv("ticket_closed.csv") 

# Create a new column with the desired format
ticket_data <- ticket_data %>%
  mutate(
    closed_at = 
      paste(
        day(as.Date(closed_at, format = "%d/%b/%y")), 
        month(as.Date(closed_at, format = "%d/%b/%y")), 
        year(as.Date(closed_at, format = "%d/%b/%y")), 
        sep = "/"
      )
  )

# Save the modified data to a new CSV file
write.csv(ticket_data, "ticket_closed_formatted.csv", row.names = FALSE) 
