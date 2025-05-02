# Install required packages if not already installed
if (!require("quantmod")) install.packages("quantmod")
if (!require("dplyr")) install.packages("dplyr")
if (!require("zoo")) install.packages("zoo")
if (!require("lubridate")) install.packages("lubridate")

# Load libraries
library(quantmod)
library(dplyr)
library(zoo)
library(lubridate)

# Set start and end dates
start_date <- as.Date("2018-11-07")
end_date <- as.Date("2023-11-06")

# Download VNQ data
getSymbols("VNQ", src = "yahoo", from = start_date, to = end_date)

# Extract price data
vnq_all_prices <- VNQ[, 6]  # Column 6 is adjusted close price
colnames(vnq_all_prices) <- "Adjusted_Close"

# Convert to data frame
vnq_all_prices_df <- data.frame("Date" = index(vnq_all_prices),
                               "Price" = coredata(vnq_all_prices))

colnames(vnq_all_prices_df) [2]<- "Price"

# Get the first day of each month
vnq_all_prices_df$Year_Month <- floor_date(vnq_all_prices_df$Date, "month")


# Group by Year_Month and get the last price for each month
vnq_monthly_last_price <- vnq_all_prices_df %>%
  group_by(Year_Month) %>%
  filter(Date == max(Date)) %>%
  ungroup()


# For each month, get the first available trading day
# vnq_prices <- vnq_all_prices_df %>%
#   group_by(Year_Month) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(Date, Price)

# Get dividend data
vnq_dividends <- getDividends("VNQ", from = start_date, to = end_date)

# Convert to data frame
vnq_dividends_df <- data.frame(Date = index(vnq_dividends),
                              Dividend = coredata(vnq_dividends))
colnames(vnq_dividends_df) [2]<- "Dividend"

# Add year and month columns
vnq_dividends_df$Year_Month <- floor_date(vnq_dividends_df$Date, "month")

# Sum dividends by month
monthly_dividends <- vnq_dividends_df %>%
  group_by(Year_Month) %>%
  summarize(Monthly_Dividend_Sum = sum(Dividend, na.rm = TRUE)) %>%
  ungroup()

# Merge monthly prices and dividends by year-month
vnq_monthly_data <- vnq_monthly_last_price %>%
  mutate(Year_Month = floor_date(Date, "month")) %>%
  left_join(monthly_dividends, by = "Year_Month") %>%
  select(Date, Price, Monthly_Dividend_Sum, Year_Month)

# Replace NA values with 0 for months with no dividends
vnq_monthly_data$Monthly_Dividend_Sum[is.na(vnq_monthly_data$Monthly_Dividend_Sum)] <- 0

# View the data
print(head(vnq_monthly_data))

# Export to CSV
#write.csv(vnq_monthly_data, "vnq_monthly_data.csv", row.names = FALSE)

# Optional: Calculate monthly returns
# Calculate monthly returns where the first month will be NA
vnq_monthly_returns <- vnq_monthly_data %>%
  arrange(Date) %>%
  mutate(
    Previous_Month_Price = lag(Price),
    Previous_Month_Date = lag(Date),
    VNQ_Monthly_Return = (Price - Previous_Month_Price + Monthly_Dividend_Sum) / Previous_Month_Price
  ) %>%
  select(Date, Previous_Month_Date, Previous_Month_Price, Price, Monthly_Dividend_Sum, VNQ_Monthly_Return)

# The first row will have NA for previous month's price
# Keep all rows but be aware the first entry's return calculation will be NA

# Export monthly returns to CSV
write.csv(vnq_monthly_returns, "vnq_monthly_returns.csv", row.names = FALSE)
str(vnq_monthly_returns)

