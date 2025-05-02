library(reshape2)
library(readxl)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)

# --- Load Price Data and Prepare monthly_last ---
path_px <- "C:/Users/USER/Documents/Projects/Riet Stocks/Data/"
file_name_px <- "DATA_selected.xlsx"
sheet_px <- "PX_LAST"
data_PX_LAST <- read_excel(paste0(path_px, file_name_px), sheet = sheet_px)

long_px <- data_PX_LAST %>%
  pivot_longer(
    cols = -stock,
    names_to = "Date",
    values_to = "Price"
  ) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(Month_full = floor_date(Date, unit = "month"),
         Price = as.numeric(ifelse(Price == '#N/A N/A', NA, Price))) # Keep full month date


# filter the data for the last trading day at each month
monthly_last <- long_px %>%
  group_by(stock, Month_full) %>%
  filter(Date == max(Date)) %>%
  select(stock, Date, Month_full, last_price = Price) %>% # Rename Price to last_price for clarity
  ungroup()


# --- Load Dividend Data and Prepare monthly_summary ---
path_div <- "C:/Users/USER/Documents/Projects/Riet Stocks/Data/"
file_name_div <- "DATA_selected.xlsx"
sheet_div <- "Distributed_Dividends"
data_Distributed_Dividends <- read_excel(paste0(path_div, file_name_div), sheet = sheet_div)

long_div <- data_Distributed_Dividends %>%
  #select(-Sector) %>% # Remove Sector and Currency columns
  pivot_longer(
    cols = c(-stock,-Sector),
    names_to = "Date",
    values_to = "Dividend"
  ) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(Year = year(Date), Month_num = month(Date)) # Extract year and month number

monthly_summary <- long_div %>%
  group_by(stock,Sector, Year, Month_num) %>%
  summarise(
    monthly_sum_dividend = sum(Dividend, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ungroup()

# --- Merge Dividend and Last Price Data ---
merged_data <- monthly_summary %>%
  mutate(
    # Create a matching Month_full column in monthly_summary
    Month_full = make_date(Year, Month_num, 1)
  ) %>%
  left_join(monthly_last, by = c("stock", "Month_full")) %>%
  select(-Year, -Month_num) # Remove the separate year and month number columns

str(return_data)
# --- Calculate the Return ---
return_data <- merged_data %>%
  arrange(stock, Date) %>%
  group_by(stock) %>%
  #arrange(Month_full) %>%
  mutate(
    lag_price = lag(last_price),
    return = (last_price + monthly_sum_dividend - lag_price) / lag_price
  ) %>%
  ungroup()

# Print the resulting data frame with returns
print(return_data)


# Merge the data frames based on the 'Date' column
merged_returns <- return_data %>%
  left_join(vnq_monthly_returns, by = "Date", suffix = c("_stock", "_vnq"))%>%
select(stock,Sector, Date,return, VNQ_Monthly_Return )


# Filter for stocks where their return is higher than VNQ's monthly return
filtered_stocks <- merged_returns %>%
  filter( return>  VNQ_Monthly_Return)


top_10_stocks_monthly <- filtered_stocks %>%
  group_by(Date) %>%
  slice_max(order_by = return, n = 10, with_ties = FALSE) %>%
  ungroup()
