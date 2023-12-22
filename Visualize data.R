
# Extract month and year, day from date_order
fSales$month <- month(fSales$date_order)
fSales$year <- year(fSales$date_order)
fSales$day <- day(fSales$date_order)




library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Convert date_order to date-time format (if not already done)
fSales$date_order <- as.Date(as.numeric(fSales$date_order), origin = "1899-12-30")

# Extract month from date_order
fSales$month <- month(fSales$date_order)

# Group by month and calculate distinct orders and sum of gross
summary_month_sales <- fSales %>%
  group_by(month) %>%
  summarise(
    distinct_orders = n_distinct(id_order),
    total_gross = sum(gross, na.rm = TRUE)
  )

# Create a ggplot with formatted x-axis and y-axis
ggplot(summary_month_sales, aes(x = month, y = total_gross, fill = as.factor(month))) +
  geom_col() +
  geom_text(aes(label = distinct_orders), vjust = -0.5, size = 3) +
  labs(x = "Month", y = "Total Gross (M)", title = "Distinct Orders and Total Gross by Month") +
  scale_x_continuous(breaks = unique(summary_month_sales$month)) +  # Format x-axis as whole numbers
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Format y-axis in millions
  theme_minimal()



# View the updated data
View(fSales)
saveRDS(fSales,"fSales.RDS")
#-------------- Sales


fSales %>%
  group_by(id_order) %>%
  summarize(
    total_sales = sum(amount, na.rm = TRUE),
    total_gross = sum(gross, na.rm = TRUE)
  ) %>%
  summarise(
    total_order = n(),
    total_sales = sum(total_sales, na.rm = TRUE),
    total_gross = sum(total_gross, na.rm = TRUE)
  )


