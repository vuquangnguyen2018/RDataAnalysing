setwd("G:/Other computers/My Computer/Green Food/4. MISA Report")

# package, hear, utf8, dplyr

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

dCustomer <- read_excel("Khach_hang.xlsx", 
                        skip = 1)
#------------

colnames(dCustomer) <- c("Mã khách hàng", "Tên khách hàng", "D???a ch???", "Di???n tho???i", "DT di d???ng", "Nhân viên", "Tên nhân viên", "Nhóm KH, NCC", "Mã s??? thu???", "TK ngân hàng", "Tên ngân hàng", "T??? ch???c/Cá nhân", "Ng???ng theo dõi")

dCustomer <- dCustomer %>%
  rename(id_customer = "Mã khách hàng",
         customer_name = "Tên khách hàng",
         address = "D???a ch???",
         phone = "Di???n tho???i",
         id_sales = "Nhân viên",
         sales = "Tên nhân viên",
         type_customer = "Nhóm KH, NCC")

dCustomer <- dCustomer %>%
  select(id_customer, customer_name, address, phone, id_sales, sales, type_customer)
View(dCustomer)



dStaff <- read_excel("Nhan_vien.xlsx", 
                     skip = 1)
#View(dStaff)

dStaff <- dStaff %>%
  rename(id_customer = `Mã nhân viên`,
         customer_name = `Tên nhân viên`) %>%
  select(id_customer, customer_name)

#----------------------------

dCustomer<-bind_rows(dCustomer, dStaff) %>%
  filter(!is.na(customer_name)) %>%
  mutate(
    id_sales = if_else(is.na(id_sales), "NV00008", id_sales),
    sales = if_else(is.na(sales), "NV Nguyên (Sale Admin)", sales),
    type_customer = if_else(is.na(type_customer),"KL_CuaHang",type_customer)
  )



View(dCustomer)

saveRDS(dCustomer, "dCustomer.rds")

#---------------------- Sales

fSales <- read_excel("SO_CHI_TIET_BAN_HANG.xlsx", 
                     skip = 2)


# Use clean_names to clean column names
library(janitor)

# Use clean_names to clean column names
fSales <- fSales %>%
  clean_names() %>%
  rename(
    date_order = ngay_hach_toan,
    id_order = so_chung_tu,
    warehouse = ten_kho,
    id_product = ma_hang,
    product = ten_hang,
    id_customer = ma_khach_hang,
    customer_name = ten_khach_hang,
    unit_price = don_gia,
    qty_sales = tong_so_luong_ban,
    unit = dvt,
    amount = doanh_so_ban,
    qty_return = tong_so_luong_tra_lai,
    gross = thanh_tien
  ) %>%
  select(date_order, id_order, warehouse, id_product, product, id_customer, customer_name, qty_sales, qty_return, unit, unit_price, amount, gross)%>%
  filter(!is.na(id_order))

library(lubridate)

# Convert date_order to date-time format
fSales$date_order <- as.Date(as.numeric(fSales$date_order), origin = "1899-12-30")

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


# Extract month and year, day from date_order
fSales$month <- month(fSales$date_order)
fSales$year <- year(fSales$date_order)
fSales$day <- day(fSales$date_order)



# Group by month and calculate distinct orders and sum of gross
summary_data <- fSales %>%
  group_by(month) %>%
  summarise(
    distinct_orders = n_distinct(id_order),
    total_gross = sum(gross, na.rm = TRUE)
  )

# Create a ggplot with formatted x-axis
ggplot(summary_data, aes(x = month, y = total_gross, fill = as.factor(month))) +
  geom_col() +
  geom_text(aes(label = distinct_orders), vjust = -0.5, size = 3) +
  labs(x = "Month", y = "Total Gross", title = "Distinct Orders and Total Gross by Month") +
  scale_x_continuous(breaks = unique(summary_data$month)) +  # Format x-axis as whole numbers
  theme_minimal()



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


#----------- Receivable
fReceivable <- read_excel("TONG_HOP_CONG_NO_PHAI_THU.xlsx", 
                                        skip = 2)


fSales_month
fReceivable
View(fReceivable)



 