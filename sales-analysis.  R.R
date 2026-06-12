library(tidyverse)
library(lubridate)
library(janitor)

# Load the dataset
df <- read_csv("C:/Users/NOAH B/Downloads/Superstore_sales.csv")


dim(df)           
colnames(df)      
glimpse(df)      
head(df)         

colSums(is.na(df))
sum(duplicated(df))

df <- df %>% clean_names()
colnames(df)

df <- df %>% 
  mutate( order_date = dmy(order_date),
         ship_date = dmy(ship_date) )

glimpse(df %>% select(order_date, ship_date))

df %>% distinct(region)
df %>% distinct(segment)
df %>% distinct(category)

df <- df %>% 
  mutate ( profit_margin = (profit/sales)* 100,
           shipping_days = as.numeric( ship_date - order_date ),
           order_year = year(order_date),
           order_month = month(order_date, label = TRUE),
           year_month  = floor_date(order_date, "month"))

df %>% 
  select(sales, profit, profit_margin, shipping_days, order_year, order_month, year_month) %>% 
  head(10)

kpis <- df %>%
  summarise(
    total_sales    = sum(sales),
    total_profit   = sum(profit),
    total_orders   = n_distinct(order_id),
    avg_order_value = total_sales / total_orders,
    profit_margin  = (total_profit / total_sales) * 100
  )

print(kpis)

df %>%
  group_by(region) %>%
  summarise(
    total_sales  = sum(sales),
    total_profit = sum(profit),
    margin_pct   = (sum(profit) / sum(sales)) * 100,
    orders       = n_distinct(order_id)
  ) %>%
  arrange(desc(total_sales))

df %>%
  group_by(category) %>%
  summarise(
    total_sales  = sum(sales),
    total_profit = sum(profit),
    margin_pct   = (sum(profit) / sum(sales)) * 100
  ) %>%
  arrange(desc(total_sales))

df %>%
  group_by(product_name) %>%
  summarise(
    total_sales  = sum(sales),
    total_profit = sum(profit)
  ) %>%
  arrange(desc(total_profit)) %>%
  head(10)

df %>%
  group_by(product_name) %>%
  summarise(
    total_sales  = sum(sales),
    total_profit = sum(profit)
  ) %>%
  arrange(total_profit) %>%
  head(10)

# Is discounting killing profit?
df %>%
  group_by(discount) %>%
  summarise(
    avg_profit_margin = mean(profit_margin),
    orders = n()
  ) %>%
  arrange(discount)

# Monthly sales trend
df %>%
  group_by(year_month) %>%
  summarise(total_sales = sum(sales)) %>%
  ggplot(aes(x = year_month, y = total_sales)) +
  geom_line(color = "#1D9E75", linewidth = 1) +
  geom_point(color = "#1D9E75", size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Monthly sales trend", x = "Years", y = "Sales") +
  theme_minimal()

# Is Central region discounting more than others?
df %>%
  group_by(region) %>%
  summarise(
    avg_discount = mean(discount) * 100,
    pct_orders_over_30 = mean(discount > 0.3) * 100
  ) %>%
  arrange(desc(avg_discount))

write_csv(df, "superstore_clean.csv")
getwd()

head(df)
