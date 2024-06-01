library(tidyverse)
library(ggplot2)

orders <- read_csv("orders.csv")

#2.
orders_factorized <- orders %>%
  mutate(
    State = factor(State),
    Region = factor(Region),
    Market = factor(Market),
    Category = factor(Category),
    SubCategory = factor(SubCategory),
    OrderPriority = factor(OrderPriority)
  )
#3.
scatterplot <- ggplot(orders_factorized, aes(x = Sales, y = Profit,
                                             color = Region)) + geom_point() +
  labs(title = "Profit vs. Sales by Region", 
       x = "Sales",
       y = "Profit",
       color = "Region")

ggsave(filename = "scatterplot.png", plot = last_plot(), width = 10, height = 6, dpi =300)
print(scatterplot)

#4.
scatterplot_categories <- ggplot(orders_factorized, aes(x = Sales, y = Profit, color = Category )) +
  geom_point() +
  labs(title = "Profit vs. Sales by Category",
       x = "Sales",
       y = "Profit",
       color = "Category")

ggsave(filename = "scatterplot_categories.png", plot = last_plot, width = 10, height = 6, dpi = 300)
print(scatterplot_categories)

#5.
sales_summary <- orders_factorized %>%
  group_by(Region, Category) %>%
  summarise(total_quantity = sum(Quantity))

bar_chart <- ggplot(sales_summary, aes(x = Region, y = total_quantity, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales Quantity by Region and Category",
       x = "Region",
       y= "Total Quantity",
       fill = "Category")

ggsave("bar_chart.png", plot = last_plot, width = 10, height = 6, dpi = 300)
print(bar_chart)

#6.
sales_histogram <- ggplot(orders_factorized, aes(x = Sales)) + 
  geom_histogram(binwidth = 250, origin = 0, fill = "skyblue", color = "Black") +
  labs(title = "Histogram of Sales",
       x = "Sales",
       y = "Frequency")

ggsave("sales_histogram.png", plot = last_plot, width = 10, height = 6, dpi = 300)
print(sales_histogram)

#7.
order_counts <- orders_factorized %>%
  group_by(Market, SubCategory) %>%
  summarise(total_orders = n())

stacked_bar_chart <- ggplot(order_counts, aes(x = Market, y = total_orders, fill = SubCategory)) +
  geom_bar(stat = "identity") + 
  labs(title = "Order Count by Market and SubCategory",
       x = "Market",
       y = "Total Orders",
       fill = "SubCategory")

ggsave("stacked_bar_chart.png", plot = last_plot, width = 10, height = 6, dpi = 300)
print(stacked_bar_chart)

#8.
jitter_plot <- ggplot(orders_factorized, aes(x =Discount, y = Sales, color = Category)) +
  geom_jitter()+
  labs(title = "Jitter Plot of Discount vs Sales",
       x = "Discount",
       y = "Sales",
       color = "Category")

ggsave("jitter_plot.png", plot = last_plot, width = 10, height = 6, dpi = 300)
print(jitter_plot)

#10.
lm_model <- lm(Profit ~ Sales, data = orders_factorized)
summary(lm_model)

#11
lm_model_multiple <- lm(Profit ~ Sales + Discount, data = orders_factorized)
summary(lm_model_multiple)
