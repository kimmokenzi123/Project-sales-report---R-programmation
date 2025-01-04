###  Project --- Sales Report 


setwd("/Users/kamalkenzi/Desktop/Data")


# Load the CSV file into a variable
data <- read.csv("data.csv")



# View the first few rows of the dataset
head(data)


##step : verify the Data 

## use head (data) to see the fisrt few rows
head(data)




### use str(data) to check the structure of the dataset

str(data)


### use summary(data) to get a statistical summary

summary(data)


### show column Names 

colnames(data)


### 1.	What is the total sales revenue and total profit for the dataset?
##	Summarize the Sales and Profit columns

# Load necessary library
library(dplyr)


# Summarize total sales revenue and total profit
summary_result <- data %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )

# Print the result
print(summary_result)


### What is the average discount given across all orders?
##	Analyze the Discount column

# Calculate the average discount
average_discount <- data %>%
  summarise(Average_Discount = mean(Discount, na.rm = TRUE))


# Print the result
print(average_discount)

### 3.	How does the quantity of products sold vary across categories?
##Group by Category and calculate total Quantity.

# Group by Category and calculate the total quantity sold for each category
quantity_by_category <- data %>%
  group_by(Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE))


# Print the result
print(quantity_by_category)

### 4.	What are the top 5 most profitable products?
##3	Rank Product.Name by total Profit.


# Group by Product.Name and calculate total profit for each product
top_profitable_products <- data %>%
  group_by(Product.Name) %>%
  summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
  arrange(desc(Total_Profit)) %>%
  slice_head(n = 5) # Select the top 5 products

# Print the result
print(top_profitable_products)


### Time-Based Analysis


### 5.	What is the trend of sales over time?
### Aggregate Sales by Order.Date (monthly or yearly) and create a line plot.


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)


# Ensure Order.Date is in Date format
data$Order.Date <- as.Date(data$Order.Date, format = "%Y-%m-%d") # Adjust format if needed


# Aggregate sales by month
sales_trend <- data %>%
  mutate(Month = floor_date(Order.Date, "month")) %>% # Extract month
  group_by(Month) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))


# Plot the trend of sales over time
ggplot(sales_trend, aes(x = Month, y = Total_Sales)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Sales Trend Over Time",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal()


##3 6.	Which month has the highest sales?
##Extract months from Order.Date and calculate total Sales by month

# Load necessary libraries
library(dplyr)
library(lubridate)

## # Ensure Order.Date is in Date format
data$Order.Date <- as.Date(data$Order.Date, format = "%Y-%m-%d") # Adjust the format if necessary


# Extract the month and calculate total sales by month
monthly_sales <- data %>%
  mutate(Month = month(Order.Date, label = TRUE)) %>% # Extract month as a labeled factor (e.g., Jan, Feb)
  group_by(Month) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) # Arrange in descending order of sales

# Print the month with the highest sales
print(monthly_sales)



# Highlight the top month
top_month <- monthly_sales[1, ]
cat("The month with the highest sales is:", top_month$Month, "with total sales of:", top_month$Total_Sales, "\n")

print(top_month)

### 7.	How does the average delivery time (difference between Order.Date and Ship.Date) vary by region?
## Calculate delivery time and group by Region.

# Ensure Order.Date and Ship.Date are in Date format
data$Order.Date <- as.Date(data$Order.Date, format = "%Y-%m-%d") # Adjust format if necessary
data$Ship.Date <- as.Date(data$Ship.Date, format = "%Y-%m-%d") # Adjust format if necessary


# Calculate delivery time and group by Region
delivery_time_by_region <- data %>%
  mutate(Delivery_Time = as.numeric(Ship.Date - Order.Date)) %>% # Calculate delivery time in days
  group_by(Region) %>%
  summarise(Average_Delivery_Time = mean(Delivery_Time, na.rm = TRUE)) %>% # Calculate average delivery time
  arrange(desc(Average_Delivery_Time)) # Sort by average delivery time

print(delivery_time_by_region)

# Print the results
print(delivery_time_by_region)


### Customer and Segment Analysis

##	Who are the top 5 customers by total sales?
## Rank Customer.Name by total Sales.


# Group by Customer.Name and calculate total sales
top_customers <- data %>%
  group_by(Customer.Name) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>% # Sort in descending order of sales
  slice_head(n = 5) # Select top 5 customers

# Print the top 5 customers
print(top_customers)



###	How do sales and profit vary across customer segments?
##	Group by Segment and calculate total Sales and Profit


# Group by 'Segment' and calculate total 'Sales' and 'Profit'
result <- data %>%
  group_by(Segment) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )


# Print the result
print(result)


### 10.	What is the average profit margin (Profit/Sales) across customer segments?
##	Group by Segment and compute the average profit margin.

# Group by 'Segment' and calculate the average profit margin (Profit/Sales)

result <- data %>%
  group_by(Segment) %>%
  summarise(
    Average_Profit_Margin = mean(Profit / Sales, na.rm = TRUE)
  )

# Print the result
print(result)
### 11. Geographical Analysis


# Load necessary library
library(dplyr)

# Assuming you have a dataframe called 'data' with 'State' and 'Sales' columns

# Group by 'State' and calculate total 'Sales'
state_sales <- data %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) # Sort in descending order of Total Sales

# Print the state with the most sales
top_state <- state_sales[1, ]
print(top_state)

# Print all states with their sales (optional)
print(state_sales)


### 12.	How do sales and profit compare across regions?
##	Group by Region and calculate total Sales and Profit.


# Load necessary library
library(dplyr)

# Assuming you have a dataframe called 'data' with 'Region', 'Sales', and 'Profit' columns

# Group by 'Region' and calculate total Sales and Profit
region_sales_profit <- data %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )

# Print the result
print(region_sales_profit)




### 13.	What are the top 5 cities by sales revenue?
### Rank City by total Sales.


# Load necessary library
library(dplyr)

# Assuming you have a dataframe called 'data' with 'City' and 'Sales' columns

# Group by 'City' and calculate total Sales, then rank cities by Sales
top_cities <- data %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice_head(n = 5) # Select the top 5 cities

# Print the result
print(top_cities)


### Product and Category Analysis 


# Load necessary library
library(dplyr)


# Group by 'Sub.Category' and calculate total Profit
subcategory_profit <- data %>%
  group_by(Sub.Category) %>%
  summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
  arrange(desc(Total_Profit)) # Sort in descending order of Total Profit

# Print the sub-category with the highest profit
top_subcategory <- subcategory_profit[1, ]
print(top_subcategory)

# Print all sub-categories with their total profits (optional)
print(subcategory_profit)


### 15.	How does discount affect profit across product categories?
###	Group by Category and analyze the relationship between Discount and Profit.

# Load necessary libraries
library(dplyr)
library(ggplot2)

##'data' with 'Category', 'Discount', and 'Profit' columns

# Group by 'Category' and calculate average Discount and Profit
category_analysis <- data %>%
  group_by(Category) %>%
  summarise(
    Average_Discount = mean(Discount, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )

# Print the summarized data
print(category_analysis)




# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming you have a dataframe called 'data' with 'Category', 'Discount', and 'Profit' columns

# Group by 'Category' and calculate total Profit
category_profit <- data %>%
  group_by(Category) %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    Average_Discount = mean(Discount, na.rm = TRUE)
  )

# Print the summarized data
print(category_profit)

# Create a pie chart to show profit distribution across categories
ggplot(category_profit, aes(x = "", y = Total_Profit, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Profit Distribution Across Categories",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )









