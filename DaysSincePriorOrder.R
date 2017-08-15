
#### Days Since Prior Order #####

#The below is a distribution of the days since prior order.


library(readr)          # To read data
# library(tidyverse)    # includes readr
library(data.table)     # For data manipulation
library(ggplot2)        # For plotting
library(gridExtra)      # To arrange ggplots
library(scales)         # To adjust numeric scales in ggplots
library(dplyr)          # To use pipes?

dt_orders <- fread('D:/Instacart/Data/orders.csv')
orders <- as.data.table(dt_orders)

#Some observations:
# There are a number of people who make orders on the same day (0 days)
# There are lots of people who make orders on a weekly basis (7 days)
# There are some people who make orders every fortnight (14 days)
# There are yet some people who make orders every 3 weeks (21 days)
# 30 days appears to represent either:
# People making orders monthly
# Orders made 30 or more days apart

ggplot(orders, aes(x = days_since_prior_order)) +
  geom_bar(fill = c("orange", rep("blue", 6), 
                    "orange", rep("blue", 6), 
                    "orange", rep("blue", 6),
                    "orange", rep("blue", 6),
                    "orange", "blue", "orange")) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Days Since Prior Order",
       y = "Count",
       title = "Distribution of Orders by Days Since Prior Order") +
  scale_y_continuous(labels = comma)


### Days Since Prior Order & Day of Week

#Splitting the order data by day of week presents deviations from the overall pattern.
#We can now start to see sub-populations in the data.
#It appears that people who order on days 0 and 1 have  a tendency  to order 7 days apart.
#This shows a population with a habbit of purchasing on the same day each week.
#This pattern is not so pronounced  during days 2 - 4, though there is again for days 5 and 6. 

## Facet using grid.arrange for greater control of layout and colours
po0 <- ggplot(orders[order_dow == 0, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 7), "blue", rep("orange", 6), "blue",
                    rep("orange", 6), "blue", rep("orange", 6), "blue",
                    rep("orange", 2))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 0")

po1 <- ggplot(orders[order_dow == 1, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 7), "blue", rep("orange", 6), "blue",
                    rep("orange", 6), "blue", rep("orange", 6), "blue",
                    rep("orange", 2))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 1")

po2 <- ggplot(orders[order_dow == 2, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 7), "blue",  rep("orange", 6), "blue", "blue",
                    rep("orange", 6), "blue", rep("orange", 6), "blue",
                    rep("orange", 1))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 2")

po3 <- ggplot(orders[order_dow == 3, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 2), "blue",  rep("orange", 28))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 3")

po4 <- ggplot(orders[order_dow == 4, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 3), "blue",  rep("orange", 3), "blue",
                    rep("orange", 23))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 4")

po5 <- ggplot(orders[order_dow == 5, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 4), "blue",  rep("orange", 2), "blue",
                    rep("orange", 23))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 5")

po6 <- ggplot(orders[order_dow == 6, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("orange", 6), "blue", "blue",  rep("orange", 5), "blue",
                    rep("orange", 6), "blue", rep("orange", 6), "blue",
                    rep("orange", 3))) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 6",
       x = "Day Since Last Order")

grid.arrange(po0, po1, po2, po3, po4, po5, po6,
             ncol = 1)


### Order Hour of Day when orders on the same day {.tabset}

#There are a sizeable number of orders that happen on the same day, represented
#by days_since_prior_order == 0. This raises some questions:

#When are they making the second order for the day?
#How many hours apart are they making orders if ordered on the same day?

#### When is the second order?

# Select orders where days_since_prior_order == 0
ggplot(orders[as.numeric(orders$days_since_prior_order) == 0, ], 
       aes(x = order_hour_of_day)) +
  geom_bar( fill = "blue") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Hour of Day",
       y = "",
       title = "Hour of day when days since prior order = 0")



#### How far apart?

#If someone is ordering on the same day, they are mostly ordering within the same
#hour, or shortly after their first order. Good thing they noticed soon after
#their original order!
  

## Calculate hours since prior order, only if days since prior order is 0
orders <- data.table(orders)
temp <- orders$order_hour_of_day
temp <- c("24", temp)
temp <- temp[1:length(temp)-1]
orders$temp <- temp
orders[, hours_since_last_order := as.numeric(order_hour_of_day) - as.numeric(temp)]
orders[days_since_prior_order != 0 | is.na(days_since_prior_order), 
       hours_since_last_order := NA]
temp <- orders[days_since_prior_order == 0, ]

ggplot(temp, aes(x = hours_since_last_order)) +
  geom_bar( fill = "red") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Hours Since Last Order",
       y = "",
       title = "Hours Since Last Order if Ordered on the Same Day")




### Within user purchasing pattern
#Some users vary in hours that they make an order, while some are less variable.

## Calculate variance of order_hour_of_day to use as size of lines
orders <- orders[, 
                 order_hour_of_day_variance := var(order_hour_of_day), 
                 by = user_id]

## Smooth line going through points
## http://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points-maybe-polynomial-interpolation

## Number of users to plot
users <- 6
## Calculate splines for users to create smoothed lines
spline_int = data.frame()
for (i in 1:users) {
  temp <- data.frame(spline(orders[user_id == i, order_number],
                            orders[user_id == i, order_hour_of_day]),
                     user_id = i)
  spline_int <- rbind(spline_int, temp)
}

rm(temp)
## attach the order_hour_of_day_variance
for (i in 1:nrow(spline_int)) {
  spline_int$order_hour_of_day_variance[i] = mean(orders[user_id == spline_int$user_id[i], order_hour_of_day_variance])
}



## Plot purchasing hours for a few users_ids
ggplot(orders[user_id <= users, ],
       aes(x = order_number, y = as.numeric(order_hour_of_day))) +
  ## Points for each user_id and order
  geom_point(data = orders[user_id <= users & user_id > 2, ],
             aes(size = order_hour_of_day_variance, alpha = 0.5),
             colour = "blue") +
  ## Colour user_id 1 in orange
  geom_point(data = orders[user_id <= users & user_id == 1, ],
             aes(size = order_hour_of_day_variance, alpha = 0.5),
             colour = "red") +
  ## Colour user_id 2 in light blue
  geom_point(data = orders[user_id <= users & user_id == 2, ],
             aes(size = order_hour_of_day_variance, alpha = 0.5),
             colour = "black") +
  ## Smooth lines for each user_id
  geom_line(data = spline_int[spline_int$user_id > 2,],
            aes(x = x, y = y, group = user_id, 
                size = order_hour_of_day_variance, alpha = 0.5), colour = "grey") +
  ## Colour user_id 1 in orange
  geom_line(data = spline_int[spline_int$user_id == 1,],
            aes(x = x, y = y, group = user_id, 
                size = order_hour_of_day_variance, alpha = 0.5), colour = "#E69F00") +
  ## Colour user_id 2 in light blue
  geom_line(data = spline_int[spline_int$user_id == 2,],
            aes(x = x, y = y, group = user_id, 
                size = order_hour_of_day_variance, alpha = 0.5), colour = "#56B4E9") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_y_continuous(limits = c(0, 24)) +
  labs(x = "Order Number",
       y = "Hour of Day")







## Orders by Day of the Week

#There are a lot of orders on each day of the week. However, there are 
#significantly more orders on days 0 and 1. 

#Could this represent the weekend?


ggplot(orders, aes(x = order_dow)) + 
  geom_bar(fill = c(rep("gold", 2), rep("blue", 5))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Day of Week (unknown)",
       y = "Count",
       title = "Distribution of Orders by Day of Week") +
  scale_y_continuous(labels = comma)




## Orders by Hour of the Day

#Here we can see at what hours of the day people are making orders.

#Reading the plot from left to right:
  
# There are few orders between 00 - 05, representing few orders during sleeping hours
# Orders pick up beginnig at hour 06 as people start to wake up
# Orders continue to pick up rapidly in the morning, peaking at hour 10 (gold)
# Orders continue to be made during the morning, experiencing a small dip at noon
# There is a slight pick up of orders just after lunch at hour 15
# Orders begin to drop commencing at hour 16 through to hour 23


ggplot(orders, aes(x = order_hour_of_day)) + 
  geom_bar(fill = c(rep("gold", 10), "red", rep("gold", 4), "blue",
                    rep("gold", 8))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Hour of the Day",
       y = "",
       title = "Distribution of Orders by Hour of the Day") +
  scale_y_continuous(labels = comma)



#So most orders are made between the hours 09 and 16. But does this pattern exist
#over all days of the week?

### Order by Hour and Day of Week

#In the graph below, the pattern we observed is no longer observed.

#Instead, Day 1 to Day 5 has hour 10 as the most popular hour for orders, while
#days 0 and 6 have hour 14 as the most popular. It suggests to me that Days 1 - 5
#are weekdays, while Days 0 and 6 are weekends.



p0 <- ggplot(orders[order_dow == 0, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 14), "blue", rep("gold", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 0")

p1 <- ggplot(orders[order_dow == 1, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 10), "blue", rep("gold", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 1")

p2 <- ggplot(orders[order_dow == 2, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 10), "blue", rep("gold", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 2")

p3 <- ggplot(orders[order_dow == 3, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 10), "blue", rep("gold", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 3")

p4 <- ggplot(orders[order_dow == 4, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 10), "blue", rep("gold", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 4")

p5 <- ggplot(orders[order_dow == 5, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 10), "blue", rep("gold", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 5")

p6 <- ggplot(orders[order_dow == 6, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("gold", 14), "blue", rep("gold", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 6",
       x = "Hour of the Day")



grid.arrange(p0, p1, p2, p3, p4, p5, p6, ncol = 1)