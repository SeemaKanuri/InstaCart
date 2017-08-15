
###########################################################################################
###########################     Products by Hour Chart    #################################
###########################################################################################


#shows time of day that users purchase specific products
#products are assigned to morning or afternoon according to the mode order hour is
#between 09:00-11:00 and 15:00-17:00.in the day that the particular product was ordered.



dt_orders <- fread('D:/Instacart/Data/orders.csv')
dt_products <- fread('D:/Instacart/Data/products.csv')
dt_train <- fread('D:/Instacart/Data/order_products__train.csv')
dt_prior <- fread('D:/Instacart/Data/order_products__prior.csv')
#aisles <- fread('D:/Instacart/Data/aisles.csv')
#departments <- fread('D:/Instacart/Data/departments.csv')
dt_submission <- fread('D:/Instacart/Data/sample_submission.csv/sample_submission.csv')



setkey(dt_orders, order_id)
setkey(dt_prior, order_id)
dt_prior <- dt_prior[dt_orders[dt_orders$eval_set=="prior"]]

# Summarise orders by product and hour
dt_product_hour <- dt_prior[, .(count = .N), by=c('product_id','order_hour_of_day')]
dt_product_sum  <- dt_prior[, .(total = .N), by=c('product_id')]

setkey(dt_product_hour, product_id)
setkey(dt_product_sum, product_id)
dt_product_hour <- dt_product_hour[dt_product_sum]

# create % product by hour metric
setkey(dt_product_hour, product_id,order_hour_of_day )
dt_product_hour$pcnt_orders <- dt_product_hour$count / dt_product_hour$total

# Create summary of top 25 morning and afternoon products----
dt_tmp <- dt_product_hour[dt_product_hour$total>3000 & dt_product_hour$order_hour_of_day %in% 8:11, ][order(-pcnt_orders)]
dt_product_hour_subset <- dt_tmp[, .(pcnt_orders=max(pcnt_orders),order_hour_of_day=max(order_hour_of_day)), by =product_id ][1:25]

dt_tmp <- dt_product_hour[dt_product_hour$total>3000 & dt_product_hour$order_hour_of_day %in% 15:17, ][order(-pcnt_orders)]
dt_product_hour_subset <- rbind(dt_product_hour_subset, dt_tmp[, .(pcnt_orders=max(pcnt_orders),order_hour_of_day=max(order_hour_of_day)), by =product_id ][1:25])

#merge on product description
setkey(dt_product_hour_subset, product_id)
setkey(dt_products, product_id)
dt_product_hour_subset <- merge(dt_product_hour_subset, dt_products[, 1:2], all.x = TRUE)

# Extract summary of all data for those products----
setkey(dt_product_hour_subset, product_id)
setkey(dt_product_hour, product_id)
dt_product_hour_subset2 <- merge(dt_product_hour_subset[,1], dt_product_hour, all.x = TRUE)

#  Create a plot of the data----
setkey(dt_product_hour_subset, pcnt_orders)

#extract list of products for looping later
lst_products <- rev(dt_product_hour_subset$product_id)

#set up a plotly container
p <- plot_ly()

# loop through product list adding line to chart
for(i in lst_products){
  trace_colour <- ifelse(dt_product_hour_subset[dt_product_hour_subset$product_id==i]$order_hour_of_day>12,"#ff000044", "#078a1f44")
  p <- add_trace(p,
                 x = dt_product_hour_subset2[dt_product_hour_subset2$product_id==i]$order_hour_of_day,
                 y = 100 * dt_product_hour_subset2[dt_product_hour_subset2$product_id==i]$pcnt_orders,
                 type = 'scatter',
                 mode = "lines",
                 line=list(color= trace_colour))
}

# add vertical line at midday
p <- add_trace(p,
               x = c(12, 12),
               y= c(100 * min(dt_product_hour_subset2$pcnt_orders),100 * max(dt_product_hour_subset2$pcnt_orders)),
               mode = "lines",
               line=list(color= "#D3D3D3CC",  dash = 'dash'))

# Build the annotations
annotate_am <- list(
  xref = 'x',
  yref = 'y',
  x = -1,
  y = 100 * max(dt_product_hour_subset2$pcnt_orders),
  xanchor = 'left',
  yanchor = 'top',
  text = paste((dt_product_hour_subset[dt_product_hour_subset$order_hour_of_day<12]$product_name) ,collapse = "\n"),
  font = list(family = 'Arial',
              size = 10,
              color = "#078a1f44"),
  align = "left",
  showarrow = FALSE)

annotate_pm <- list(
  xref = 'paper',
  yref = 'y',
  x = 1.1,
  y = 100 * max(dt_product_hour_subset2$pcnt_orders),
  xanchor = 'right',
  yanchor = 'top',
  text = paste((dt_product_hour_subset[dt_product_hour_subset$order_hour_of_day>=12]$product_name) ,collapse = "\n"),
  font = list(family = 'Arial',
              size = 10,
              color = "#ff0000"),
  align = "right",
  showarrow = FALSE)

# add titles
p <-  layout(p,                      
             xaxis = list(           
               range = list(-1,24),
               title = "Hour of Day Ordered",
               titlefont = list(
                 family = "Arial, Bold",
                 size = 18,
                 color = "#000000"),
               tickfont = list(
                 family = "Arial",
                 size = 12,
                 color = "#000000"),
               tickmode = "auto",
               nticks = 13,
               ticklen = 7,
               tickwidth = 2,
               ticks = "outside",
               showline = FALSE,
               zeroline = FALSE,
               showgrid = FALSE
             ),
             yaxis = list(
               title = "Percent of Orders by Product",
               titlefont = list(
                 family = "Arial, Bold",
                 size = 18,
                 color = "#000000"),
               tickfont = list(
                 family = "Arial",
                 size = 12,
                 color = "#000000"),
               tickmode = "auto",
               nticks = 4,
               ticks = "outside",
               ticklen = 7,
               tickwidth = 2,
               showline = FALSE,
               zeroline = FALSE,
               showticklabels = TRUE,
               showgrid = FALSE
             ),
             showlegend = FALSE,
             margin = list(l=70, 
                           b=60,
                           r= 100),
             annotations = annotate_am
)

#add second set of annotations
p <-  layout(p,
             annotations = annotate_pm)                    
     
p
#In evenings as well as Ice Cream being popular, so is wine and assorted savoury snacks!



############################################################################################
########################     Days Since Prior Order & Day of Week  #########################
###########################################################################################



orders <- as.data.table(dt_orders)

### Days Since Prior Order & Day of Week

#It appears that people who order on days 0 and 1 have  a tendency  to order 7 days apart.
#This shows a population with a habbit of purchasing on the same day each week.
#This pattern is not so pronounced  during days 2 - 4, though there is again for days 5 and 6. 

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
  labs(y = "Day 0", title = " habbit of purchasing on the same day each week")

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





############################################################################################
########################     Order Hour of Day when orders on the same day  ###############
###########################################################################################



#When are they making the second order for the day?
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
       title = "

When the User making the second order for the day ??? 
       
       
       Hour of day when days since prior order = 0")





############################################################################################
########################    Within user purchasing pattern  ###############################
###########################################################################################


#Some users vary in hours that they make an order, while some are less variable


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
       y = "Hour of Day",
       title = " 
       
       Within user purchasing pattern ")



############################################################################################
########################    Order by Hour and Day of Week #################################
###########################################################################################


#Day 1 to Day 5 has hour 10 as the most popular hour for orders, while days 0 and 6 have hour 14 as the most popular. 
#It suggests that Days 1 - 5 are weekdays, while Days 0 and 6 are weekends.



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
  labs(y = "Day 0", title = "Most popular hour for orders by day")

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
