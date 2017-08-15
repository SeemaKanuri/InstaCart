library(shiny)
library(tuneR)
library(markdown)
library(data.table)
library(dplyr)
library(fgui)
library(ggplot2)
library(gWidgets)
library(gWidgetsRGtk2)
library(lubridate)

library(RColorBrewer)
library(plyr)
library(Hmisc)
library(arules)
library(arulesViz)
library(stringr)
library(tm)

library(plotly)
library(readr)          # To read data
# library(tidyverse)    # includes readr
library(data.table)     # For data manipulation
library(ggplot2)        # For plotting
library(gridExtra)      # To arrange ggplots
library(scales)         # To adjust numeric scales in ggplots
library(dplyr)          # To use pipes?
library(plyr)
library(reshape)
library(reshape2)
library(RGtk2)
library(sqldf)
library(xlsx)
library(xlsxjars)
library(shiny)
library(plyr)
library(plumber)
library(validate)
library(magrittr)
library(stringr)
library(plotrix)
library(mice)
ui<-fluidPage(
  fluidRow(column(8, align="center", offset = 2,
                  textInput("string", label="",value = "INSTACART'S INSTA-BUDDY", width = "100%"),
                  tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
           
           tags$audio(src = "intro.mp3", type = "audio/mp3",autoplay = NA),
           column(8, align="center", offset = 2,img(src="ab.jpeg", height = 350, width = 800, align= "centre"))
  ),
  column(6,align="center",plotlyOutput("Beta")),
  column(6,align="center",height = 350,plotOutput(outputId="plotgraph1")),
  actionButton(inputId="OK",label="Click Me for exploaratory analysis"),
  fluidRow(
    column(12, align="center",wellPanel(
      #drop down with the analysis type
      selectInput("input_type", "Enter the type of analysis",
                  c("Select a value","Exploratory_Analysis", "User_wise_Analysis")
                  
      )
    ))
  ),
  
  uiOutput("ui")
  
)



server <- function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Exploratory_Analysis" = radioButtons("radio",label = h3("TIME VS ORDERS"),
                                                 choices = list("Products by hour"=1 , "Habit of purchase"=2,"Second Order"=3,"User Pattern"=4,"Popular Hours"=5,
                                                                "Regional Food"=6,"Regional Food Per Department"=7,"Day of Week"=8,"Days since prior order"=9,
                                                                "Number of prior orders"=10,"Top 50 orders"=11,"Top Items"=12,"Re-Ordered"=13),selected = NA),
           
           
           "User_wise_Analysis" = radioButtons("radio", "Select the State:",
                                               c("770-Day of week trend"=14,"770(Re-order interval"=15,
                                                 "890-Day of week trend"=16,"890-Day of week trend"=17),selected = NA)
    )
  })
  observeEvent(input$OK,{
    a<-input$radio
    a<-as.integer(a)
    switch(a,
           "1"={
             source("1.R")
             Sys.sleep(5)
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
             
             output$Beta=renderPlotly({p})
             #In evenings as well as Ice Cream being popular, so is wine and assorted savoury snacks!
             
             source("1B.R")
             Sys.sleep(4)
           },
           "2"={
             source("2.R")
             Sys.sleep(3)
             
             # # ############################################################################################
             # # ########################     Days Since Prior Order & Day of Week  #########################
             # # ###########################################################################################



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
             
             output$plotgraph1=renderPlot({grid.arrange(po0, po1, po2, po3, po4, po5, po6,
                                                        ncol = 1)})
             Sys.sleep(4)
             source("2B.R")
             
           },
           "3"={source("3.R")
             Sys.sleep(2)
             ############################################################################################
             ########################     Order Hour of Day when orders on the same day  ###############
             ###########################################################################################
             
             
             
             #When are they making the second order for the day?
             # Select orders where days_since_prior_order == 0
             output$plotgraph1=renderPlot({ggplot(orders[as.numeric(orders$days_since_prior_order) == 0, ], 
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
                      
                      
                      Hour of day when days since prior order = 0")})
             
             
             source("3B.R")
             Sys.sleep(3)
             },
           "4"= {
             source("4.R")
             Sys.sleep(2)
             # ############################################################################################
             # ########################    Within user purchasing pattern  ###############################
             # ###########################################################################################


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



             # ## Plot purchasing hours for a few users_ids
             output$plotgraph1=renderPlot({ggplot(orders[user_id <= users, ],
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
               
               
             })
             source("4B.R")
             Sys.sleep(7)
           },
           "5"={ source("5.R")
             Sys.sleep(3)
             # ############################################################################################
             # ########################    Order by Hour and Day of Week #################################
             # ###########################################################################################


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

             
             
             output$plotgraph1=renderPlot({grid.arrange(p0, p1, p2, p3, p4, p5, p6, ncol = 1)})
             source("5B.R")
             Sys.sleep(2)
           },
           "6"={ source("6.R")
             Sys.sleep(3)
               RegionalFood_names <- c("Afrikaans",
                                          "Albanian",
                                          "Arabic",
                                          "Armenian",
                                          "Azeri",
                                          "Basque",
                                          "Belarusian",
                                          "Bulgarian",
                                          "Catalan",
                                          "Chinese",
                                          "Croatian",
                                          "Czech",
                                          "Danish",
                                          "Dhivehi",
                                          "Dutch",
                                          "English",
                                          "Estonian",
                                          "Faroese",
                                          "Farsi",
                                          "Finnish",
                                          "French",
                                          "Galician",
                                          "Georgian",
                                          "German",
                                          "Greek",
                                          "Gujarati",
                                          "Hebrew",
                                          "Hindi",
                                          "Hungarian",
                                          "Icelandic",
                                          "Indonesian",
                                          "Italian",
                                          "Japanese",
                                          "Kannada",
                                          "Kazakh",
                                          "Konkani",
                                          "Korean",
                                          "Kyrgyz",
                                          "Latvian",
                                          "Lithuanian",
                                          "Macedonian",
                                          "Malay",
                                          "Marathi",
                                          "Mongolian",
                                          "Norwegian",
                                          "Polish",
                                          "Portuguese",
                                          "Punjabi",
                                          "Romanian",
                                          "Russian",
                                          "Sanskrit",
                                          "Serbian",
                                          "Slovak",
                                          "Slovenian",
                                          "Spanish",
                                          "Swahili",
                                          "Swedish",
                                          "Syriac",
                                          "Tamil",
                                          "Tatar",
                                          "Telugu",
                                          "Thai",
                                          "Turkish",
                                          "Ukrainian",
                                          "Urdu",
                                          "Uzbek",
                                          "Vietnamese"
             )


             ## Looping through all products to determine which product names contain at least 1 RegionalFood name


             ## Looping through all products to determine which product names contain at least 1 RegionalFood name

             # RegionalFood_names <- c("Afrikaans", "Albanian", "Arabic", "Armenian",.......



             products_temp <- dt_products

             temp_df <- ""
             temp_df <- as.data.frame(temp_df)
             RegionalFood_products_dep <- ""
             RegionalFood_products_dep <- as.data.frame(RegionalFood_products_dep)
             ### For loop to grab all products that have one RegionalFood name in them
             for (i in RegionalFood_names) {
               products_temp$RegionalFood <- i
               temp_df <-
                 as.data.frame(
                   filter(products_temp,
                          grepl(
                            tolower(i), tolower(products_temp$product_name)
                          )))
               RegionalFood_products_dep  <-
                 rbind.fill(RegionalFood_products_dep, temp_df)[, c("product_id", "department_id", "RegionalFood")]
             }

             RegionalFood_products_dep <-
               inner_join(RegionalFood_products_dep, departments, by = "department_id")
             RegionalFood_products_dep <-
               RegionalFood_products_dep[, -which(names(RegionalFood_products_dep) %in% "department_id")]

             head(RegionalFood_products_dep)



             ## Number of with a RegionalFoods found in products
             length(unique(RegionalFood_products_dep$RegionalFood))

             ## Number of RegionalFood products
             length(unique(RegionalFood_products_dep$product_id))

             ## Number of Products by RegionalFood name
             RegionalFood_products_freq <-
               aggregate(product_id ~ RegionalFood, RegionalFood_products_dep, length)
             RegionalFood_products_freq <-
               RegionalFood_products_freq[order(-RegionalFood_products_freq$product_id), ]
             names(RegionalFood_products_freq) <- c("RegionalFood", "freq")

             head(RegionalFood_products_freq)



             
             ### Which RegionalFood has the most number of products on Instacart
             output$plotgraph1=renderPlot({ggplot(RegionalFood_products_freq[1:10,], aes(reorder(RegionalFood, -freq), freq, fill = RegionalFood)) +
                 geom_bar(stat = "identity",
                          colour = "black") +
                 scale_fill_brewer(palette="Set3") +
                 stat_summary(aes(label = ..y..),
                              geom = "text",
                              vjust = +1.2,
                              size = 4) +
                 labs(title = "No of Products by Category RegionalFood- top 15",
                      x = "RegionalFood", y = "# of Products") +
                 theme(plot.title = element_text(hjust = 0.5))})
             
             source("6B.R")
             Sys.sleep(2)
           },
           "7"={source("7.R")
             Sys.sleep(3)
             ## # of Products per Department by Category (Top 5 Departments among the Top 5 RegionalFoods in # of Products )
             
             depart_cult_agg <- aggregate(product_id ~ department + RegionalFood,
                                          RegionalFood_products_dep, length)
             RegionalFood_keep <- RegionalFood_products_dep[1:15, 1]
             depart_cult_agg <-
               depart_cult_agg[depart_cult_agg$RegionalFood %in% RegionalFood_products_freq[1:15, 1], ]
             
             names(depart_cult_agg) <-
               c("department", "RegionalFood", "frequency")
             
             head(depart_cult_agg)
             
             
             temp2 <- ""
             temp2 <- as.data.frame(temp2)
             depart_cult_agg_top5 <- ""
             depart_cult_agg_top5 <- as.data.frame(depart_cult_agg_top5)
             for (i in RegionalFood_products_freq[1:5, 1]) {
               temp2 <- depart_cult_agg[depart_cult_agg$RegionalFood %in% i,]
               temp2 <- temp2[order(-temp2$frequency), ][1:5, ]
               depart_cult_agg_top5 <- rbind.fill(depart_cult_agg_top5, temp2)
               
             }
             
             depart_cult_agg_top5 <-
               depart_cult_agg_top5[2:nrow(depart_cult_agg_top5), 2:4]
             
             names(depart_cult_agg_top5) <-
               c("department", "RegionalFood", "num_of_Products")
             
             head(depart_cult_agg_top5)
             
             ### Which RegionalFood product is in the most departments
             output$plotgraph1=renderPlot({ggplot(depart_cult_agg_top5,
                                                  aes(reorder(RegionalFood,-num_of_Products), num_of_Products, fill = department)) +
                 geom_bar(
                   stat = "identity",
                   position = position_dodge(),
                   colour = "black",
                   width = .8
                 ) +
                 stat_summary(
                   aes(label = ..y..),
                   geom = "text",
                   vjust = -.789,
                   size = 2.5,
                   position = position_dodge(.789)
                 ) +
                 labs(title = " No of Products per Department by Category- RegionalFoods",
                      x = "RegionalFood", y = "# of Products") +
                 theme(plot.title = element_text(hjust = 0.5))})
             source("7B.R")
             Sys.sleep(3)
           },
           "8"={ source("8.R")
             Sys.sleep(2)
             count<-table(dt_orders$order_dow)
             count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="ORDERS PER DAY_OF_WEEK", 
                                                   xlab="Day of the Week", ylab="COUNT",col='orange')})
             source("8B.R")
             Sys.sleep(2)},
           "9"={source("9.R")
             Sys.sleep(3)
             count<-table(dt_orders$days_since_prior_order)
             #count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="When do users order again", 
                                                   xlab="days_since_prior_order", ylab="COUNT",col='blue')})
             source("9B.R")
             Sys.sleep(3)},
           "10"={source("10.R")
             Sys.sleep(3)
             orders_temp<-filter(dt_orders,eval_set=='prior')
             count<-table(dt_orders$order_number)
             output$plotgraph1=renderPlot({barplot(count, main="Number of prior orders", 
                                                   xlab="Order Number", ylab="COUNT",col='red')})
             
             source("10B.R")
             Sys.sleep(3) },
           "11"={
             source("11.R")
             Sys.sleep(3)
             count<-table(dt_train$order_id)
             count<-count[0:50]
             count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="TOP 50 Orders", 
                                                   xlab="Order Number", ylab="COUNT",col='black')})
             source("11B.R")
             Sys.sleep(2)},
           "12"={
             source("12.R")
             Sys.sleep(3)
             orders_products_temp<-merge(dt_train,dt_products,by='product_id')
             count<-table(orders_products_temp$product_name)
             count<-sort(count,decreasing = TRUE)
             count<-count[0:3]
             output$plotgraph1=renderPlot({barplot(count, main="TOP 5 Products", 
                                                   xlab="Product Name", ylab="COUNT",col='orange')})
             source("12B.R")
             Sys.sleep(2)},
           "13"={
             source("13.R")
             Sys.sleep(3)
             count<-table(dt_train$reordered)
             output$plotgraph1=renderPlot({barplot(count, main="Reordered vs Non-Reordered products", 
                                                   xlab="Reorder Status", ylab="COUNT",col='orange')})
             source("13B.R")
             Sys.sleep(2)},
           "14"={
             source("14.R")
             Sys.sleep(5)
             #ANALYSIS 2: sorting  days of the week based on order count
             dt_orders1<-filter(dt_orders,user_id==770)
             count<-table(dt_orders1$order_dow)
             count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="ORDERS PER DAY_OF_WEEK", 
                                                   xlab="Day of the Week", ylab="COUNT",col='orange')})
           },
           "15"={
             source("15.R")
             Sys.sleep(5)
             #ANALYSIS 3: When do customers order again?
             dt_orders3<-filter(dt_orders,user_id==770)
             count<-table(dt_orders3$days_since_prior_order)
             #count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="When do users order again", 
                                                   xlab="days_since_prior_order", ylab="COUNT",col='blue')})},
           "16"={
             source("16.R")
             Sys.sleep(5)
             #ANALYSIS 2: sorting  days of the week based on order count
             dt_orders2<-filter(dt_orders,user_id==890)
             count<-table(dt_orders2$order_dow)
             count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="ORDERS PER DAY_OF_WEEK", 
                                                   xlab="Day of the Week", ylab="COUNT",col='orange')})},
           "17"={
             source("17.R")
             Sys.sleep(5)
             #ANALYSIS 3: When do customers order again?
             dt_orders4<-filter(dt_orders,user_id==890)
             count<-table(dt_orders4$days_since_prior_order)
             #count<-sort(count)
             output$plotgraph1=renderPlot({barplot(count, main="When do users order again", 
                                                   xlab="days_since_prior_order", ylab="COUNT",col='blue')})
           }
           
                 )
           })
  
  
           }


shinyApp(ui, server)
