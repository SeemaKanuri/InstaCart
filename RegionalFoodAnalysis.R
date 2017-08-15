
library(RColorBrewer)
library(plyr)
library(Hmisc)
library(arules)
library(arulesViz)
library(stringr)
library(tm)


dt_orders <- fread('D:/Instacart/Data/orders.csv')
dt_products <- fread('D:/Instacart/Data/products.csv')
dt_train <- fread('D:/Instacart/Data/order_products__train.csv')
dt_prior <- read.csv('D:/Instacart/Data/order_products__prior.csv')
aisles <- fread('D:/Instacart/Data/aisles.csv')
departments <- fread('D:/Instacart/Data/departments.csv')
dt_submission <- fread('D:/Instacart/Data/sample_submission.csv/sample_submission.csv')


# Flag products in product table if they are Food products or not

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
ggplot(RegionalFood_products_freq[1:10,], aes(reorder(RegionalFood, -freq), freq, fill = RegionalFood)) +
  geom_bar(stat = "identity",
           colour = "black") +
  scale_fill_brewer(palette="Set3") +
  stat_summary(aes(label = ..y..),
               geom = "text",
               vjust = +1.2,
               size = 4) +
  labs(title = "No of Products by Category RegionalFood- top 15",
       x = "RegionalFood", y = "# of Products") +
  theme(plot.title = element_text(hjust = 0.5))




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
ggplot(depart_cult_agg_top5,
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
  theme(plot.title = element_text(hjust = 0.5))