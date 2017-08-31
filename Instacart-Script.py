
# coding: utf-8

# In[ ]:

library("ggplot2")
library(reshape2)
setwd("C:/Users/mnave/Documents/instacart")

aisles = read.csv("aisles.csv")
head(aisles)



departments = read.csv("departments.csv")
head(departments)

train = read.csv("order_products__train.csv")
head(train)
str(train)

orders = read.csv("orders.csv")
head(orders)


products = read.csv("products.csv")
head(products)


prior = read.csv("order_products__prior.csv")
head(prior)


# intially examining only 
# prior,train and orders data frames


#check =aggregate(formula= frequency~order_id+product_id,data= train2,FUN = sum)

userinfo = orders[orders$eval_set=="prior",c(1,2)]

# To create a user profile 

userproducts=merge(userinfo,prior,by= "order_id")
userproducts2 = userproducts


userproducts = userproducts[,c("user_id","product_id")]
userproducts["frequency"] = 1
userproducts=aggregate(formula=frequency~user_id+product_id,data=userproducts,FUN = sum)
userproducts=merge(userproducts,products[,c(1,2)],by="product_id")
userproducts = userproducts[,c(2,3,4)]
#write.csv(userproducts,"userproducts.csv")
userproducts = read.csv("userproducts.csv")

# remove products with frequency of 1 and 2 

userproducts

#ggplot(userproducts,aes(x=user_id,y=frequency))+geom_point(aes(color=product_name))+theme(legend.position = "none")

# user item table
#useritemtable=dcast(userproducts,user_id~product_name ,value.var = "frequency",drop = TRUE)
useritemtable=dcast(test,user_id~product_name ,value.var = "frequency",drop = TRUE)

