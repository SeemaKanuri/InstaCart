# InstaCart
Using the anonymized data on customer orders over time to predict which previously purchased products will be in a user’s next order. 


1.1	Business Objectives and Success Criteria 
Instacart is an American same-day grocery delivery service. It’s a grocery ordering and delivery app, aims to make it easy to fill your refrigerator and pantry with your personal favorites and staples when you need them. We are trying to develop models that predict which products a user will buy again, try for the first time, or add to their cart next during a session. Market Basket Analysis is a modelling technique based upon the theory that if you buy a certain group of items, you are more (or less) likely to buy another group of items. 
Customers select groceries through a web application from various retailers and delivered by a personal shopper. Everyone has a style of shopping. Even though if he/she has a shopping list or not, there is a trend what the customer’s buy in pairs. In this case , Instacart  requires a predictive model to predict which previously purchased products will be in a user’s next order. By accurately predicting a user’s next order, Instacart will be able to manage its inventory effectively, promote cross-selling and also help optimize the staff availability.
Inventory management is one of the important concerns of any retail organization. Inventory management is a practice of overseeing and controlling of the ordering, storage, and use of components that a company uses in the production of the item it sells. Successful inventory management involves creating a purchasing plan to ensure that items are items are available when they are needed-but neither not too little is purchases- and keeping track of existing inventory and its use.
Inventory optimization is a method of balancing capital investment constraints or objectives and service-level goals over a large assortment of stock-keeping units (SKUs) while taking demand and supply volatility into account. Every company has the challenge of matching its supply volume to customer demand. Demand planning is a critical component of inventory management. By knowing a user’s next order, Instacart can estimate the demand leading to better inventory management.
Workforce Optimization(WFO) or staff optimization is another important problem faced by most of the retail firms. By predicting the demand to a certain level of accuracy Instacart can plan and allot resources (Delivery boys in this case) accordingly. Such optimizations will also make sure that no staff is over-worked and no customers are missed.
Cross-selling is the action or practice of selling an additional product or service to an existing customer. In practice, businesses define cross-selling in many ways. Elements that might influence the definition might include the size of the business, the industry sector it operates within and the financial motivations of those required to define the term. The objective of cross-selling can be either to increase the income derived from the client or to protect the relationship with the client.
1.2	Data Mining Objectives and Success Criteria
Affinity analysis and association rule learning encompasses a broad set of analytics techniques aimed at uncovering the associations and connections between the products the customer added to cart in the Instacart app. So using the "market basket analysis" we want to look if there are combinations of products that frequently co-occur in transactions. For example, maybe people who buy flour and casting sugar, also tend to buy eggs (because a high proportion of them are planning on baking a cake). 

So there by using this information Instacart can implement below suggestions like ;
•	aisles layout (put products that co-occur together close to one another, to improve the customer shopping experience in the Instacart)
•	Recommended items, drive recommendation engines (e.g. target customers who buy flour with recommendations like eggs, to encourage them to spend more on their shopping basket)
•	Also, frequently brought items listing will help to improve the customer shopping experience.
•	On-Demand Products: One of our major data mining objectives is to sort products based on the number of times they are ordered. This will help us identify the products that are highly ordered and those that are less on demand.
•	Chronological Analysis of Data: Looking at the data from a year-wise, day-wise and hour-wise perspective will give a better idea on how the firm has performed over time. And drilling down this analysis to the level of products will help us identify seasonal trends in sales of certain products.
•	On-Demand Departments: Sorting the departments based on their demand will help us focus on departments that are in reasonable demand and ignore those that have less or no demand.
•	Finding and Defining Association Rules: An association rule has two parts, an antecedent (if) and a consequent (then). An antecedent is an item that is an in found in data. An antecedent is an item found in the data. A consequent is an item that is found in combination with the antecedent. Identifying and defining the association rules is a way to select and group items that occur together. In this particular case of Instacart data analysis, our objective is to find combination of products that ordered together on a regular basis.
2.	Data Understanding
2.1	Data Definition 
The dataset is anonymized and contains a sample of over 3 million grocery orders from more than 200,000 Instacart users. For each user, we have between 4 and 100 of their orders, with the sequence of products purchased in each order. We also have the week and hour of day the order was placed, and a relative measure of time between orders. 
Each entity (customer, product, order, aisle, etc.) has an associated unique id. Most of the files and variable names should be self-explanatory.
Source for the data is Kaggle and the URL is https://www.kaggle.com/c/instacart-market-basket-analysis.
•	orders (3.4m rows, 206k users):
	order_id: order identifier
	user_id: customer identifier
	eval_set: which evaluation set this order belongs in (see SET described below)
	order_number: the order sequence number for this user (1 = first, n = nth)
	order_dow: the day of the week the order was placed on
	order_hour_of_day: the hour of the day the order was placed on
	days_since_prior: days since the last order, capped at 30 (with NAs for order_number = 1)
•	products (50k rows):
	product_id: product identifier
	product_name: name of the product
	aisle_id: foreign key
	department_id: foreign key
•	aisles (134 rows):
	aisle_id: aisle identifier
	aisle: the name of the aisle
•	deptartments (21 rows):
	department_id: department identifier
	department: the name of the department
•	order_products__SET (30m+ rows):
	order_id: foreign key
	product_id: foreign key
	add_to_cart_order: order in which each product was added to cart
	reordered: 1 if this product has been ordered by this user in the past, 0 otherwise
•	where SET is one of the four following evaluation sets (eval_set in orders):
	"prior": orders prior to that users most recent order (~3.2m orders)
	"train": training data supplied to participants (~131k orders)
	"test": test data reserved for machine learning competitions (~75k orders)

	Data Preparation
Due to the fact that the data we are getting is a raw data,raw data in the real world  may be incomplete it has to be pre-processed the raw data has to go through data cleaning,data integration,data normarlization,data reduction because without a quality data there will be no quality mining results.
	data integration: combining data from multiple sources and generating the user with unified view of the data
	normarlization: normalization is used to minimize or to reduce redundancy.
	data reduction: reduction of the data set that is much smaller in volume but yet yields the same analytical results


	Data cleaning, missing values, outliers 
Data used for the analysis is data stored in relational data base of the client hence is not in raw format warranting less requirement for cleaning.
	Missing values
The only missing values in the data is the  days since prior order column of the Orders table.
As it measures days since the previous order of the customer, the first time a customer orders from the app is recorded as blank value(NA).

 

	Outliers
In statistics, an outlier is an observation point that is distant from other observations. An outlier may be due to variability in the measurement or it may indicate experimental error; the latter are sometimes excluded from the data set.In this particular case there is no evidence of outliers  and therefore all the observations were considered.
	Denormalizing relational data
The data provided by instacart is in the relational data format with primary keys in every file and corresponding foreign keys in every other file. Hence Denormalization is not required.For example products are uniquely identified by product_id and orders can be uniquely identified by order_id.

	Merging multiple data sources
There were a few instances where tables have to be merged using merge() in R. To merge two data frames (datasets) horizontally, use the merge function. In most cases, you join two data frames by one or more common key variables. One such scenario is where we merged products and order_products_train table using product_id as a common key. 
	Feature engineering 
The data has information about user past orders , products and require prediction for future orders. Following are the product and user features that are generated :
Product features
Product reorder rate : It is the number of times a product is re - ordered
	 Variable = Number of times product is reordered ÷ Number of times product is ordered
Product reorder probability: The probability of product being re-ordered
         Variable=     Number of times product is ordered second time ÷ Number of times product is ordered first time
User features

User distinct products : Number of distinct products a user ordered
User total products : Total products shopped by the user
user  reorder ratio : Proportion of orders with re-ordered ratio
Variable = Number of products reordered  ÷  Number of products
Avgbasketprods : Average products added in the basket by the user 
Variable = Number of products  ordered ÷ Number of ordered   

