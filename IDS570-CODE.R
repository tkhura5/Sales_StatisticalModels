#PROJECT SETUP
load("FinalData-Combined.RData")
#Libraries
install.packages('plotrix', repos="http://cran.r-project.org")
library(plotrix)
library(car)
library(plyr)
library(dplyr)
library(psych)
library(tidyverse)
library(gplots)
library(ggplot2)
library(corrplot)

# Final Project for IDS 570 
# WORKING ON THE DATA
# Step 1
# Loading Project_Dataset-Main.RData
# load("Data.RData")
# save(FinalData, file="Data.RData")
View(FinalData)
summary(FinalData)

# Step 2
# Inspecting the Data
# The Dimensions (number of rows and columns) of dataset.
dim(FinalData)
# Column names
names(FinalData)
# Snapshot of the data- First 6 and Last 6 rows of the dataset.
head(FinalData)
tail(FinalData)

# Step 3
# Cleaning the Data
# Removing all the blank values in the dataset.
FinalData <- na.omit(FinalData)
# Removing the unusable or duplicate columns from data.
FinalData$company<-NULL
FinalData$rep1_ord<-NULL
FinalData$wk <- NULL
FinalData$prod_line2 <- NULL
dim(FinalData)
names(FinalData)
View(FinalData)

# Step 4
# Adding some columns to the data
# Creating two columns for Profit and Profit percentage to bring more uniformity to the item list varying from type to price difference.
profit <- FinalData$sale - FinalData$cost
FinalData$profit <- profit
profit_per<-(FinalData$profit/FinalData$sale*100)
FinalData$profit_per<-profit_per

# Creating new column to identify returning and new customers # 0 = new customer, 1 = returning customer
# For new customers, the Shipping (Sale) Date's year = Customer Creation Date's Year and Shipping (Sale) date's Month= Customer Creation date's Month. 
# For returning customers, these will be different.
FinalData$cust_status<-ifelse(FinalData$year==FinalData$cust_year & FinalData$per==FinalData$cust_per,0,1)
FinalData$cust_status<-as.factor(FinalData$cust_status)

# Step 5
# Tidying the Data
# Changing the variable types 
str(FinalData)
FinalData$client_id <- as.character(FinalData$client_id)
FinalData$sales_order <- as.character(FinalData$sales_order)
FinalData$item <- as.character(FinalData$item)
FinalData$dist_channel <- as.factor(FinalData$dist_channel)
FinalData$product_line <- as.factor(FinalData$product_line)
FinalData$qtr<-as.factor(FinalData$qtr)
FinalData$st <- as.factor(FinalData$st)
FinalData$year<-as.factor(FinalData$year)
FinalData$per<-as.factor(FinalData$per)
FinalData$qtr<-as.factor(FinalData$qtr)
FinalData$cust_year<-as.factor(FinalData$cust_year)
FinalData$cust_per<-as.factor(FinalData$cust_per)
str(FinalData)
dim(FinalData)

# Step 6  
# Merging the Data
# Loading the Temperature_Data.RData
# load("Temperature_Data.RData")
# save(Temperature_Data, file="Temperature_Data.RData")
# Merging the two dataframes
NewDataTemp1<-join(FinalData, Temperature_Data, by=c("st","year","per"), type="inner")
dim(NewDataTemp1)

# Loading Distance_From_IL_Data.RData
# load("Distance_From_IL_Data.RData")
# save(Distance_From_IL_Data, file="Distance_From_IL_Data.RData")
# Merging the two dataframes
NewDataTemp2<-join(NewDataTemp1, Distance_From_IL_Data, by="st", type="inner")
dim(NewDataTemp2)

# Loading Income_Data.RData
# load("Income_Data.RData")
# save(Income_Data, file="Income_Data.RData")
# Merging the two dataframes
NewDataTemp3<-join(NewDataTemp2, Income_Data, by="st", type="inner")
dim(NewDataTemp3)

# Loading Itemtype_Data.RData
# load("Itemtype_Data.RData")
# save(Itemtype_Data, file="Itemtype_Data.RData")
# Merging the two dataframes
NewDataTemp4<-join( NewDataTemp3, Itemtype_Data, by=c("sales_order","item"), type="inner")
dim(NewDataTemp4)

# NOTE:  
# Removing profit value wherever it is less than zero as these records depict a promotional product
NewFinalData <- NewDataTemp4[NewDataTemp4$sale > 0,]
NewFinalData <- NewFinalData[NewFinalData$cost > 0,]
NewFinalData <- NewFinalData[NewFinalData$profit > 0,]
NewFinalData<-droplevels(NewFinalData)

# load("FinalData-Combined.RData")
# save(FinalData,Temperature_Data,Distance_From_IL_Data,Income_Data,Itemtype_Data, file="FinalData-Combined.RData")
# DEPENDENT VARIABLE
# PROFIT_PERCENTAGE
describe(NewFinalData$profit_per)
summary(NewFinalData$profit_per)
# The range is 95.01, between 0.8 and 95.81
# The kurtosis is  7.49 which is far from normal distribution
# Plotting Analysis
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(NewFinalData$profit_per,main="Profit percentage",xlab ="Profit percentage", col="steelblue", freq=F)
rug(jitter(NewFinalData$profit_per), col="darkgray")
box()
# Densityplot
plot(density(NewFinalData$profit_per),main="Profit percentage",xlab ="Profit percentage", col="darkgray", lwd=3) 
box() 
# Boxplot 
boxplot(NewFinalData$profit_per, main="Profit percentage", col="orange", xlab ="Profit percentage")
#write.csv(NewFinalData, "NewFinalData.csv")
#save(NewFinalData, file="NewFinalData.RData")

# UNIVARIATE ANALYSIS
# Numerical Variables

# Average Temperature
# Statistical Analysis
describe(NewFinalData$avg_temp)
summary(NewFinalData$avg_temp)
# The median is slighty bigger than the mean, so it is slighty left skewed. 
# The skewness is -0.31 which shows that it is only slighty skewed. 
# The range is 98.7, between -12.8 and 85.9. 
# The kurtosis is -0.78 which is a non normally distributed plot
# Plotting Analysis
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(NewFinalData$avg_temp,main="Temperature Distribution",xlab ="Temperature(F)", col="steelblue", freq=F)
rug(jitter(NewFinalData$avg_temp), col="darkgray")
box()
# Densityplot
plot(density(NewFinalData$avg_temp),main="Temperature Distribution",xlab ="Temperature(F)", col="darkgray", lwd=3) 
box() 
# Boxplot 
boxplot(NewFinalData$avg_temp, main="Temperature Distribution", col="orange")
# The plots are slightly left skewed. 
# It is multi modal graph.

# Square Feet
# Statistical Analysis
describe(NewFinalData$sqft)
summary(NewFinalData$sqft)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 2.69 which shows that it is only heavily skewed. 
# The range is 690, between 12 and 702. 
# The kurtosis is 11.35 which is very far from a normally distributed plot
# Plotting Analysis
dev.off()
par(mfrow=c(1,3))
# Histogram
hist(NewFinalData$sqft,main="SqFt Distribution",xlab ="Square Feet", col="steelblue", freq=F)
rug(jitter(NewFinalData$sqft), col="darkgray")
box()
plot(density(NewFinalData$sqft),main="SqFt Distribution",xlab ="Square Feet", col="darkgray", lwd=3) 
box() 
# Boxplot 
boxplot(NewFinalData$sqft, main="SqFt Distribution", col="orange")
# The plot is right skewed with large number of outliers. 

# Categorical Variables
# Distribution Channels
# Statistical Analysis
table(NewFinalData$dist_channel)
# Contractors and Store Chains make up for the majority of data with maximum being store chains.
# Plotting Analysis
# Barplot
dev.off()
barplot(table(NewFinalData$dist_channel), main = "Bar Plot", xlab = "Distribution Channel", ylab = "Frequency")
ptab<-(prop.table(table(NewFinalData$dist_channel))*100) # Convert to percentages 
barplot(ptab, main = "Distribution Channel", xlab = "Distribution Channel", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,95))
box()
# Pie Chart
pie3D(table(NewFinalData$dist_channel), labels = levels(NewFinalData$dist_channel), explode = 0.1, main = "3D Pie Chart")
# From the plots we see that Store chains and Contractors have nearly equal share and they hold the majority share.

# Customer Status
# Statistical Analysis
table(NewFinalData$cust_status)
# Returning customers hold the majority share. Nearly 9 times the old customer's share.
# Plotting Analysis
# Barplot
dev.off()
par(mfrow=c(1,2))
#barplot(table(NewFinalData$cust_status), main = "Bar Plot", xlab = "Custumer Status Distribution", ylab = "Frequency")
ptab<-(prop.table(table(NewFinalData$cust_status))*100) # Convert to percentages 
barplot(ptab, main = "Customer Status Distribution", xlab = "Customer Status Distribution", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,100))
box()
# Pie Chart
pie3D(table(NewFinalData$cust_status), labels = levels(NewFinalData$cust_status), col=c("orange", "steelblue"),explode = 0.1)
# Returning customers hold the majority share.
legend("topright", 
       legend = c("New Customers", "Returning Customers"), 
       fill = c("orange", "steelblue"))

# Item Type
# Statistical Analysis
table(NewFinalData$item_type)
# Roll Item type hold the majority share close to 90% of the total item type distribution.
# Cable, Loose Cable and Mat item types hold close to 2-3% each of the total item type distribution.
# Plotting Analysis
# Barplot
dev.off()
#barplot(table(NewFinalData$item_type), main = "Bar Plot", xlab = "Item Type Distribution", ylab = "Frequency")
ptab2<-(prop.table(table(NewFinalData$item_type))*100) # Convert to percentages 
barplot(ptab2, main = "Item Type Distribution", xlab = "Item Type Distribution", ylab = "Proportion", col=c("green", "steelblue", "red", "orange"), ylim=c(0,100))
box()
# Pie Chart
pie3D(table(NewFinalData$item_type), labels = levels(NewFinalData$item_type), col=c("orange", "steelblue"),explode = 0.1)

# BIVARIATE ANALYSIS AND HYPOTHESIS TESTING
bivariate_theme <- theme(plot.title = element_text(face="bold.italic", size="16", color="steelblue"), 
                         axis.title=element_text(face="bold.italic", size=10, color="gray"), 
                         axis.text=element_text(face="bold", size=8, color="darkblue"), 
                         panel.background=element_rect(fill="white", color="thistle"), 
                         panel.grid.major.y=element_line(color="thistle1"),
                         panel.grid.minor.y=element_line(color="thistle", linetype=2), 
                         legend.position="top")

# Numerical Variables
# To run all correlations in a dataframe we need to select all relevant numeric variables (columns). This is called a "Correlation matrix"
profit_pernum <- NewFinalData[,c(18,16,21:24)]
profit_percormat <- cor(profit_pernum)
round(profit_percormat, 2)
# With correlation values! Extremely useful when there are many variables
corrplot(profit_percormat, method="circle", addCoef.col="black")  

# Profit percentage and Average temperature
# Statistical Analysis
pt_cor<-cor(NewFinalData$profit_per,NewFinalData$avg_temp)
pt_cor_percentage<-pt_cor*100
pt_cor_percentage
# profit_per is 1.20% correlated with average temperature which is very low 
# Plotting Analysis
dev.off()
p_temp<- ggplot(data=NewFinalData, aes(x=avg_temp, y=profit_per, color=avg_temp)) + geom_point()  + geom_jitter() + ggtitle("Distribution of Profit percentage by Average Temperature") +labs(x="Average Temperature", y="Profit percentage") 
p_temp + geom_smooth(method="loess", color="darkgoldenrod3")  + geom_smooth(method="lm", color="red") +bivariate_theme
# From the graph it is clear that profit percentage is independent of average temperature as the regression line is a straight line parallel to X-axis
# plot(NewFinalData$profit_per~NewFinalData$avg_temp, pch=16, col="lightblue")
# abline(lm(NewFinalData$profit_per~NewFinalData$avg_temp), lwd=3) # Looks like there is no correlation
# cor(NewFinalData$profit_per,NewFinalData$avg_temp, use="complete.obs", method="pearson") #0.02533019
# HYPOTHESYS TESTING 
# Null hypothesis:There is no relationship between average temperature and profit_per
cor.test(NewFinalData$profit_per,NewFinalData$avg_temp) # p-value=0.06718
# We fail to REJECT the null hypothesis

# Profit percentage and Square feet
# Statistical Analysis
ps_sqft<-cor(NewFinalData$profit_per,NewFinalData$sqft)
ps_sqft_percentage<-ps_sqft*100
ps_sqft_percentage
# profit percentage is -32.6% correlated with size of the product purchased (in sqft) which indicate that they are negatively correlated with each other
# ie, when size increases, profit percentage decreases. 
# Plotting Analysis
dev.off()
p_sqft<- ggplot(data=NewFinalData, aes(x=sqft, y=profit_per, color=sqft)) + geom_point()  + geom_jitter() + ggtitle("Distribution of Profit percentage by Size of the product (sqft)") +labs(x="Size of the product (sqft)", y="Profit percentage") 
p_sqft + geom_smooth(method="loess", color="darkgoldenrod3")  + geom_smooth(method="lm", color="red") +bivariate_theme
# From the graph, profit percentage and size of the product are inversely proportional to each other
# However, smaller sized products are sold more in number as we see the data being concentrated in the region 0-200.
# and therby contribute largerly to profit percentage 
# plot(NewFinalData$profit_per~NewFinalData$sqft, pch=16, col="lightblue")
# abline(lm(NewFinalData$profit_per~NewFinalData$sqft), lwd=3) # Looks like a strong positive correlation
# cor(NewFinalData$profit_per,NewFinalData$sqft, use="complete.obs", method="pearson") #0.8442231
# HYPOTHESYS TESTING 
# Null hypothesis:There is no relationship between square feet and profit percentage
cor.test(NewFinalData$profit_per,NewFinalData$sqft) # p-value=2.2e-16
# We REJECT the null hypothesis

# Numerical and Categorical Variables
# Profit Percentage and Distribution channel  
# Statistical Analysis
profit_per_by_channel <- NewFinalData %>% group_by(dist_channel) %>% summarise(avg_profit_per = mean(profit_per), median_profit_per = median(profit_per), sd_profit_per = sd(profit_per))
profit_per_by_channel
View(profit_per_by_channel) 
# Direct-Sales generate highest average and median profit_per as items for Direct-Sales are priced more with less discounts than any other channel.    
# Distributors and Internet Stores contribute towards the least average profit_per as they indicate profit_per selling through third party companies.
# Here more discounts are given for sales through these comapnies and the sale size is sometimes even smaller than any other channel. 
# Plotting Analysis
dev.off()
p_channel<-qplot(dist_channel,profit_per, data=NewFinalData, 
                 ylab="Profit percentage", 
                 xlab="Distribution channel", 
                 main="Distribution of Profit percentage by Distribution channel", 
                 geom="boxplot", fill=I(c("lemonchiffon2","thistle2","pink","lightblue2","peachpuff1"))) + bivariate_theme 
p_channel
# The median price and IQR of Direct Sales is the highest, followed by Contractors and Distributors. 
# There is a higher proportion of profit_per from Direct Sales as they are sold wih less discounts 
# There are also many outliers in all the distribution channels
# ANOVA HYPOTHESYS TESTING 
NewFinalData %>% group_by(dist_channel) %>% summarize(avg = mean(profit_per), std = sd(profit_per)) 
boxplot(profit_per~dist_channel, data=NewFinalData, col=2:7, xlab="profit_per") 
plotmeans(NewFinalData$profit_per~NewFinalData$dist_channel, xlab="dist_channel", ylab="profit_per", lwd=3, col="red", p=0.95)
profit_per.aov <- aov(profit_per~dist_channel, data=NewFinalData)
profit_per.aov
summary(profit_per.aov) 
profit_per.tk <- TukeyHSD(profit_per.aov)
round(profit_per.tk$dist_channel,2)
# Hence we REJECT the NULL Hypothesis Direct Sales are the higest mean profit_per, Followed by Contractors

# Profit Percentage and Customer Status
# Statistical Analysis
profit_per_by_status <- NewFinalData %>% group_by(cust_status) %>% summarise(avg_profit_per = mean(profit_per), median_profit_per = median(profit_per), sd_profit_per = sd(profit_per))
profit_per_by_status
View(profit_per_by_status) 
# The average and median profit_per generated by new customers is more than returning customers
# Returning customers get products at a discounted price 
# Higher average  profit_per of the company comes from new customers
# Plotting Analysis
p_status<-ggplot(data=NewFinalData, aes(x=profit_per, fill=cust_status)) +
  geom_density(alpha=.3)+xlab("Profit percentage") + bivariate_theme +
  ggtitle("Distribution of Profit percentage by Customer status") +labs(x="Profit percentage", y="Density")
p_status
# Even though new customers generate higher average profit_per, they are less in number
# The number of returning customers are very high (9.6times) hence, more total profit_per
dev.off()
# t-test HYPOTHESYS TESTING 
# To see if we have enough observations per level bucket
customer_tbl <- table(NewFinalData$cust_status)
customer_tbl
# Determine the mean to perform t-test 
summary(NewFinalData$profit_per) # mean 69.69 all population
summary(NewFinalData$profit_per[NewFinalData$cust_status==0]) # mean 72.037
summary(NewFinalData$profit_per[NewFinalData$cust_status==1]) # mean 69.44 
# Null Hypothesis:the mean profit percentage by new customers is not statistically different than the mean profit percentage by all the customers
# alternative Hypothesis:the mean profit percentage by new customers is greater than the mean profit percentage by all the customers
profit_per_status0 <- t.test(NewFinalData$profit_per[NewFinalData$cust_status==0], alternative="greater", mu= 69.44, conf.level=0.95)
profit_per_status0 
# We REJECT the null,hence, mean profit percentage for new customers is greater than the mean profit percentage for the total population. 
# Null Hypothesis:the mean profit percentage by old customers is not statistically different than the mean profit percentage by all the customers
# alternative Hypothesis:the mean profit_per by old customers is less than the mean profit_per by all the customers
profit_per_status1 <- t.test(NewFinalData$profit_per[NewFinalData$cust_status==1], alternative="less", mu=72.146, conf.level=0.95)
profit_per_status1 
# We REJECT the null, mean profit percentage for old customers is lesser than the mean profit percentage of the total population. 

# Profit Percentage and Item type
# Statistical Analysis
profit_per_by_item <- NewFinalData %>% group_by(item_type) %>% summarise(avg_profit_per = mean(profit_per), median_profit_per = median(profit_per), sd_profit_per = sd(profit_per))
profit_per_by_item
View(profit_per_by_item)
# Plotting Analysis
p_item<-qplot(item_type,profit_per, data=NewFinalData, 
              ylab="Profit percentage", 
              xlab="Item type", 
              main="Distribution of Profit percentage by Item type", 
              geom="boxplot", fill=I(c("lemonchiffon2","thistle2","pink","lightblue2"))) + bivariate_theme 
p_item
# The profit percentage IQR of the Roll is the Mat (outdoor) is the biggest. Of all the item types, the loose cable (indoor) has the least outliers.
# ANOVA HYPOTHESYS TESTING 
NewFinalData %>% group_by(item_type) %>% summarize(avg = mean(profit_per), std = sd(profit_per)) 
boxplot(profit_per~item_type, data=NewFinalData, col=2:7, xlab="profit_per") 
plotmeans(NewFinalData$profit_per~NewFinalData$item_type, xlab="region", ylab="profit_per", lwd=3, col="red", p=0.95)
dev.off()
profit_per.aov <- aov(profit_per~item_type, data=NewFinalData)
profit_per.aov
summary(profit_per.aov)
profit_per.tk <- TukeyHSD(profit_per.aov)
round(profit_per.tk$item_type,2)
# Hence,we REJECT the NULL hypothesis as Roll has higher profit percentage than other item types

# Now let us try to change numeric variables to categorical variables and see if there is a correlation 
# Temperature levels
hist(NewFinalData$avg_temp) # Skewed to the right and multimodal
plot(density(NewFinalData$ avg_temp)) # Same pattern as histogram
describe(NewFinalData$avg_temp)#We have temperature at state level. Median 53.1
# Let us divide the temperature into different levels as low ,medium and high
NewFinalData$temp_level[NewFinalData$avg_temp<=42]<-"low"
NewFinalData$temp_level[NewFinalData$avg_temp>42 & NewFinalData$avg_temp<=60]<-"medium"
NewFinalData$temp_level[NewFinalData$avg_temp>60]<-"high"
# To See if we have enough observations per level bucket
testData_tbl <- table(NewFinalData$temp_level)
testData_tbl
# Now taking temperature levels for ANOVA Hypothesis testing
NewFinalData %>% group_by(temp_level) %>% summarize(avg = mean(profit_per), std = sd(profit_per)) 
boxplot(profit_per~temp_level, data=NewFinalData, col=2:7, xlab="Profit percentage") 
plotmeans(NewFinalData$profit_per~NewFinalData$temp_level, xlab="temp_level", ylab="profit_per", lwd=3, col="red", p=0.95)
dev.off()
profit_per.aov <- aov(profit_per~temp_level, data=NewFinalData)
profit_per.aov
summary(profit_per.aov)
profit_per.tk <- TukeyHSD(profit_per.aov)
round(profit_per.tk$temp_level,2)
# Hence we fail to REJECT the NULL, there is no difference between low,medium and high levels.

rownames(NewFinalData) <- NULL
# REGRESSION ANALYSIS
# View factors levels to be included in regression model
table(NewFinalData$item_type) #4 Values, one has a preponderand numebr of observations, we shoudl relevel to make Roll a reference
table(NewFinalData$dist_channel) #5 values, from hyposthesis testing we have determined that Direct_Sales have the higher profit percentage, therefore we will make it a reference
table(NewFinalData$cust_status) # 2 values, New customers transactions in a given month are far outnumbered by repeat customers. We will make reference to repeat customers to see the diference a new customer brings in profit percentage

# Releveling factors that need it
NewFinalData$item_type <- relevel(NewFinalData$item_type,"Roll")
NewFinalData$dist_channel <- relevel(NewFinalData$dist_channel,"Direct_Sales")
NewFinalData$cust_status <- relevel(NewFinalData$cust_status,"1")

# RM 1 with a numeric varaible : sqft
mod1 <- lm (profit_per ~ sqft, data=NewFinalData)  
round(confint(mod1),3) #at 95% confidence level profit percentage lays between $73.90 and $74.34, decreasing at the rate between -0.056 to -0.051 per each additional sqft
summary(mod1) # R-Squared 10.62% and 3 stars, as sqft increases profit percentage will be reduced due to volume or bulk discounts 

# RM 2 adding item_type factor
mod2 <- lm (profit_per ~ sqft+item_type, data=NewFinalData)  
round(confint(mod2),3) #taking item type Roll as reference we see that profit percentage decreases -5.28 for LooseCable, -14.95 for Cable and -19.68 for Mats
summary(mod2) # R-Squared 32.38% and 3 stars, the item type had a significant effect in profit percentage
summary(mod2)$adj.r.squared-summary(mod1)$adj.r.squared # +21.76% increase to the single regression model

# RM 3 adding dist_channel factor
mod3 <- lm (profit_per ~ sqft+item_type+dist_channel, data=NewFinalData)  
round(confint(mod3),3) #We can see that compared to Direct Sales, all the other channels have a reduced profit percentage but that impact the model
summary(mod3) # R-Squared 38.96% and 3 stars, the dist channel has a significant effect in profit percentage
summary(mod3)$adj.r.squared-summary(mod2)$adj.r.squared # +6.58% increase with respect to the model 2

# RM 4 adding cust_status factor
mod4 <- lm (profit_per ~ sqft+item_type+dist_channel+cust_status, data=NewFinalData)  
round(confint(mod4),3) #New customers have a postivie effect in profit percentage of 0.46
summary(mod4) # R-Squared 39.37% and 3 stars, the customer status has a significant effect in profit percentage
summary(mod4)$adj.r.squared-summary(mod3)$adj.r.squared # +0.46% increase with respect to the model 3

# Regression diangnostics
plot(predict(mod4), residuals(mod4)) # Model seems OK, it has pattern
par(mfrow=c(2,2)) 
plot(mod4) # we have identified some outliers
plot(hatvalues(mod4)) 
identify(hatvalues(mod4), col="red")
dev.off()
outliers<-c(19416,17866,46,2348,15588,2056)
NewFinalData1<-NewFinalData[-outliers,]

# RM 5 with Outliers Removed
mod5 <- lm (profit_per ~ sqft+item_type+dist_channel+cust_status, data=NewFinalData1)  
round(confint(mod5),3)
summary(mod5) # R-Squared 39.63% removing outliers have improved the R-Squared
summary(mod5)$adj.r.squared-summary(mod4)$adj.r.squared # +0.25% increase, nice!

# Checking for Multicolliniearity
vif(mod5) 
sqrt(vif(mod5)) > 2 # All of the variables comeback FALSE, so no risk of collinearity

# Transforming the Data
# We will check our dependent variable
plot(density(NewFinalData1$profit_per)) # right skewed
plot(density(log(NewFinalData1$profit_per))) # It worsens the skeweness
plot(density(sqrt(NewFinalData1$profit_per))) # Still not improvement we will keep the variable as is
dev.off()
# Now we check out numerical variable sqft
plot(density(NewFinalData$sqft)) # left Skewed
plot(density(log(NewFinalData$sqft))) # Much better 
plot(density(sqrt(NewFinalData$sqft))) # No Improvemnent, we will try to see with log of sqft
dev.off()

# RM 6 with data trasnformation
mod6 <- lm (profit_per ~ log(sqft)+item_type+dist_channel+cust_status, data=NewFinalData1)  
round(confint(mod6),3)
summary(mod6)# R-squared increased to 41.41
summary(mod6)$adj.r.squared-summary(mod5)$adj.r.squared # +1.79 improvement in the model. 

