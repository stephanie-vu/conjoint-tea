#Conjoint Analysis for Tea
#By Stephanie Vu

# import conjoint library
library(conjoint)

## Step 1: experimental design

#define four attributes of tea for the conjoint study
price=c("40", "55", "70")  
variety=c("black","green","red")
kind=c("bags", "granulated", "leafy")
aroma=c("yes", "no")

#create level names <---- we want to know what attributes associate to what level
levelnames = data.frame("levels" = c(price,variety,kind,aroma))
print(levelnames)

# full factorial design, total 3x3x3x2 = 54 combinations
experiment = expand.grid(price=price, variety=variety, kind=kind, aroma=aroma)
surveycards = caFactorialDesign(data=experiment,cards=13, type="fractional") #<--------This is fractional factorial design of 13 cards

#view experiment and carddesign
head(experiment)
print(surveycards)

# QC design cards
cor(caEncodedDesign(surveycards))
print(surveycards)
# encode the design
profiles=caEncodedDesign(design=surveycards)

## Step 2: collect survey data from csv file
preferences = read.csv(file="C:\\Stephanie\\BUS 421\\Conjoint Analysis\\tea_pref.csv")

print(profiles)

## Step 3: Find the result for the conjoint model

## find the feature importance & print
u2 = caImportance(y=preferences,x=profiles) #check the importance of the attributes
barplot(u2, names.arg = c("price","variety","kind","aroma"))

## Step 4: Find the optimal price

# 1. Find the most favorable product profile

u1 = caUtilities(y=preferences,x=profiles,z=levelnames)
prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)

#From the plots, we can find the most favorable profile for tea as price='40', variety ='black', kind = 'granulated', aroma ='yes')

# 2. Find the optimal price

#price1 demand = intercept + black + granulated + yes + price1
d1 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['granulated',] + prod_utility['yes',] + prod_utility['40',] 
print(d1)
r1 = d1* 40 #revenue = demand * price

#price2 demand = intercept + black + granulated + yes + price2
d2 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['granulated',] + prod_utility['yes',] + prod_utility['55',] 
print(d1)
r2 = d2* 55 #revenue = demand * price

#price3 demand = intercept + black + granulated + yes + price3
d3 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['granulated',] + prod_utility['yes',] + prod_utility['70',] 
print(d1)
r3 = d3* 70 #revenue = demand * price

#create the dataframe for plot
est_demand = c(d1,d2,d3)
est_rev = c(r1,r2,r3)
price_level = c(40, 55, 70)

revenue = data.frame(est_demand, est_rev, price_level)

#plot demand curve
plot(price_level, est_demand, type='o',xlab="price", ylab="demand")

#plot revenue curve
plot(price_level, est_rev, type='o',xlab="price", ylab="revenue")



