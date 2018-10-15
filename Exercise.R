################################################
## R Training for Data Analytics  ##############
## Authors: Dr. Matthias Duschl   ##############
##          Dr. Daniel Lee        ##############
################################################
################################################

library(data.table)
library(dplyr)
library(ggplot2)

################################################
## EXERCISE SCRIPT  ############################
################################################

# create data set that will be used througout the training
n = 200
Companies <- data.table(
  Company = paste("Company",1:n),
  Office  = sample(c("New York","Frankfurt","London","Boston","Singapur","San Francisco")
                   , size = n, replace = T, prob = sample(1:100,6)),
  Infrastructure = sample(c("weak","strong"), size = n, replace = T, prob = c(0.3,0.7)),
  Marketing = round(runif(n, 0, 0.7), digits = 0),
  Contracts = sample(c("Few","Medium","Many"), size = n, replace = T, prob = c(0.2,0.3,0.5)),
  Sales = sample(2000:4000, n, replace = TRUE),
  Profit = sample(100:500, n, replace = TRUE)
)

## EXERCISE 1

# 1-A: Write a function which uses the Pythagorean theorem to compute the
# longest side of a right triangle based on the lengths of the two other sides.
pythagorean <- function(a, b) {
  c.squared <- a^2 + b^2
  sqrt(c.squared)
}

# 1-B: Write functions which summarize different animals differently based on their class.
summary.dog <- function(dog) {
  cat("This dog is named", dog$name, "and its favourite treat is", dog$treat,
      ".", fill = T)
}
summary.cat <- function(cat) {
  cat("This cat is named", cat$name,
      "and you'll never know its favourite treat because cats are snobby.",
      fill = T)
}
fido <- list(name = "Fido", treat = "red herring")
class(fido) <- c(class(fido), "dog")
# Notice the cat can still *have* a favourite, it just behaves differently.
garfield <- list(name = "Garfield", treat = "lasagna")
class(garfield) <- c(class(garfield), "cat")

# 1-C: Write a partially applied function that computes something to the power of something else.
power.to <- function(power) {
  function(x) {x^power}
}
square <- power.to(2)
cube <- power.to(3)
for (i in 1:5) print(square(i))
for (i in 1:5) print(cube(i))

## EXERCISE 2

# 2-A: Select companies by various criteria
Companies[Office %in% c("Frankfurt","London","New York") & Marketing == 1 & Sales > 3000] 

# 2-B: group by two variables or more
Companies[, .(AvgSales = mean(Sales,na.rm=T), MaxSales = max(Sales,na.rm=T), MinSales = min(Sales,na.rm=T),
              AvgProfit = mean(Profit, na.rm=T), MaxProfit = max(Profit, na.rm=T), MinProfit = min(Profit, na.rm=T)), 
            by =.(Infrastructure, Marketing)]

# 2-C: create data.table of 1-billion rows and perform aggregation
DT = data.table(x=c("b","b","b","a","a","a","c","c","c","c"),
                v=rnorm(100000000))
DT[, mean(v), by=x]

system.time(DT[, mean(v), by=x])


## EXERCISE 3

# 3-A: add Employee variable (random variable) and calculate Sales per Employee
Companies[, "NrEmployees" := rnorm(200, mean=50, sd=20)]
Companies[, "SalesperEmpl" := round(Sales/NrEmployees, 2)]

# 3-B: re-code "Infrastructure" into binary values
Companies[, "Infrastructure_re" := 0]
Companies[Infrastructure == "strong",  Infrastructure_re := 1]

Companies[, "SalesperEmpl_re" := 0]
Companies[SalesperEmpl > 100,  SalesperEmpl_re := 1]

# why to re-code into binary values? 1) enables counting and 2) sometimes useful for 
# linear algebra (e.g. statistical model building) 


## EXERCISE 4

# 4: benchmark various melt implementations
n <- 1000000
CompanySales <- data.table(Company = paste0("Company ",1:n)
                           , Office = sample(c("New York","Frankfurt","London","Boston","Singapur","San Francisco")
                                             , size = n, replace = TRUE)
                           , Sales_Prod1 = sample(1000:5000,n, replace = TRUE)
                           , Sales_Prod2 = sample(1000:5000,n, replace = TRUE)
                           , Sales_Prod3 = sample(1000:5000,n, replace = TRUE))

# using base::reshape for melting multiple columns (but not reshape2)
system.time(longSales0 <- reshape(data = CompanySales, varying = names(CompanySales)[3:5], v.names = "Sales"
                      , timevar = "Product", idvar = c("Company","Office"), times = paste0("Product ",1:3)
                      , direction = "long"))

# data.table::melt() all three "Sales" columns to obtain long format
system.time(longSales1 <- melt(CompanySales, id.vars = c("Company","Office")
                  , measure.vars = paste("Sales_Prod",1:3,sep="")
                  , variable.name = "Product", value.name = "Sales"))

# data.table::melt() all three "Sales" columns to obtain long format
system.time(longSales1 <- reshape2::melt(CompanySales, id.vars = c("Company","Office")
                   , measure.vars = paste("Sales_Prod",1:3,sep="")
                   , variable.name = "Product", value.name = "Sales"))


## EXERCISE 5

# 5-A: translate nested code into pipes
arrange(filter(select(Companies, Company, Sales, Marketing), 
               Sales > 2500), desc(Marketing), Sales)

Companies %>%
  select(Company, Sales, Marketing) %>%
  filter(Sales > 2500) %>%
  arrange(desc(Marketing), Sales)

# 5-B: Create a Look-up table countries of each office and join it to the the company table 
Countries <- data.frame(
              City=c("Boston", "Frankfurt", "London", "New York", "San Francisco", "Singapur"),
              Country=c("US","DE","GB","US","US","SG")
)

# Use left join with different variables for x and y table
Companies2 <- left_join(Companies, Countries, by=c("Office" = "City"))

# 5-C: Use filter() from dplyr
filter(Companies2, Country!="US")

## EXERCISE 6

# 6-A: Compute standard deviance of all rows of diamonds
apply(diamonds[,-2:-4], 1, sd)

# 6-B: Apply mean to all columns
apply(diamonds[,-2:-4], 2, mean)

## EXERCISE 7

# 7-A: Compute linear model (lm) between "carat" and "price" for each group of
# "cut" using foreach
# Assumes you've setup your cluster already
foreach(group = split(diamonds, diamonds$cut)) %do% lm(group$carat ~ group$price)

## EXERCISE 8

# 8-A: Write your own sdC function which computes the standard deviance in C++
cppFunction('double sdC(NumericVector x) {
  double m = mean(x);
  double deviances = 0;
  for (int i = 0; i < x.length(); i++) {
    double deviance = x[i] - m;
    deviances += deviance * deviance;
  }
  double mean_deviances = deviances / x.length();
  return sqrt(mean_deviances);
}')

## EXERCISE 9

# 9-A: Replicate the heat-map example from http://docs.ggplot2.org/current/geom_tile.html
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))

# 9-B: Analyse data structure needed for heat-maps
faithfuld
str(faithfuld)

# 9-C: Explore additional themes
library(ggthemes)
ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
       geom_point() +
       theme_economist() + scale_colour_economist()

