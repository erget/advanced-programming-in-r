################################################
## R Training for Data Analytics  ##############
## Authors: Dr. Matthias Duschl   ##############
##          Dr. Daniel Lee        ##############
################################################
################################################

################################################
## LECTURE SCRIPT  #############################
################################################
## 1. Recap: read multiple files ###############
## 2. Functional programming     ###############
## 3. Data wrangling             ###############
## 4. Loops & parallelization    ###############
## 5. Advanced visualization     ###############
## 6. Outlook: Git etc.          ###############
################################################


################################################
## 1. Recap: read multiple files ###############
################################################

library(data.table)

setwd("U:/Desktop/R for data analytics/data")

# generate 36 data sets w/ same data and filename structure
for(i in 2013:2015){
  for(j in 1:12){
    MonthlySales <- data.table(Division = rep(LETTERS[1:3],each=3)
                               , Office = rep(c("New York","Frankfurt","London"),3)
                               , Sales = sample(2000:4000,9))
    write.table(MonthlySales, file = paste("Sales_",i,"_",j,".csv", sep =""), sep = ",", quote = TRUE)

  }
}


# pattern understands regular expressions
# "+" after "Sales_" gives a matching if "Sales_" occurs at least once in a filename (easy rule)
dirFiles <- list.files(pattern = "Sales_+")

# read data from a folder
AllSales <- data.table()   # initialize AllSales

for(fname in dirFiles){
  tmpSales <- read.table(file = fname, header = T, sep = ",")
  
  # create Year and Month from filename
  tmpNumbers <- gsub("[^0-9]","",fname)
  tmpSales$Year <- as.numeric(substr(tmpNumbers, 1, 4))
  tmpSales$Month <- as.numeric(substr(tmpNumbers, 5, nchar(tmpNumbers)))
 
  # stack data
  AllSales <- rbind(AllSales, tmpSales)
}

# inspect data
AllSales


################################################
## 2. Functional programming ###################
################################################

# A demonstration of recursion vs. a loop
qsort <- function(v) {
  if (length(v) > 1) {
    pivot <- v[1]
    c(qsort(v[v < pivot]), v[v == pivot], qsort(v[v > pivot])) 
  } else v
}

qsort(rnorm(10))

## Partially applied functions
# Take a predictor and return a function which computes r² of lm against supplied variable
calcRsq <- function(pred) {
  function(y) {
    summary(lm(y ~ pred))$r.squared
  }
}

# Make some sample data
pred <- data.frame(pred1 = rnorm(100, 2, 1),
                   pred2 = 1:100,
                   pred3 = rpois(100, 2),
                   pred4 = 200:101)
resp <- data.frame(resp1 = 1:100,
                   resp2 = rnorm(100, 2, 1),
                   resp3 = 200:101,
                   resp4 = rpois(100, 2))
# Prime function to work with this predictor
calcRsq_pred1 <- calcRsq(pred$pred1)
# Apply that function to work with all samples
apply(resp, 2, calcRsq_pred1)

## Classes
# You can write a "method" for a class by appending the class to the function name
# This function will be called when summary is called on an object with class spam
summary.spam <- function(x) {
  print("This object has class is spam!")
}
foo <- 1:5  # foo is a normal integer
class(foo)
summary(foo)
# Now we extend foo to also be of class spam
class(foo) <- append(class(foo), "spam")
# When we call summary on foo, our special spam function is called
summary(foo)

# You can write your own such functions even if they don't exist already
report <- function(x) {
  UseMethod("report", x)
}
report.integer <- function(x) {
  print(sum(x))
  print(min(x))
  print(max(x))
}
report.character <- function(x) {
  print("This character object contains the following entries:")
  print(x)
}
report(1:5)
report("Now a different function is called because this is a character vector.")


################################################
## 3. Data wrangling ###########################
################################################

###### Data.table intro ######

# load data.table package
library(data.table)

# set working directory
setwd("U:/Desktop/R for data analytics/data")

# create data set
n = 200
comp_data <- data.frame(
  Company = paste("Company",1:n),
  Office  = sample(c("New York","Frankfurt","London","Boston","Singapur","San Francisco")
                   , size = n, replace = T, prob = sample(1:100,6)),
  Infrastructure = sample(c("weak","strong"), size = n, replace = T, prob = c(0.3,0.7)),
  Marketing = round(runif(n, 0, 0.7), digits = 0),
  Contracts = sample(c("Few","Medium","Many"), size = n, replace = T, prob = c(0.2,0.3,0.5)),
  Sales = sample(2000:4000, n, replace = TRUE),
  Profit = sample(100:500, n, replace = TRUE)
)

# have a glimpse at Companies data
comp_data
head(comp_data) # show the first rows only
class(comp_data) # Companies is a data.frame object

# create a data.table object from a data.frame
# this enables to use the data.table package functionality
Companies <- as.data.table(comp_data)
Companies
class(Companies)
# remember: a data.table is always also a data.frame. It has two classes. 
# That means all functions working for data.frames also work for data.tables!

# Instead of converting a data.frame to a data.table, you can also create a data.table (see CheatSheet)
DT = data.table(x=c("b","b","b","a","a"),
                y=rnorm(5),
                z=runif(5))
class(DT)

###### Subsetting rows ######

# some filter operations on rows
Companies[3:5,,]
Companies[3:5,]
Companies[3:5] 
Companies[Office == "London"] 
Companies[Contracts %in% c("Medium","Many")] 
Companies[(Office == "London" & Infrastructure == "strong") | Marketing == 1]

###### Selecting and manipulating columns ######

# selecting columns
# note the .() notation for specifying the variables
Companies[,.(Company, Sales, Profit)]
Companies[,list(Company, Sales, Profit)]

# aggregate columns
Companies[,.(AvgProfit = mean(Profit, na.rm=TRUE), SdProfit = sd(Profit, na.rm=TRUE))]
# automatic recycling
Companies[,.(Profit, AvgProfit = mean(Profit, na.rm=TRUE), SdProfit = sd(Profit, na.rm=TRUE))]

###### Grouping with by ######

# notation: data[ , , by= ...]
# group by Contracts
Companies[, .(SumSales = sum(Sales, na.rm=T)), by = Contracts]
# group by two variables or more
Companies[, .(AvgSales = mean(Sales, na.rm=T)), by =.(Contracts, Marketing)]
# the .N function for the count of rows per group
Companies[, .N, by = Contracts]
# use-defined functions for grouping via by= ; E.g. ifelse()
Companies[, .(.N, AvgSales = mean(Sales, na.rm=T)), by = .(StarGroup = ifelse(Profit < 400,0,1))]


## vvvvvvvvvvvvvvvvvvvvvvvvvvvv##
## ------- EXERCISE 1 -------- ##

###### Adding/Updating columns with := ######

# Updating col Office
tempCompanies <- Companies[Office == "San Francisco", Office := "S.F."]
# CAUTION:  assignment (<-) by reference, not copy. I.e. direct manipulation of Companies table;

# Catching up on the CAUTION note
WannaBeCopy <- Companies

# Add a new col "ProfitMargin"
Companies[, ProfitMargin := Profit/Sales]

# See what happened to "WannaBeCopy"
WannaBeCopy
# To avoid this behaviour and get a real copy, use data.table::copy(), see below

# Remove a col
Companies[, ProfitMargin := NULL]

# Create a physical copy and add ProfitMargin again
RealCopy <- copy(Companies)
Companies[, ProfitMargin := Profit/Sales]
Companies
RealCopy

# Adding/ Updating two cols or more simultaneously
Companies[, ProfitMargin := Profit/Sales]
Companies[, c("ProfitMargin","CompanyID") := .(round(ProfitMargin,digits = 4)*100, 1:(.N))]

# Adding calculated variables across within groups
Companies[, AvgProfit := mean(Profit, na.rm=TRUE), by =.(Contracts, Marketing)]

# Removing multiple cols
Companies[, c("CompanyID","ProfitMarginPCT") := NULL]

# For referring to variables, external to the data set, use "()"
ColsToBeDeleted <- c("CompanyID","ProfitMargin")
Companies[, (ColsToBeDeleted) := NULL]


###### Sort, reorder and rename cols (also by reference)###### 
# get back original data
Companies <- as.data.table(comp_data)

# setorder() to sort by two or more vars
setorder(Companies, -Infrastructure, Marketing, Contracts)
# "-" sorts descending, defaults to ascending

# reorder columns with setcolorder()
colNames <- names(Companies)
setcolorder(Companies,c(colNames[1:2],colNames[4:7],colNames[3])) # reorder data set 

# use setnames() to rename columns


## vvvvvvvvvvvvvvvvvvvvvvvvvvvv##
## ------- EXERCISE 2 -------- ##

###### Joining / Merging ######
?merge # show help file
# Country table
Countries <- data.table(City = c("London", "Boston", "New York", "Singapur", "Frankfurt", "San Francisco"),
                        Country = c("UK", "US", "US", "SG", "DE", "US")  )
# Left join
merge(Companies, Countries, by.x = "Office", by.y = "City", all.x=TRUE)


###### "Chaining" operations/statements ######
# no chaining (tmp variable needs to be created)
tmp <- Companies[, .(SumSales = sum(Sales)), by=Office]
tmp[SumSales > 100000]
# chaining
Companies[, .(SumSales = sum(Sales)), by=Office][SumSales > 100000]

###### lag / shift operations ######
# Calculate the lag-1 and lag-2 variables within a group
TS_data = data.table(year=rep(2010:2011, each=3), v1=1:6)
TS_data[, c("lag1", "lag2") := shift(.SD, 1:2, fill=0, type="lag"), by=year]


###### .SD and .SDcols ######
# Computing (grouped) mean over "all other than by-variables"
Companies[, lapply(.SD, mean, na.rm = TRUE)
          , by = .(Infrastructure, Marketing, Office)
          , .SDcols = c("Profit", "Sales")] # specification of .SDcols after by= with comma separator



###### Re-shaping data - wide & long ###### 

# data.table melt() and dcast()
?melt.data.table

# produce data in wide format
n <- 9
CompanySales <- data.table(Company = paste0("Company ",1:n)
                           , Office = sample(c("New York","Frankfurt","London","Boston","Singapur","San Francisco")
                                             , size = n, replace = TRUE)
                           , Sales_Prod1 = sample(1000:5000,n)
                           , Sales_Prod2 = sample(1000:5000,n)
                           , Sales_Prod3 = sample(1000:5000,n))

# using base::reshape for melting multiple columns (but not reshape2)
longSales0 <- reshape(data = CompanySales, 
                      varying = names(CompanySales)[3:5], 
                      v.names = "Sales", 
                      timevar = "Product", 
                      idvar = c("Company","Office"), 
                      times = paste0("Product ",1:3)
                      , direction = "long")

# data.table::melt() all three "Sales" columns to obtain long format
longSales1 = melt(CompanySales, id.vars = c("Company","Office")
                  , measure.vars = paste("Sales_Prod",1:3,sep="")
                  , variable.name = "Product", value.name = "Sales")

## vvvvvvvvvvvvvvvvvvvvvvvvvvvv##
## ------- EXERCISE 3 -------- ##

# use patterns() in melt() 
longSales2 = melt(CompanySales, id.vars = c("Company","Office")
                  , measure.vars = patterns("^Sales")
                  , variable.name = "Product", value.name = "Sales")

# dcast() - the reverse operation. Get back to wide format
wideSales <- dcast(longSales1, Company + Office ~ Product, 
                               value.var = "Sales")

## -- OPTIONAL CONTENT -- ##
# Extend the CompanySales data by Profit cols
CompanySales[, paste("Profit_Prod",1:3, sep="") := 
               .(sample(100:500,n), sample(100:500,n), sample(100:500,n))]

# SalesProduct and ProfitProduct in one column (as before)
# Challenge: We have two different measures (Sales & Profit)
longSales3 = melt(CompanySales, id.vars = c("Company","Office")
                  , measure.vars = 3:8
                  , variable.name = "Measure", value.name = c("Value"))

wideSales <- dcast(longSales3, Company + Office ~ Measure
                               , value.var = "Value")

# melt() Sales and Profit simultaneously (as two separate variables)
longSales4 = melt(CompanySales, id.vars = c("Company","Office")
                  , measure.vars = patterns("^Sales","^Profit")
                  , variable.name = "Product", value.name = c("Sales","Profit"))

# dcast Sales and Profit simultaneously
wideSales <- data.table::dcast(longSales4, Company + Office ~ Product
                                           , value.var = c("Sales","Profit"))
## -- END OPTIONAL CONTENT -- ##


###### dplyr ######

library(dplyr) 

# generate data set
n = 200
Companies <- data.frame(
  Company = paste("Company",1:n),
  Office  = sample(c("New York","Frankfurt","London","Boston","Singapur","San Francisco")
                   , size = n, replace = T, prob = sample(1:100,6)),
  Infrastructure = sample(c("weak","strong"), size = n, replace = T, prob = c(0.3,0.7)),
  Marketing = round(runif(n, 0, 0.7), digits = 0),
  Contracts = sample(c("Few","Medium","Many"), size = n, replace = T, prob = c(0.2,0.3,0.5)),
  Sales = sample(2000:4000, n, replace = TRUE),
  Profit = sample(100:500, n, replace = TRUE)
)

# tbl_df() convert to a "local df" -> more readable if printed to console
Companies <- tbl_df(Companies)
# ... or use data.table class, which is also supported by dplyr
Companies <- as.data.table(Companies)


###### dplyr::filter() examples ###### 

# filtering in base R:
Companies[Companies$Sales > 3000 & Companies$Marketing == 1, ]
# dplyr:
f1 <- filter(Companies, Sales > 3000, Marketing == 1) # "&" for AND would also work
f2 <- filter(Companies, Office == "Frankfurt" | Office == "London") # "|" means OR
f3 <- filter(Companies, Office %in% c("Frankfurt","London")) # alternative: %in% operator


######dplyr::select() examples ###### 

# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(Companies, Marketing, Contracts, Sales)
select(Companies, Infrastructure:Contracts, contains("Sal"))

# "Chaining" instead of nesting operations:
# select() is nested in filter():
filter(select(Companies, Contracts, Sales), Sales > 3500)

# could use %>% operator (can be read as "then"). More readable code
Companies %>% 
  select(Contracts, Sales) %>% 
  filter(Sales > 3500)

# Pipe operator (%>%) available, when dplyr is loaded
# Globally available, through package magrittr


###### arrange() to reorder rows ###### 

# sorting data.frames in R: one of the most viewved stack-overflow questions
# https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns

# dplyr
arrange(select(Companies, Company, Sales), Sales)
# or using pipe operator:
Companies %>%
  select(Company, Sales) %>%
  arrange(Sales)

# order by two columns, where one is ordered by descending:
arrange(select(Companies, Company, Sales, Marketing), desc(Marketing), Sales)


###### mutate() to add new (calculated/derived) variables ###### 

# base R - adding a new var "Speed" to the df
Companies$Margin <- Companies$Profit / Companies$Sales
# dplyr
Companies <- mutate(Companies, Margin = Profit*100/Sales)
# data table
Companies <- as.data.table(Companies)
Companies[, Margin:=Profit*100/Sales]


## vvvvvvvvvvvvvvvvvvvvvvvvvvvv##
## ------- EXERCISE 4 -------- ##


###### summarise() and group_by() ###### 

# base R approaches to calculate the average Sales by Marketing
head(with(Companies, tapply(Sales, Marketing, mean, na.rm=TRUE))) # approach1
head(aggregate(Sales ~ Marketing, Companies, mean)) # approach2 (slower)

# dplyr
summarise(group_by(Companies, Marketing), Avg_Sales = mean(Sales))
summarise(group_by(Companies, Marketing), mean(Sales), mean(Profit))

# summarise_at() to apply aggregation function to multiple columns selected by vars()
summarise_at(group_by(Companies, Marketing), vars(Sales, Profit), mean)

# use funs() to to generate a named list of functions
summarise_at(group_by(Companies, Marketing),
               vars(Sales, Profit),
               funs(min(., na.rm=TRUE), max(., na.rm=TRUE), mean(.), sd(.)) 
               )

# n(), n_distinct(vector) can be used within summarise, mutate, filter
summarise(group_by(Companies, Office, Infrastructure), Count = n())
summarise(group_by(Companies, Infrastructure), OfficesCount = n_distinct(Office))

# n_distinct(vector) can also be used outside summarise(); Faster than length(unique(vector))
n_distinct(Companies$Office)

# TODO: Complete
################################################
## 4. Loops & parallelization ##################
################################################

require(ggplot2)
## index for start and end rows to be extracted
starts <- seq(1, nrow(diamonds), 2000)
ends <- c(starts[-1] - 1, nrow(diamonds))

# Create new directory to save files to
dir <- "results"
dir.create(dir, showWarnings = FALSE)

# Looping through the files
for (i in seq(starts)) {
  
  # Indices for current iteration
  start <- starts[i]
  end <- ends[i]
  # Subset data based on row indices
  data <- diamonds[start:end, ]
  
  outfile <- paste0(dir, "/", "diamonds_subset_", sprintf("%05.0f", start), "_",
                    sprintf("%05.0f", end), ".csv")
  write.csv(data, outfile, row.names = FALSE)
}

# Recursive quicksort implementation - this won't work well for long vectors!
qsort <- function(v) {
    if (length(v) > 1) {
          pivot <- v[1]
    c(qsort(v[v < pivot]), v[v == pivot], qsort(v[v > pivot])) 
      } else v
}
qsort(rnorm(10))

# Example of using lapply flexibly
l <- list(mean, sd)
lapply(l, function(fun) {fun(1:5)})

# doParallel example
library(doParallel)
# How many cores are available in this environment?
nodes <- detectCores()
# If doing interactively, you may want to reduce this.
cluster <- makeCluster(nodes)
# Register the cluster we craeted for use in parallel work
registerDoParallel(cluster)  # This is not functional programming!
# Now you can do things in parallel
foreach(i = 1:10) %do% sqrt(i)  # Evalutes expression and returns list of results
foreach(1:10, .combine = "c") %do% sqrt(i)  # Combine results into a vector
foreach(1:10, .combine = "rbind") %do% sqrt(i)  # Combine results into a matrix
# When you're done, release the cluster
showConnections()  # This just shows you how many connections you have up
stopCluster(cluster)

# Rcpp
library(Rcpp)
# Simple expressions
evalCpp("1 + 1")  # Takes a while the first time
evalCpp("1 + 1")  # Already compiled
evalCpp("1+1")  # Gets compiled again!

# Functions
# Scalar input, scalar output
cppFunction('int signC(int x) {
  if (x > 0) return 1;
  else if (x == 0) return 0;
  else return -1;
}')

signC(-1)
signC(0)
signC(1)

# Vector input, vector output
cppFunction("NumericVector colMeansC(NumericMatrix x) {
  int nCol = x.ncol();
  int nRow = x.nrow();
  
  // Create output vector
  NumericVector out(nCol);
  
  // Create intermediate variable of size nrow(x) to store column values in
  NumericVector column(nRow);
  
  // loop over each column
  for (int i = 0; i < nCol; i++) {
    // Retrieve values in current column
    column = x(_, i);
    // store mean of current 'nVal' in 'out[i]'
    out[i] = mean(column);
  }
  return out;
}")

# From a file, it's cleaner
sourceCpp("corC.cpp")  # Don't forget the [[Rcpp::export]] comment

# Benchmarking
microbenchmark({
  # Block to be executed
  1:100L
})

################################################
## 5. Advanced visualization ###################
################################################

###### ggplot2 ######

library(ggplot2)

# Create data set
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


# simple scatter plot
ggplot(data=Companies, aes(x=Sales, y=Profit)) +
  geom_point() 

# ... and a bit more customized 
ggplot(data=Companies, aes(x=Sales, y=Profit)) +
  geom_point(pch=4, color="darkgreen", size=2) +
  labs(title="Company Data", x="Sales (2016)", y="Profit (2016)")

# adding a layer: a smooth line
ggplot(data=Companies, aes(x=Sales, y=Profit)) +
  geom_point(pch=4, color="darkgreen", size=2) +
  geom_smooth(method="lm", color="orange", linetype=2) +
  labs(title="Company Data", x="Sales (2016)", y="Profit (2016)")

# use different colors for marketing 
ggplot(data=Companies, aes(x=Sales, y=Profit, col=Infrastructure)) +
  geom_point(pch=4, size=2) +
  geom_smooth(method="lm", color="orange", linetype=2) +
  labs(title="Company Data", x="Sales (2016)", y="Profit (2016)") +
  scale_colour_brewer(palette = "Set1")

# facetting by Infrastructure and Contracts
ggplot(data=Companies, aes(x=Sales, y=Profit, shape=Infrastructure, color=Contracts)) +
  geom_point(size=2) +
  facet_grid(Marketing~Contracts) +
  labs(title="Company Data", x="Sales", y="Profit") +
  scale_colour_brewer(palette = "Set3") +
  theme_dark()
# use scales = "free" in facet_grid, for each facet to have its own scale. See ?facet_grid

# bar plots
# NOTE: Grouping is achieved by assigning a variable to the fill option
ggplot(Companies, aes(x=Contracts, fill=Marketing)) +
  geom_bar(stat="count")  
# try another position= argument
ggplot(Companies, aes(x=Contracts, fill=Marketing)) +
  geom_bar(position="dodge", stat="count")

# Combining multiple geom layers
ggplot(Companies, aes(x=Contracts, y=Sales)) +
  geom_boxplot(fill="lightblue",
               color="black", notch=TRUE) +
  geom_point(position="jitter", color="blue", alpha=.5) +
  geom_rug(sides="r", color="lightsteelblue")


## vvvvvvvvvvvvvvvvvvvvvvvvvvvv##
## ------- EXERCISE 5 -------- ##

###### plotly ######

# plotly
library(plotly)

# ggplotly() converts ggplot2 graphic objects in interactive plotly objects (= htmlwidgets) 
gg <- ggplot(data=Companies, aes(x=Sales, y=Profit, color=Contracts)) +
  geom_point(size=2) +
  facet_grid(~Contracts) +
  labs(title="Company Data", x="Sales", y="Profit") 
gg_ly <- ggplotly(gg)

gg_ly


# A plotly graph can also be built from scratch using plot_ly()
# core arguments are type, group, color, colors, symbol, symbols, size
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = ~carat, y = ~price, color = ~carat,
        size = ~carat, text = ~paste("Clarity: ", clarity))
# See https://plot.ly/r/ for more examples/code snippets/tutorials
p <- plot_ly(z = volcano, type = "surface")
p

p <- plot_ly(z = volcano, type = "contour")
p


###### shiny ######

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput(inputId = "SSize",
              label = "Sample size",
              value = 100, min = 1, max = 1000),
  radioButtons(inputId = "DistChoice",
               label = "Choose sampling distribution",
               choices = c("Uniform", "Normal"),
               selected = "Uniform", inline = T),
  plotOutput("SampleHist")
)

server <- function(input, output){
  output$SampleHist <- renderPlot({
    if(input$DistChoice == "Normal"){
      data <- input$SSize %>% rnorm
      #data <- rnorm(input$SSize)
    }else{
      data <- input$SSize %>% runif(-1, 1)
    }
    qplot(data, geom = "histogram"
          , main = paste(input$DistChoice,"data"))
  })
}

shinyApp(ui = ui, server = server)

## vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ##
## FOR MORE shiny, see separate shiny .R files ##


################################################
## 6. git                    ###################
################################################

# TODO: Do this
