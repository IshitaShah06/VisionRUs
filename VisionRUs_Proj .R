#Team: Vision R US

train <-read.csv("A:/KDD_Hamsa/Project/Project_data/train.csv")
dim(train)
test <-read.csv("A:/KDD_Hamsa/Project/Project_data/test.csv")
dim(test)
store <-read.csv("A:/KDD_Hamsa/Project/Project_data/store.csv")
new_store <-read.csv("A:/KDD_Hamsa/Project/Project_data/newstore.csv")
dim(new_store)
new_train <-read.csv("A:/KDD_Hamsa/Project/Project_data/newtrain.csv")
dim(new_train) # Preprocessed by removing Sales zero
# Load data and check the dimensions of each dataset

#all the columns, their datatype is clear through this command.
str(train)
str(test)
str(store)

#summary of all these datasets and then helps get the min, max, IQR information foreach variable in the dataset.
# we are doing some statistical test on train and store data before we go ahead with exploring relationship between sales and input variables.

summary(train)
summary(test)
summary(store)

#In test set, there is only 856 stores which are included in training set which contains 1115 stores.
#we only need to generate models for 856 stores instead of the total of 1115
#In training set, the period range is from 2013-01-01 to 2015-07-31.
#In test set, the period we need to predict is from 2015-08-01 to 2015-09-17.

# The target variable would be sales
Sum.Sales<-summary(train$Sales)
Sum.Sales

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0    3727    5744    5774    7856   41551 

# Join the train dataset with the store dataset on StoreId column
library('sqldf')

# Merging train and store data set using Store column 

Joined_DS <- bind_rows(train,test)

length(which(is.na(Joined_DS)))

Joined_DS <- left_join(new_train,new_store, by = "Store")

length(which(is.na(Joined_DS)))


# Visualization on train and store data before exploring relationship between sales and input variables.

# Bargraph of Stateholiday
sum.sH<-summary(train$StateHoliday)

barplot(summary(train$StateHoliday),
        ylim = c(0, 1100000),
        main = "Bar Graph of StateHolidays",
        col = "light Blue")
box(which = "plot",
    lty = "solid",
    col="black")

#Bargraph of Day of week

str(train$DayOfWeek)
plot(train$DayOfWeek)

#hist(train$DayOfWeek,xlab="Days Of Week")

#Histogram of Customers

hist(train$Customers, 100)
plot(train$Customers)

#Histogram of Sales

sum.Sales<-summary(train$Sales)
hist(train$Sales, 100)
hist(train$Sales,xlab="Sales")
boxplot(train$Sales)

#Frequency of Open

sum.op<-summary(train$Open)
hist(train$Open, 100)
hist(train$Open,xlab="Shops Open Frequency")
plot(train$Open)

#Histogram of Promo

hist(train$Promo,xlab="Promo Frequency")

#Histogram of School Holiday

sum.School<-summary(train$SchoolHoliday)
hist(train$SchoolHoliday,xlab="School Holiday")


#The stores have different amounts of days with zero sales. There are spikes in the sales before the stores close and after the reopen:

zerosPerStore <- sort(tapply(train$Sales, list(train$Store), function(x) sum(x == 0)))
hist(zerosPerStore,100)

#Stores with the most zeros in their sales:
tail(zerosPerStore, 10)
  
#Histogram of CompetitionDistance
hist(store$CompetitionDistance, 100)
 
#Graphs on Store dataset
   
hist(new_store$CompetitionDistance)
hist(new_store$CompetitionOpenSinceMonth)
hist(new_store$CompetitionOpenSinceYear)
hist(new_store$Promo2)
hist(new_store$Promo2SinceWeek)
hist(new_store$Promo2SinceYear)
hist(new_store$PromoInterval)
   
#Graph on Customers vs Sales  
plot(Sales~Customers,data=train)
   
plot(tapply(new_train$Sales,new_train$Month,mean),xlab="Month",ylab="Mean of Sales")
   
tapply(new_train$Sales,new_train$DayOfWeek,mean)
   
plot(tapply(new_train$Sales,new_train$Day,mean),xlab="Day of Month",ylab="Mean of Sales")
   
# Distribution of the CompetitionDistance, has 3 NA values only
ggplot(new_store, aes(x = CompetitionDistance)) + geom_density() + labs(title = "Distribution of CompetitionDistance")
   
# three vaues that are missing are ignored here.
   
#Plotting a graph for customers vs sales
### Sales and Customers
   
   ggplot(Joined_DS, aes(x = Customers, y = Sales)) + 
     geom_point() + 
     labs(title = "Customers vs Sales")
   
   #Customers can be one of the predictors for sales since we are uncertain about the upcoming/future customers we hinder to take customers as one of the predictor variables
   # The graph shows the positive co relation between the number of customers to sales.We could see some outliers as well.
   
   #Plotting a graph for customers vs Sales based on store type
   ggplot(Joined_DS, aes(x = Customers, y = Sales)) + 
     geom_point(aes(colour = StoreType)) + 
     labs(title = "Customers and Sales")
   
   #The below graph tells the relation between sales and customers based on store type.
   library(ggplot2)
   ggplot(Joined_DS, aes(x = Customers, y = Sales)) + 
     geom_point(aes(colour = StoreType)) + 
     geom_smooth(data = Joined_DS[Joined_DS$StoreType == "a", ], formula = y ~ x) +
     geom_smooth(data = Joined_DS[Joined_DS$StoreType == "b", ], formula = y ~ x) +
     geom_smooth(data = Joined_DS[Joined_DS$StoreType == "c", ], formula = y ~ x) +
     geom_smooth(data = Joined_DS[Joined_DS$StoreType == "d", ], formula = y ~ x) +
     labs(title = "Customers and Sales")
   #It's possible to say that StoreType is an important variable, but this appears only true when there are more Customers, as StoreType *a* and *c* appear quite mixed until around 2000 Customers. 
   
   # Checking the relation between sales and promo
   
   ggplot(train, aes(x = Customers, y = Sales)) +
     geom_point(aes(colour = Promo)) + 
     labs(title = "Promo on Sales")
 
    # Relation between  Store type and assortment
   
   table(Joined_DS$StoreType, Joined_DS$Assortment)
   prop.table(Joined_DS(Joined_DS$StoreType, Joined_DS$Assortment))
   
   #Promo2
   # Sales on days of the week for three years 
   library(ggplot2)
   ggplot(Joined_DS[Joined_DS$Sales != 0,], aes(x = factor(DayOfWeek), y = Sales)) + 
     geom_jitter(alpha = 0.1) + 
     geom_boxplot(colour = "orange", outlier.colour = NA, fill = NA) + 
     labs(title = "Sales by Day and Year") +
     facet_wrap(~ Year)
# Over past three years it can be inferred that the maximum sales ocurs on 1st and 7th day of the week
   #############################################################################################
   #finds the rows with zero length in train dataset
    train[!complete.cases(train),]
  # No records has fields with value zero
    
   # No. of test stores that are present in train
   sum(unique(test$Store) %in% unique(train$Store)) #856 stores are present in test that are in train
   
   # No.of  train stores that are not present in test.
   sum(!(unique(train$Store) %in% unique(test$Store))) #259 stores are not present in test that are in train
   
   #percentage of promo in train
   table(train$Promo) / nrow(train)
   #61.8% stores are not on promo and only 38.15% stores are on promo
   
   #percentage of Open stores train data
   table(train$Open) / nrow(train)
   #16.98% stores are closed and 83.01% stores are open in train data
   
   #percentage of time there is school holiday in train data set  
   table(train$SchoolHoliday) / nrow(train)
  #82.1% of data has information on sales when there is no holiday for schools and 17.8% of data is when the school has holiday
    
   # missing values in store dataset
   
   length(store) # total no of fields in the store dataset --10
   length(which(is.na(store))) #Number of missing values in store dataset-- 1799
   length(which(!is.na(store))) # number of non-missing values in store dataset -- 9351
   
   # missing values in train dataset
   
   length(which(!is.na(train))) #-- 9154881
   length(which(is.na(train))) #-- 0
   # so we claim that train dataset does not have any missing values.
   
   
   ##Number of NA values in following fields in store dataset
   length(which(is.na(store$CompetitionDistance)))# 3 missing values
   length(which(is.na(store$CompetitionOpenSinceMonth)))# 354 missing values
   length(which(is.na(store$CompetitionOpenSinceYear)))  # 354 missing values
   length(which(is.na(store$Promo2SinceWeek))) # 554 missing values            
   length(which(is.na(store$Promo2SinceYear))) # 554 missing values          
   sum(store$PromoInterval=="") # 554 blank values
   
   #top 20 records from the train dataset
   
   head(train[order(train$Customers, decreasing=TRUE), ], 20)
   head(store[order(store$CompetitionDistance, decreasing=TRUE),],10)
   head(arrange(store, desc(CompetitionDistance)), n = 20)
   
   # Analyse which are the stores' Open is NAs
   
   test[is.na(test$Open), ] #we see store 622 has NA's
   test$Open[test$Store ==622] 
   train$Open[train$Store == 622] 
   #we can see test has NA values in store open status
   # only 11 stores with 622 has open NA values
   
   #Analysing the relationship between variables
   
   table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
         ifelse(train$Promo, "Promo", "No promo"))
   #Gives the count of stores participating in promo and not participating in promo when the sales is zero and when the sales are greater than zero
   
   table(ifelse(train$Open == 1, "Opened", "Closed"),
         ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))
   #Gives the count of stores opened and closed when the sales is zero and when the sales greater than zero
   
   table(ifelse(train$Open == 1, "Opened", "Closed"),
         ifelse(train$Customer >0, "Customers > 0", "Customers = 0"))
   
   #Gives the count of closed and opened stores when the customer count is zero and customers > 0
   
   table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
         ifelse(train$Customers, "Customers > 0", "Customers = 0"))
   #Gives the count of sales vs customer when sales is zero and customers are zero, sales zero and customers more than zero and vice versa
   
   
   #Analysing the above results we can find the factors that results in no. of sales,impact of sales when there are promotions
   #but stores are closed, stores are opened during promotions but no sales,sometimes the stores are opened on promotion but no sales.
   
   # Merging train and store data set using Store column 
   
   dim(train)
   
   dim(test)
   
   complete_data<- bind_rows(train,test)
   
   library('sqldf')
   
   complete_data<-sqldf("select complete_data.*,store.* from complete_data inner join store on complete_data.store = store.store")
   
   head(complete_data)
   
   dim(complete_data)
  # ******************************************************************************************************
   #Analysis on predictor variable Sales
   
   length(train$Sales[!train$Open]) / nrow(t)
   
   summary(train[train$Sales > 20000,])
   
   # Check for the data available on stores that are open
   openDays <- aggregate(new_train$Store,list(new_train$Store),length)
   head(openDays)
   
   #Sales average by year, month and day
   tapply(new_train$Sales,new_train$Year,mean)
   tapply(new_train$Sales,new_train$Date,mean)
   tapply(new_train$Sales,new_train$Month,mean)
   
   
   #Inferences
   
   avg_day<-tapply(new_train$Sales,new_train$Day,mean)
   plot(avg_day)
   
   avg_month<-tapply(new_train$Sales,new_train$Month,mean)
   plot(avg_month)
   
   avg_year<-tapply(new_train$Sales,new_train$Year,mean)
   plot(avg_year)
################################################################################################
#Skewness of train data with predictor variables
   install.packages("e1071")
   library(e1071)
   
   max(new_train$Sales) #maximum sales for train data set
   min(new_train$Sales) #minimum sales for train data set
   mean(new_train$Sales) 
   sd(new_train$Sales)
   skewness(new_train$Sales, na.rm = FALSE, type = 3)
  #Positive skewness would indicate that the mean of the data values is larger than the median, and the data distribution is right-skewed.
   
   skewness(new_train$Promo, na.rm = FALSE, type = 3)
   
   
   max(new_train$Customers)
   min(new_train$Customers)
   mean(new_train$Customers)
   sd(new_train$Customers)
   
   skewness(new_train$Customers, na.rm = FALSE, type = 3)
   
   #Positive skewness would indicate that the mean of the data values is larger than the median, and the data distribution is right-skewed.
   
   
   min(new_train$DayOfWeek)
   max(new_train$DayOfWeek)
   mean(new_train$DayOfWeek)
   sd(new_train$DayOfWeek)
   
   skewness(new_train$DayOfWeek, na.rm = FALSE, type = 3) #na.rm = false means ignore the NA values and type = 3 is the algorithm to be used.
   
   skewness(new_train$Open, na.rm = FALSE, type = 3) #na.rm = false means ignore the NA values and type = 3 is the algorithm to be used.
  
    #negative skewness indicates that the mean of the data values is less than the median, and the data distribution is left-skewed.
   skewness(new_train$SchoolHoliday, na.rm = FALSE, type = 3) #na.rm = false means ignore the NA values and type = 3 is the algorithm to be used.
   
   skewness(new_train$StateHoliday, na.rm = FALSE, type = 3) #na.rm = false means ignore the NA values and type = 3 is the algorithm to be used.
   
   #Skewness of store data with predictor variables
   
   str(new_store)
   skewness(new_store$CompetitionDistance, na.rm = FALSE, type = 3)
   skewness(new_store$CompetitionOpenSinceMonth, na.rm = FALSE, type = 3)
   skewness(new_store$CompetitionOpenSinceYear, na.rm = FALSE, type = 3)
   skewness(new_store$Promo2, na.rm = FALSE, type = 3)
   skewness(new_store$Promo2SinceWeek, na.rm = FALSE, type = 3)
   skewness(new_store$Promo2SinceYear, na.rm = FALSE, type = 3)
   skewness(new_store$PromoInterval, na.rm = FALSE, type = 3)
   
   #############################################################################################
   # Data Transformation/Pre-Processing
   
   #Import packages required for the datapreparation process
   
   library(naniar)
   
   library(rlang)
   
   library(dplyr)
   
   library(magrittr)
   
   library(ggplot2)
   
   library(readr)
   
   library(zoo)  
   
   library(forecast)
   
   library(sqldf)
   
   #================================================================================================================
   
   #Input data sets into data frames sample_submission, test, train
   

   store<- read.csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/Dataset/store/store.csv")
   
   train<- read.csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/Dataset/train/train.csv")
   
   test<- read.csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/Dataset/test/test.csv")
   
   train <- read_csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/Dataset/train/train.csv", col_types=list(
     Store = col_integer(),
     DayOfWeek= col_integer(),
     Date = col_date(),
     Sales = col_integer(),
     Customers = col_integer(),
     Open = col_integer(),
     Promo = col_integer(),
     StateHoliday = col_character(),
     SchoolHoliday = col_integer()))
   
   #================================================================================================================
   
   #Replacing the alphabetical values in train dataset column - StateHoliday
   
   #Since the StateHoliday column comprises of 
   
   new_train$StateHoliday <- as.character(new_train$StateHoliday)
   
   new_train$StateHoliday[new_train$StateHoliday == "a"] <- "2"
   
   new_train$StateHoliday[new_train$StateHoliday == "b"] <- "3"
   
   new_train$StateHoliday[new_train$StateHoliday == "c"] <- "4"
   
   new_train$StateHoliday <- as.integer(new_train$StateHoliday)
   
   typeof(new_train$StateHoliday)
   
   #================================================================================================================
   #Median of CompetitionDistance
   
   median <- median(new_store$CompetitionDistance,na.rm=TRUE)
   
   median
   
   #================================================================================================================
   
   #Replacing mode value for the values missing in the CompetitionOpenSinceMonth column of store dataset
   
   Mode <- function(x, na.rm = TRUE) {
     if(na.rm){
       x = x[!is.na(x)]
     }
     
     ux <- unique(x)
     return(ux[which.max(tabulate(match(x, ux)))])
   }
   
   x <- (new_store$CompetitionOpenSinceMonth)
   
   result<- Mode(x)
   
   result
   
   #================================================================================================================
   
   #Replacing mode value for the values missing in the CompetitionOpenSinceYear column of store dataset
   
   Mode <- function(y, na.rm = TRUE) {
     if(na.rm){
       y = y[!is.na(y)]
     }
     
     uy <- unique(y)
     return(uy[which.max(tabulate(match(y, uy)))])
   }
   
   y <- (new_store$CompetitionOpenSinceYear)
   
   result1<- Mode(y)
   
   result1
   
   
   new_store$CompetitionOpenSinceYear[is.na(new_store$CompetitionOpenSinceYear)] <- round(result1)
   
   new_store$CompetitionOpenSinceYear
   
   #================================================================================================================
   
   #Replacing mode value for the values missing in the Promo2Sinceweek column of store dataset
   
   
   Mode <- function(z, na.rm = TRUE) {
     if(na.rm){
       z = z[!is.na(z)]
     }
     
     uz <- unique(z)
     return(uz[which.max(tabulate(match(z, uz)))])
   }
   
   z <- (new_store$Promo2SinceWeek)
   
   result2<- Mode(z)
   
   result2
   
   
   new_store$Promo2SinceWeek[is.na(new_store$Promo2SinceWeek)] <- result2
   
   new_store$Promo2SinceWeek
   
   
   #================================================================================================================
   
   #Replacing mode value for the values missing in the CompetitionOpenSinceYear column of store dataset
   
   Mode <- function(a, na.rm = TRUE) {
     if(na.rm){
       a = a[!is.na(a)]
     }
     
     ua <- unique(a)
     return(ua[which.max(tabulate(match(a, ua)))])
   }
   
   a <- (new_store$Promo2SinceYear)
   
   result3<- Mode(a)
   
   result3
   
   
   new_store$Promo2SinceYear[is.na(new_store$Promo2SinceYear)] <- result3
   
   new_store$Promo2SinceYear
   
   #================================================================================================================
   
   #Replacing NA for the values missing in the PromoInterval column of store dataset
   
   new_store$PromoInterval[store$PromoInterval == ''] <- NA
   
   (new_store$PromoInterval = 'Jan,Apr,Jul,Oct') <- a
   
   new_store$PromoInterval
   
   replace(new_store$PromoInterval,new_store$PromoInterval=="Jan,Apr,Jul,Oct",1)
   
   new_store$PromoInterval[is.na(new_store$PromoInterval)] <- ("Jan,Apr,Jul,Oct") 
   
   
   ------------
     
     match(new_store$PromoInterval,month.abb)
   
   max1 <- max(unlist(new_store$PromoInterval, na.rm=TRUE))
   
   max1
   
   #================================================================================================================
   
   
   #================================================================================================================
  
   
   ##get the date in separate columns as day, month and year, converting them in form of integers and the format as M-D-YYYY
   library(Rcpp)
   library(dplyr)
   library(lubridate)
   library(anytime)
   new_train$Date <- as.Date(new_train$Date, "%Y-%m-%d")
   #converts the date as YYYY-M-D
   new_train <- mutate(new_train,Day = as.integer(format(Date, "%d")))
   new_train <- mutate(new_train,Month = as.integer(format(Date, "%m")))
   new_train <- mutate(new_train,Year = as.integer(format(Date, "%Y")))
   head(new_train)
   #############################################################################################
   #Log Transformation of data
   
   boxplot(new_store$Promo2, new_store$CompetitionOpenSinceMonth)
   qqnorm(new_store$CompetitionDistance,main=" Competition Distance ")
   qqline(new_store$CompetitionDistance)
   lognew_store <- log(new_store$CompetitionDistance)
   
   hist(lognew_store,main=" CompetitionDistance ")
   #nearly equally distributed
   
   qqnorm(lognew_store,main="QQ Plot for the Log of the Competition Distance")
   qqline(lognew_store)
   
   meanoflognew_store <- mean(lognew_store)
   sd <- sd(lognew_store)
   n <- length(lognew_store)
   
   se <- sd/sqrt(n)
   #confidence variable
  # Calculating a Confidence Interval From a Normal Distribution
   
   error <- se*qt(0.975,df=n-1)
   #range of confidence variable
   left <- meanoflognew_store-error
   right <- meanoflognew_store + error
   
   -------------------------------------------------------------------------
   new_store <-read.csv("C:/ch2hw/Vision R Us/newstore.csv")
   
   
   logn_CompetitionOpenSinceMonth <- log(new_store$CompetitionOpenSinceMonth)
   
   hist(logn_CompetitionOpenSinceMonth,main=" CompetitionOpenSinceMonth ")
   #nearly equally distributed
   
   hist(new_store$CompetitionOpenSinceMonth)
   #gives an errr that data has non-numeric values
   
   qqnorm(logn_CompetitionOpenSinceMonth,main="QQ Plot for the Log of the CompetitionOpenSinceMonth")
   qqline(logn_CompetitionOpenSinceMonth)
   #the graph is not normally distributed.
   meanoflognew_store <- mean(logn_CompetitionOpenSinceMonth)
   sd <- sd(logn_CompetitionOpenSinceMonth)
   n <- length(logn_CompetitionOpenSinceMonth)
   
   se <- sd/sqrt(n)
   #confidence variable
   #Calculating a Confidence Interval From a Normal Distribution
   
   error <- se*qt(0.975,df=n-1)
   #range of confidence variable
   left <- meanoflognewstore-error
   right <- meanoflognewstore + error
   
   
   # Non-numeric aruguements
   -----------------------------------------------
   new_train <- read.csv("C:/ch2hw/Vision R Us/newtrain.csv")
   logn_Sales <- log(new_train$Sales)
   
   hist(logn_Sales,main=" Sales ")
   #No Outliers
   
   hist(new_train$Sales)
   hist(logn_Sales)
   hist(mmnorm.sales)

   qqnorm(new_train$Sales,main="QQ Plot for the Log of the CompetitionOpenSinceMonth")
   #log one is giving error to define ylim
   
   qqline(new_train$Sales)
   qqline(logn_Sales)
   
   #the graph is not normally distributed.
   Log_CompetitionSinceYear <- log(new_store$CompetitionOpenSinceYear)
   
   logn_Promo2 <- log(new_store$Promo2)
   
   logn_Promo2SinceWeek <- log(new_store$Promo2SinceWeek)
   
   logn_Promo2SinceYear <- log(new_store$Promo2SinceYear)
   
   logn_PromoInterval <- log(new_store$PromoInterval)
   
   
   
   #Train
   logn_DayofWeek <- log(newtrain$DayOfWeek)
   
   
   logn_Open <- log(new_train$Open)
   logn_Promo <- log(new_train$Promo)
   
   
   #error
   
   a <-as.numeric(new_train$StateHoliday)
   logn_StateHoliday <- log(a)
   
   b <-as.numeric(new_train$SchoolHoliday)
   logn_SchoolHoliday <- log(b)
   
   #############################################################################################
   
   #Normalise the variables for train dataset
   
   #Normalize the sales variable of train dataset
   mmnorm.sales <- (new_train$Sales - min(new_train$Sales))/(max(new_train$Sales) - min(new_train$Sales))
   mmnorm.sales
   
   #Normalize the Store variable of train dataset
   
   mmnorm.store <- (new_train$Store - min(new_train$Store))/(max(new_train$Store) - min(new_train$Store))
   mmnorm.store
   
   #Normalize the DayOfWeek variable of train dataset
   
   mmnorm.DOW <- (new_train$DayOfWeek - min(new_train$DayOfWeek))/(max(new_train$DayOfWeek) - min(new_train$DayOfWeek))
   mmnorm.DOW
   
   #Normalize the Customers variable of train dataset
   
   mmnorm.Customers <- (new_train$Customers - min(new_train$Customers))/(max(new_train$Customers) - min(new_train$Customers))
   mmnorm.Customers
   
   #Normalize the Open variable of train dataset
   
   mmnorm.Open <- (new_train$Open - min(new_train$Open))/(max(new_train$Open) - min(new_train$Open))
   mmnorm.Open
   
   #Normalize the Promo variable of train dataset
   
   mmnorm.Promo <- (new_train$Promo - min(new_train$Promo))/(max(new_train$Promo) - min(new_train$Promo))
   mmnorm.Promo
   
   #Normalize the SchoolHoliday variable of train dataset
   
   mmnorm.SchoolHoliday <- (new_train$SchoolHoliday - min(new_train$SchoolHoliday))/(max(new_train$SchoolHoliday) - min(new_train$SchoolHoliday))
   mmnorm.SchoolHoliday
   
   #Normalize the StateHoliday variable of train dataset
   
   mmnorm.StateHoliday <- (new_train$StateHoliday - min(new_train$StateHoliday))/(max(new_train$StateHoliday) - min(new_train$StateHoliday))
   mmnorm.StateHoliday
 
   #Normalize the variables for store dataset
   
   mmnorm.CompetitionDistance <- (new_store$CompetitionDistance - min(new_store$CompetitionDistance))/(max(new_store$CompetitionDistance) - min(new_store$CompetitionDistance))
   mmnorm.CompetitionDistance

   mmnorm.CompetitionOpenSinceMonth <- (new_store$CompetitionOpenSinceMonth - min(new_store$CompetitionOpenSinceMonth))/(max(new_store$CompetitionOpenSinceMonth) - min(new_store$CompetitionOpenSinceMonth))
   mmnorm.CompetitionOpenSinceMonth
   
   mmnorm.CompetitionOpenSinceYear <- (new_store$CompetitionOpenSinceYear - min(new_store$CompetitionOpenSinceYear))/(max(new_store$CompetitionOpenSinceYear) - min(new_store$CompetitionOpenSinceYear))
   mmnorm.CompetitionOpenSinceYear

   mmnorm.Promo2 <- (new_store$Promo2 - min(new_store$Promo2))/(max(new_store$Promo2) - min(new_store$Promo2))
   mmnorm.Promo2
   
   mmnorm.Promo2SinceWeek <- (new_store$Promo2SinceWeek - min(new_store$Promo2SinceWeek))/(max(new_store$Promo2SinceWeek) - min(new_store$Promo2SinceWeek))
   mmnorm.Promo2SinceWeek
   
   mmnorm.Promo2SinceYear <- (new_store$Promo2SinceYear - min(new_store$Promo2SinceYear))/(max(new_store$Promo2SinceYear) - min(new_store$Promo2SinceYear))
   mmnorm.Promo2SinceYear

   mmnorm.PromoInterval <- (new_store$PromoInterval - min(new_store$PromoInterval))/(max(new_store$PromoInterval) - min(new_store$PromoInterval))
   mmnorm.PromoInterval
    
   #Corelation Matrix for Train data
   
   cor(train$Customers,train$Sales)
   plot(train$Customers,train$Sales)
   tr_data <- train[,4:length(train)]
   round(cor(tr_data),2)
   
   cor(train$Customers,train$Sales)
   plot(train$Customers,train$Sales) 
   
    #Corelation Matrix for Store data
   
   cor(new_store$CompetitionDistance,new_store$CompetitionOpenSinceMonth)
   plot(new_store$CompetitionDistance,new_store$CompetitionOpenSinceMonth)
   
   cor(new_store$Promo2SinceWeek,new_store$Promo2)
   plot(new_store$Promo2SinceWeek,new_store$Promo2)
   
   
   Str_data <- new_store[,5:length(new_store)]
   round(cor(Str_data),2)
   
   
#############################################################################################
   
   #Welch Two Sample t-test
   
   #data:  store_data1$CompetitionOpenSinceMonth and store_data1$CompetitionDistance
   #t = -23.508, df = 1114, p-value < 2.2e-16
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #  -5838.606 -4939.046
   #sample estimates:
   #  mean of x   mean of y 
   #7.788341 5396.614350 
   
   t.test(new_train$Promo, new_train$Open)
   
   #Welch Two Sample t-test
   
   #data:  new_train$Promo and new_train$Open
   #t = -1023.4, df = 844340, p-value < 2.2e-16
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #  -0.5547034 -0.5525827
   #sample estimates:
   #  mean of x mean of y 
   #0.4463569 1.0000000 
   
   t.test(new_store$Promo2SinceWeek, new_store$Promo2SinceWeek)
   
   #Welch Two Sample t-test
   
   #data:  new_store$Promo2SinceWeek and new_store$Promo2SinceWeek
   #t = 0, df = 2228, p-value = 1
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #  -0.9298995  0.9298995
   #sample estimates:
   #  mean of x mean of y 
   #18.9139   18.9139 
   
   
   t.test(new_store$Promo2, new_store$PromoInterval)
   
   #Welch Two Sample t-test
   
   #data:  new_store$Promo2 and new_store$PromoInterval
   #t = -121.23, df = 2212.4, p-value < 2.2e-16
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #-2.506263 -2.426473
   #sample estimates:
   #  mean of x mean of y 
   #0.5121076 2.9784753
   
   t.test(new_train$Customers, new_train$DayOfWeek)
   
   #Welch Two Sample t-test
   
   #data:  new_train$Customers and new_train$DayOfWeek
   #t = 1738.9, df = 844370, p-value < 2.2e-16
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #  758.3993 760.1108
   #sample estimates:
   # mean of x  mean of y 
   #762.775369   3.520348 
   
   t.test(new_store$CompetitionDistance,new_store$Promo2)
   
   #Welch Two Sample t-test
   
   #data:  new_store$CompetitionDistance and new_store$Promo2
   #t = 23.54, df = 1114, p-value < 2.2e-16
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #  4946.322 5845.882
   #sample estimates:
   #  mean of x    mean of y 
   #5396.6143498    0.5121076 
##########################################################################################
   #Algorithm Execution
  #Extract date, day, month from date and ignore date column from train
   library(dplyr)
   new_train$Date <- as.Date(new_train$Date, "%Y-%m-%d")
   new_train <- mutate(new_train,Day = as.integer(format(Date, "%d")))
   new_train <- mutate(new_train,Month = as.integer(format(Date, "%m")))
   new_train <- mutate(new_train,Year = as.integer(format(Date, "%Y")))
   
   #new_train <- as.numeric(strftime(new_train$Date, format="%W"))
   
   #new_train <- new_train[c(1,4,7:13,2)]
   
   # Merging train and store data set using Store column 
   
   dim(new_train)
   
   dim(test)
   
   Joined_DS <- bind_rows(new_train,test)
   
   length(which(is.na(Joined_DS)))
   
   Joined_DS <- left_join(new_train, new_store, by = "Store")
   
   
  # we will be fitting models for each shop separately in order to predict sales in the test period, data for shops that are absent from the test set serve no purpose. They will not be used to fit models anyway. Therefore, we collect the store IDs from the test set in a separate variable.
    head(test)
   
   teststores <- as.numeric(as.character(unique(test$Store)))
   teststores[1:10]
   
   library(randomForest)
   
   ## randomForest 4.6-12
   
   numericmodel <- function(storeNumber,modelType,...)
     {
     store <- new_train[new_train$Store == storeNumber,]# a store is selected
     store$Promo <- as.numeric(store$Promo)
     store$StateHoliday <- as.numeric(store$StateHoliday)
     store$SchoolHoliday <- as.numeric(store$SchoolHoliday)
     store$DayOfWeek <- as.numeric(store$DayOfWeek)
     shuffledIndices <- sample(nrow(store))
     store$Prediction <- 0
     z <- nrow(store)
     for (i in 1:10) {
       sampleIndex <- floor(1+0.1*(i-1)*z):(0.1*i*z) # 10 % of all data rows is selected
       test_1 <- store[shuffledIndices[sampleIndex],]# it is used as test set
       train_1 <- store[shuffledIndices[-sampleIndex],(2:(ncol(store)-1))]# the rest is used as training set
       modell <- modelType(Sales ~ ., train_1, ...)
       store[shuffledIndices[sampleIndex],]$Prediction <- predict(modell,test_1)
     }
    # sqrt(mean((store$Sales-store$Prediction)^2))
     rmse(store$Sales,store$Prediction) # the result is validated against rmse
  
   }
   
   print(system.time(print(numericmodel(1,randomForest,ntree=200)))) # Algorithm is run against different store numbers
   library(Metrics)
   library(FactoMineR)
   print(system.time(print(numericmodel(262,randomForest,ntree=200))))
   
   ##Linear Regression Model
   
   storelm <- function(storeNumber) {
     store <- new_train[new_train$Store == storeNumber,]  # a store is selected
     shuffledIndices <- sample(nrow(store))  # the data for the store are shuffled
     store$Prediction <- 0
     z <- nrow(store)
     for (i in 1:10) {    # 10-fold cross-validation
       sampleIndex <- floor(1+0.1*(i-1)*z):(0.1*i*z)  # 10 % of all data rows is selected
       test_2 <- store[shuffledIndices[sampleIndex],]  # it is used as test set
       train_2 <- store[shuffledIndices[-sampleIndex],]  # the rest is used as training set
       modell_2 <- lm(Sales ~ Promo + SchoolHoliday + DayOfWeek + as.factor(Year) + as.factor(Month) + as.factor(Day), train_2)  # a linear model is fitted to the training set
       store[shuffledIndices[sampleIndex],]$Prediction <- predict(modell_2,test_2) # predictions are generated for the test set based on the model
     }
      RMSPE(store$Sales,store$Prediction)
     }
   paste("RMSPE: ", storelm(1))
   print(system.time(print(paste("RMSE: ", storelm(1014)))))
   library(Metrics)
   library(MLmetrics)
   library(FactoMineR)
   print(system.time(print(paste("RMSE: ", storelm(262)))))
   # Algorithm is run against different store numbers
  ################################################################################################
   
   # For combined dataset of train and store by storeid
   
   library(randomForest)
   
   ## randomForest 4.6-12
   
   numericmodel <- function(storeNumber,modelType,...)
   {
     store <- Joined_DS[Joined_DS$Store == storeNumber,]# select the store
     store$Promo <- as.numeric(store$Promo)
     store$StateHoliday <- as.numeric(store$StateHoliday)
     store$SchoolHoliday <- as.numeric(store$SchoolHoliday)
     store$DayOfWeek <- as.numeric(store$DayOfWeek)
     store$CompetitionDistance <- as.numeric(store$CompetitionDistance)
     store$CompetitionOpenSinceMonth <- as.numeric(store$CompetitionOpenSinceMonth)
     store$CompetitionOpenSinceYear <- as.numeric(store$CompetitionOpenSinceYear)
     store$Promo2 <- as.numeric(store$Promo2)
     store$Promo2SinceWeek <- as.numeric(store$Promo2SinceWeek)
     store$Promo2SinceYear <- as.numeric(store$Promo2SinceYear)
     #store$PromoInterval <- as.numeric(store$PromoInterval)
     
     shuffledIndices <- sample(nrow(store))# Shuffles the data for the store
     store$Prediction <- 0
     z <- nrow(store)
     for (i in 1:10) { # 10-fold cross-validation
       sampleIndex <- floor(1+0.1*(i-1)*z):(0.1*i*z) # 10 % of all data rows is selected
       test_1 <- store[shuffledIndices[sampleIndex],]# it is used as test set
       train_1 <- store[shuffledIndices[-sampleIndex],(2:(ncol(store)-1))]# the rest is used as training set
       modell <- modelType(Sales ~ ., train_1, ...)
       store[shuffledIndices[sampleIndex],]$Prediction <- predict(modell,test_1)# predictions are generated for the test set based on the model
     }
     
     # sqrt(mean((store$Sales-store$Prediction)^2))
     RMSPE(store$Sales,store$Prediction)
     
   }
   
   print(system.time(print(numericmodel(1,randomForest,ntree=2000))))
   library(Metrics)
   library(FactoMineR)
   print(system.time(print(numericmodel(262,randomForest,ntree=2000))))
   
   ##Linear Regression Model
   
   storelm <- function(storeNumber) {
     store <- Joined_DS[Joined_DS$Store == storeNumber,]  # a store is selected
     shuffledIndices <- sample(nrow(store))  # the data for the store are shuffled
     store$Prediction <- 0
     z <- nrow(store)
     for (i in 1:10) {    # 10-fold cross-validation
       sampleIndex <- floor(1+0.1*(i-1)*z):(0.1*i*z)  # 10 % of all data rows is selected
       test_2 <- store[shuffledIndices[sampleIndex],]  # it is used as test set
       train_2 <- store[shuffledIndices[-sampleIndex],]  # the rest is used as training set
       modell_2 <- lm(Sales ~ Promo + SchoolHoliday + DayOfWeek + as.factor(Year) + as.factor(Month) + as.factor(Day)+as.numeric(CompetitionDistance)+as.numeric(CompetitionOpenSinceMonth)+ as.numeric(CompetitionOpenSinceYear)+ as.numeric(Promo2)+as.numeric(Promo2SinceWeek)+as.numeric(Promo2SinceYear), train_2)  # a linear model is fitted to the training set
       store[shuffledIndices[sampleIndex],]$Prediction <- predict(modell_2,test_2) # predictions are generated for the test set based on the model
     }
     RMSPE(store$Sales,store$Prediction)
   }
   
   paste("RMSPE: ", storelm(1))
   print(system.time(print(paste("RMSE: ", storelm(1)))))
   library(Metrics)
   library(MLmetrics)
   library(FactoMineR)
   print(system.time(print(paste("RMSE: ", storelm(262)))))
   # Algorithm is run against different store numbers
   #######################################################################################
   
  # ARIMA Modelling
   
   library(naniar)
   
   library(rlang)
   #Libraries required to 
   
   library(dplyr)
   
   library(ggplot2)
   
   library(readr) 
   
   library(forecast)
   
   #=====================================================================================================================
   
   #Input data sets - train into data frames
   
   new_train <- read_csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/Dataset/train/train.csv", col_types=list(
     Store = col_integer(),
     DayOfWeek= col_integer(),
     Date = col_date(),
     Sales = col_integer(),
     Customers = col_integer(),
     Open = col_integer(),
     Promo = col_integer(),
     StateHoliday = col_character(),
     SchoolHoliday = col_integer()))
   
   #=====================================================================================================================
   #From the above figure, we can see that the gap occurs at the beginning of July 2014 
   #and runs until the end of the year
   
   typeof(new_train$Date)
   
   #Date = col_date()
   
   new_train$Date <- as.Date(new_train$Date, "%m/%d/%Y")
   
   #train$Date <- strptime(as.character(train$Date), format = "%m/%d/%Y")
   
   df_Date <- new_train %>% group_by(Date) %>% summarise(NumStores=n())
   
   
   ggplot(df_Date, aes(Date,NumStores)) + geom_line()
   
   by_Date_Gap <- df_Date[df_Date$Date %in% 
                            seq(as.Date("2014-06-30"),as.Date("2015-01-01"),by="day"),]
   
   head(by_Date_Gap,3)
   
   tail(by_Date_Gap,3)
   
   #=====================================================================================================================
   
   #stores reporting sales at the beginning of the gap on 2014-7-1 and subtract this from the list of all stores, 
   #finding the stores which have missing data.
   
   all_stores <- unique(new_train$Store)
   stores_reporting <- new_train$Store[new_train$Date == as.Date("2014-7-1")]
   missing_stores <- all_store[!(all_store %in% stores_reporting)]
   missing_stores
   
   #====================================================================================================================
   
   #Picking the first store in the list, store = 13
   
   store13 <- subset(new_train, Store==13)
   
   ggplot(store13, aes(Date,Sales)) +
     geom_line() +
     geom_smooth() + 
     ggtitle("Revenue for Store 13 over time")
   
   #====================================================================================================================
   
   #Creating a new list and checking the difference, if the values are zero then we have constant list of zero with missing data
   
   for (date in seq(as.Date("2014-7-2"),as.Date("2014-12-31"),by="day")) {
     stores_reporting <- new_train$Store[new_train$Date == date]
     missing_on_date <- all_stores[!(all_stores %in% stores_reporting)]
     if (length(setdiff(missing_on_date,missing_stores)) > 0) {
       cat("Date:",date," Difference in missing stores",setdiff(missing_on_date,missing_stores))
     } 
   }
   
   
   all_stores <- unique(new_train$Store)
   
   stores_reporting <- new_train$Store[new_train$Date == as.Date("2013-1-1")]
   
   additional_missing_store <- all_stores[!(all_stores %in% stores_reporting)]
   
   additional_missing_store
   
   #====================================================================================================================
   
   #Binding the missing data to the initial training set
   
   date <- as.Date("2013-1-1")
   day_of_week <- unique(new_train$DayOfWeek[new_train$Date == date])
   sales <- as.numeric(names(which.max(table(new_train$Sales[train$Date == date]))))
   customers <- as.numeric(names(which.max(table(new_train$Customers[new_train$Date == date]))))
   open <- as.numeric(names(which.max(table(new_train$Open[new_train$Date == date]))))
   promo <- as.numeric(names(which.max(table(new_train$Promo[new_train$Date == date]))))
   state_holiday <- names(which.max(table(new_train$StateHoliday[new_train$Date == date])))
   school_holiday <- as.numeric(names(which.max(table(new_train$SchoolHoliday[new_train$Date == date]))))
   
   missing_row <- data.frame(Store = additional_missing_store,
                             DayOfWeek = day_of_week,
                             Date = date,
                             Sales = sales,
                             Customers = customers,
                             Open = open,
                             Promo = promo,
                             StateHoliday = state_holiday,
                             SchoolHoliday = school_holiday)
   
   new_train <- rbind(train,missing_row)
   
   #====================================================================================================================
   
   #Imputing Missing Values
   #adding an extra column with the log of the sales values and impute values for stores in our missing data set
   
   new_train$logSales <- log(new_train$Sales+1)
   
   gap <- seq(as.Date("2014-7-1"),as.Date("2014-12-31"),by="day")
   n_missing <- length(gap)*length(missing_stores)
   missing_df <- data.frame(Store = integer(n_missing),
                            DayOfWeek = integer(n_missing),
                            Date = rep(gap,length(missing_stores)),
                            Sales = integer(n_missing),
                            Customers = integer(n_missing),
                            Open = integer(n_missing),
                            Promo = integer(n_missing),
                            StateHoliday = character(n_missing),
                            SchoolHoliday = integer(n_missing),
                            logSales = numeric(n_missing),
                            stringsAsFactors=FALSE)
   
   #====================================================================================================================
   
   #the majority of stores are open we will assume that this store is open, 
   #if the majority of stores are having a promo we will assume this store is having a promo, etc.
   
   for (date in gap) {
     missing_df$Store[missing_df$Date == date] <- missing_stores
     
     day_of_week <- unique(new_train$DayOfWeek[new_train$Date == date])
     missing_df$DayOfWeek[missing_df$Date == date] <- rep(day_of_week, length(missing_stores))
     
     missing_df$Sales[missing_df$Date == date] <- rep(NA, length(missing_stores))
     
     missing_df$Customers[missing_df$Date == date] <- rep(NA, length(missing_stores))
     
     open <- as.numeric(names(which.max(table(new_train$Open[new_train$Date == date]))))
     missing_df$Open[missing_df$Date == date] <- rep(open, length(missing_stores))
     
     promo <- as.numeric(names(which.max(table(new_train$Promo[new_train$Date == date]))))
     missing_df$Promo[missing_df$Date == date] <- rep(promo, length(missing_stores))
     
     state_holiday <- names(which.max(table(new_train$StateHoliday[new_train$Date == date])))
     missing_df$StateHoliday[missing_df$Date == date] <- rep(state_holiday, length(missing_stores))
     
     school_holiday <- as.numeric(names(which.max(table(new_train$SchoolHoliday[new_train$Date == date]))))
     missing_df$SchoolHoliday[missing_df$Date == date] <- rep(school_holiday, length(missing_stores))
     
     missing_df$logSales[missing_df$Date == date] <- rep(NA, length(missing_stores))
     
   }
   
   
   head(missing_df)
   
   #====================================================================================================================
   
   #binding this to existing training set and re-order everything according to date
   
   train_filled_gap <- rbind(new_train,missing_df)
   train_filled_gap <- train_filled_gap[order(train_filled_gap$Date),]
   
   #====================================================================================================================
   
   #imputing values for the missing time period,median of existing Sales 
   #and Customer data but contrained to the day of the week
   
   train_filled_gap <- train_filled_gap %>% 
     group_by(Store, DayOfWeek, Open, Promo) %>%
     mutate(Sales = as.integer(ifelse(is.na(Sales), 
                                      ifelse(Open == 0, 
                                             0,
                                             median(Sales, na.rm=T)), 
                                      Sales))) %>%
     mutate(Customers = as.integer(ifelse(is.na(Customers),
                                          ifelse(Open == 0, 
                                                 0,
                                                 median(Customers, na.rm=T)),
                                          Customers))) %>%
     mutate(logSales = ifelse(is.na(logSales),
                              ifelse(Open == 0,
                                     0,
                                     mean(logSales, na.rm=T)), 
                              logSales))
   
   
   #====================================================================================================================
   
   #checking if there are any remaining NA values in the Sales, Customers and logSales columns
   
   anything_missed <- subset(train_filled_gap, is.na(Sales) | is.na(Customers) | is.na(logSales))
   anything_missed
   
   #====================================================================================================================
   
   #we now take a look at the time series for our example store, Store 13, we should have a continuous time series
   #Revenue of store13 over time
   
   store13 <- subset(train_filled_gap, Store==13)
   ggplot(store13, aes(Date,Sales)) +
     geom_line() +
     geom_smooth() + 
     ggtitle("Revenue for Store 13 over time")
   
   #====================================================================================================================
   
   #Log of revenue for store13, the logarithm reducing the spikes it is a little less obvious
   
   ggplot(store13, aes(Date,logSales)) +
     geom_line() +
     geom_smooth() + 
     ggtitle("Log of Revenue for Store 13")
   
   #====================================================================================================================
   
   #creating a new csv file
   
   write_csv(train_filled_gap,"C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/train_filled_gap.csv")
   
   #====================================================================================================================
   
   #Forecasting
   
   #forecast sales for all stores over the entire time series
   #Reading a file which was saved
   
   test <- read_csv("C:/Rizwan/UNCC/Course Study/semester 2/KDD/project/train_filled_gap.csv", col_types=list(
     Id = col_integer(),
     Store = col_integer(),
     DayOfWeek= col_integer(),
     Date = col_date(),
     Open = col_integer(),
     Promo = col_integer(),
     StateHoliday = col_character(),
     SchoolHoliday = col_integer()))
   
   store13_test <- subset(test, Store == 13)
   
   #====================================================================================================================
   
   #we are using ARIMA model to forecast the data
   #Forecasting the future sales of the store 13 from all the sales data of the store 13
   #This model helps to forecast the sales information of the Rossman stores
   
   
   holiday <- 1 - store13$Open
   holidayf <- 1 - store13_test$Open
   promo <- store13$Promo
   promof <- store13_test$Promo
   test_period <- max(store13_test$Date) - min(store13_test$Date) + 1
   
   y <- ts(store13$Sales, frequency=7)
   z <- fourier(ts(store13$Sales, frequency=365.25), K=5)
   zf <- fourierf(ts(store13$Sales, frequency=365.25), K=5, h=test_period)
   fit <- auto.arima(y, xreg=cbind(z,holiday,promo), seasonal=FALSE)
   fc <- forecast(fit, xreg=cbind(zf,holidayf,promof), h=test_period)
   plot(fc)
   
   #====================================================================================================================
   
   #The previous forecast shows negative revenue on days that the store should be 
   #closed but this can be easily corrected by multiplying with the vector of open days
   
   store13_test$Sales <- fc$mean*store13_test$Open
   store13_test$Sales
   
   #====================================================================================================================
   
   #Now the forecasting is done using the log value of sales and use exponential to convert back 
   
   y <- ts(store13$logSales, frequency=7)
   z <- fourier(ts(store13$logSales, frequency=365.25), K=5)
   zf <- fourierf(ts(store13$logSales, frequency=365.25), K=5, h=test_period)
   fit <- auto.arima(y, xreg=cbind(z,holiday,promo), seasonal=FALSE)
   fc <- forecast(fit, xreg=cbind(zf,holidayf,promof), h=test_period)
   plot(fc)
   
   #Thus the forecasted values are shown in the graph
   
   #====================================================================================================================
  
   
   
