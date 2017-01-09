# R codel for Data Science Challenge: Trips.

#Question 1
#download and load the trip data for September 2015 to local des
URL <- "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"
destfile <- "C:/Users/Cathy/Downloads/green_tripdata_2015-09.csv"
download.file(URL, destfile)
#load the data store as dataframe "green_tripdata"
green_tripdata <- read.csv(file=destfile, header=TRUE)

# report number of rows and columns of data 
N_obs<-nrow(green_tripdata)
names(N_obs) <- "number of rows"
N_col<-ncol(green_tripdata)
names(N_col) <- "number of columns"

#Question 2
# Quick look at the statistical summary of trip distance
y=green_tripdata$Trip_distance
cat("statistical summary of trip distance")
summary(y)

# Use package ggplot2 to implement visualization
library(ggplot2)
ggplot(data=green_tripdata, aes(green_tripdata$Trip_distance)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=unique(quantile(green_tripdata$Trip_distance, 
                                        probs = seq(0, 1, by=0.05)))) + 
  geom_density(col="blue") + 
  xlim(c(0, 10))+
  labs(title="Histogram for Trip distance") +
  labs(x="distance", y="density")

# use fitdistrplus package to check the goodness-of-fit of log-normal distribution
library(fitdistrplus)
fit.lnorm <- fitdist(y[y>0 & y<100], "lnorm", method = c("mge"))
plot(fit.lnorm)

#Question 3
# change the type of datetime to POSIXct objects
library(lubridate)
green_tripdata$lpep_pickup_datetime <- ymd_hms(green_tripdata$lpep_pickup_datetime)
green_tripdata$Lpep_dropoff_datetime <- ymd_hms(green_tripdata$Lpep_dropoff_datetime)
# add column Travel_hour to data
green_tripdata$Travel_hour <- as.factor(hour(green_tripdata$lpep_pickup_datetime))
# report mean and median trip distance grouped by hour of day
library(dplyr)
m_by_hour<-green_tripdata  %>% group_by(.,Travel_hour) %>% summarise(.,avg_distance =mean(Trip_distance),medi_distance=median(Trip_distance)) 
as.data.frame(m_by_hour)

#charistiristics of trips originate or terminate at JFK or Newark airport
green_tripdata_ap<-green_tripdata %>% filter(RateCodeID  %in% c(2,3)) %>%  summarise(avg_fare=mean(Fare_amount),avg_tip=mean(Tip_amount),avg_distance =mean(Trip_distance),count=n())
cat("characteristics of trips originate or terminate at JFK or Newark airport")
as.data.frame(green_tripdata_ap)

green_tripdata$Travel_hour <- as.factor(hour(green_tripdata$lpep_pickup_datetime))
ap_hour<-subset(green_tripdata,RateCodeID  %in% c(2,3))$Travel_hour
cat("count of trips in different hour in a day")
summary(ap_hour)

#Question 4
# Data cleaning
# correct the type of data
green_tripdata$VendorID <- as.factor(green_tripdata$VendorID)
green_tripdata$Trip_type <-  as.factor(green_tripdata$Trip_type)
green_tripdata$RateCodeID <- as.factor( green_tripdata$RateCodeID)
green_tripdata$Payment_type <- as.factor(green_tripdata$Payment_type )
green_tripdata$Trip_type <-  as.factor(green_tripdata$Trip_type)

# features that can affect the tip percentage
green_tripdata$duration <- as.numeric(difftime(green_tripdata$Lpep_dropoff_datetime, green_tripdata$lpep_pickup_datetime , units="mins")) 
green_tripdata$Travel_hour <- as.factor(hour(green_tripdata$lpep_pickup_datetime))
green_tripdata$Travel_weekdays <- as.factor(weekdays(green_tripdata$lpep_pickup_datetime))
green_tripdata$Travel_speed  <- green_tripdata$Trip_distance / green_tripdata$duration *60
# derive the tip percentage
green_tripdata$Tip_percentage <- green_tripdata$Tip_amount / green_tripdata$Total_amount * 100

# filter the data for training and testing
green_tripdata_pre <-  subset(green_tripdata,0<Fare_amount & Fare_amount<100 & 0<Total_amount 
                              & Total_amount<100 & 0<Trip_distance & Trip_distance<100
                              & 0<duration & duration<120 & Travel_speed<80 & Payment_type %in% c(1,2) 
                              & RateCodeID  %in% c(1,2,3,4,5,6))

#split data into test/train set
N <- nrow(green_tripdata_pre)
train <- sample(N, size=floor(0.8*N), replace=FALSE)

#build the multi regression model on selected predictors
Multi_reg<-lm(Tip_percentage~ Travel_hour+Travel_weekdays+Travel_speed+Trip_distance+duration+Trip_type+Payment_type
              +RateCodeID+Passenger_count ,data=green_tripdata_pre,subset=train)
summary(Multi_reg)

#use rpart package for regression tree model on selected predictors
library(rpart)
Reg_tree <- rpart(Tip_percentage~ Travel_hour+Travel_weekdays+Travel_speed+Trip_distance+duration+Trip_type
                  +Payment_type+RateCodeID+ Passenger_count,
                  data=green_tripdata_pre,subset=train, method="anova")
summary(Reg_tree)

#use randomforest package for random forest model on selected predictors
library(randomForest)
set.seed(1234)
f_train <- sample(N, size=floor(0.01*N), replace=FALSE)
rf <- randomForest(Tip_percentage~ Travel_hour+Travel_weekdays+Travel_speed+Trip_distance+duration+Trip_type
                   +Payment_type+RateCodeID+Passenger_count,
                   data=green_tripdata_pre, subset=f_train, ntree=50)
importance(rf)
print(rf)
plot(rf) 

#predict on test set
pred_lm <- predict(Multi_reg, newdata=green_tripdata_pre[-train,])
pred_tree <- predict(Reg_tree, newdata=green_tripdata_pre[-train,])
pred_rf <- predict(rf, newdata=green_tripdata_pre[-train,])

#calc mean squared error
mse <- numeric(3)
mse[1] <- mean((pred_lm - green_tripdata_pre[-train,"Tip_percentage"])^2)
mse[2] <- mean((pred_tree - green_tripdata_pre[-train,"Tip_percentage"])^2)
mse[3] <- mean((pred_rf - green_tripdata_pre[-train,"Tip_percentage"])^2)
names(mse) <- c("LinReg","Tree", "RForrest")
mse

#Question 5
#function find distance between two points with lat and long
dist <- function (long1, lat1, long2, lat2)
{
  #convert Lat/Lon to to radian 
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378 # radius of earth
  d <- R * c
  return(d)
}

# function find k nearest trips given point P 
# input longitude of point P p1,latitude of point P p2, k
# output k trip records
getKNearest <- function(p1,p2, k){
  #compute distance between P and all points in the Data set
  dist_all <- apply(green_tripdata[,c('Pickup_longitude', 'Pickup_latitude')], 1,
                    function(x) {dist(p1,p2,x[1],x[2])} )
  #sort and return the k smallest
  Sorted_idx <- sort.list(dist_all, method = "quick", na.last = NA)
  #index of first kth nearest trip
  k_idx<- Sorted_idx[1:k]
  green_tripdata[k_idx,]  
}

###Test case###
# getKNearest(-73.7781, 40.6413, 5)

# function find k nearest trips condering both time and distance for pickup given point P 
# input longitude of point P p1,latitude of point P p2, current time cur_t, acceptable mins
# before taxi request tAhead
# output k trip records

getKNearestTime <- function(p1, p2, curr_time, k, tAhead)
  {
library(lubridate)
#convert time to POSXct objects
  curr_time <- as.POSIXct(curr_time,format = '%Y-%m-%d %H:%M:%S')
#find acceptable start time of trips
start_time = curr_time - minutes(tAhead)
# get the data before the time, str_time
tripdata_selected = subset( green_tripdata, 
                  as.numeric(as.POSIXct(lpep_pickup_datetime))>=as.numeric(start_time) 
                   & as.numeric(as.POSIXct(lpep_pickup_datetime) <= as.numeric(curr_time)))
# compute the distance 
dist_selected <- apply(tripdata_selected[,c('Pickup_longitude', 'Pickup_latitude')], 1,
                  function(x) {dist(p1,p2,x[1],x[2])} )
#sort and return the k smallest
Sorted_idx <- sort.list(dist_selected, method = "quick", na.last = NA)
#index of first kth nearest trip
k_idx<- Sorted_idx[1:k]
tripdata_selected[k_idx,]  
}

###Test###
#getKNearestTime(-73.7781, 40.6413 ,"2015-09-29 17:58:27", 5, 10)
#write.csv(getKNearestTime(-73.7781, 40.6413 ,"2015-09-29 17:58:27", 5, 10), file = "nearest trips considering pickup time.csv")

