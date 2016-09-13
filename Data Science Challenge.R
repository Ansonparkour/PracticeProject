# Data Science Challenge
# 9/11/2016


#---------------------------Question 1------------------------------

library(ggplot2)
library(dplyr)

getwd()
setwd('./Desktop/')

#load dataset
fileUrl <- "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"
download.file(fileUrl, destfile = "./green.csv", method = "curl")

#read csv file
MyGreen <- read.csv("./green.csv")
data = MyGreen

dim(MyGreen)
#[1] 1494926   21, that is 1494926 rows and 21 columns

#---------------------------Question 2------------------------------

#get overall sence of how trip distance distributed
hist(MyGreen$Trip_distance)

#zoom in a little bit 
ggplot(data=MyGreen, aes(MyGreen$Trip_distance)) + 
  geom_histogram(breaks=seq(0, 50, by =2), 
                 col="red", 
                 aes(fill=..count..))

#zoom in more 
ggplot(data=MyGreen, aes(MyGreen$Trip_distance)) + 
  geom_histogram(breaks=seq(5, 30, by =2), 
                 col="red", 
                 aes(fill=..count..))


str(MyGreen)

#---------------------------Question 3------------------------------


#firstly, extract the hours of a day, and store in a new column called hour
MyGreen$hour = substr(MyGreen$lpep_pickup_datetime, 11, 13)

MyGreen %>% 
     group_by(hour) %>%
          summarise(mean_hour = mean(Trip_distance), median_hour = median(Trip_distance))


#airport near NYC area 
JFK = c(40.639801, -73.7789002)
DowntownManhattanHeliport = c(40.7016, -74.0091)
LaGuardiaAirport = c(40.7769, -73.8740)
NewarkLiberty = c(40.6895, -74.1745)

airports = rbind(JFK, DowntownManhattanHeliport, LaGuardiaAirport, NewarkLiberty)
colnames(airports) <- c("latitude","longitude")
airports = as.data.frame(airports)

#transactions dropoff at airport near NYC area 
for (i in 1:4){
     print(table(airports[i,][1,2]*(1+0.0001) <= MyGreen$Dropoff_longitude & MyGreen$Dropoff_longitude <= airports[i,][1,2]*(1-0.0001) & 
                airports[i,][1,1]*(1-0.0001) <= MyGreen$Dropoff_latitude & MyGreen$Dropoff_latitude <= airports[i,][1,1]*(1+0.0001))[2])
}

#JFK 
#1762 
#DowntownManhattanHeliport 
#2725 
#LaGuardiaAirport 
#11233 
#NewarkLiberty 
#266 

#transactions pickup at airport near NYC area 
for (i in 1:4){
     print(table(airports[i,][1,2]*(1+0.0001) <= MyGreen$Pickup_longitude & MyGreen$Pickup_longitude <= airports[i,][1,2]*(1-0.0001) & 
                      airports[i,][1,1]*(1-0.0001) <= MyGreen$Pickup_latitude & MyGreen$Pickup_latitude <= airports[i,][1,1]*(1+0.0001))[2])
}

#JFK 
#24 
#DowntownManhattanHeliport 
#NA 
#LaGuardiaAirport 
#124 
#NewarkLiberty 
#13 

#---------------------------Question 4------------------------------

MyGreen$TipPercent = round(MyGreen$Tip_amount / MyGreen$Fare_amount, 4)


#just keep the variables which will be using into the model 
MyData = cbind(MyGreen$Passenger_count, MyGreen$Trip_distance, MyGreen$Fare_amount,
                    MyGreen$Tolls_amount, MyGreen$improvement_surcharge, MyGreen$Extra, MyGreen$TipPercent)
colnames(MyData) <- c("Passenger_count","Trip_distance","Fare_amount","Tolls_amount",
                      "improvement_surcharge","Extra","TipPercent")
MyData = as.data.frame(MyData) #1494926

#change infinite value to NA
is.na(MyData) <- sapply(MyData, is.infinite)

#remove all NA and NaN
MyData = MyData[complete.cases(MyData), ] #1490458

#set up training dataset(70%) and validation dataset(30%)
n = nrow(MyData) 
index = sample(n, round(0.7*n))
training = MyData[index,]   
validation  = MyData[-index,]  
validation$TipPercent = NULL

#fit liner regression model
model <- lm(TipPercent ~ . , data=training)

#Coefficients:
#(Intercept)        Passenger_count          Trip_distance  
#0.0643117             -0.0007772              0.0026437  
#Fare_amount           Tolls_amount  improvement_surcharge  
#-0.0008187              0.0073848              0.1107919  
#Extra  
#0.0118700  

#using validation dataset fit the model
predict(model, validation, se.fit = TRUE)

#$residual.scale
#[1] 0.434723



#---------------------------Question 5------------------------------

#calculate the time spent
diff_in_hours = difftime(MyGreen$Lpep_dropoff_datetime, MyGreen$lpep_pickup_datetime, units = "hours")
time = as.double(diff_in_hours)

#calculate the speed
MyGreen$average_speed = round(MyGreen$Trip_distance / time, 4)


MyGreen$day = substr(MyGreen$lpep_pickup_datetime, 9, 10)

MyGreen$Week = as.numeric(MyGreen$day)

for (i in 1:6){
     MyGreen$Week[MyGreen$Week == i] = 'week1'
}

for (i in 7:13){
     MyGreen$Week[MyGreen$Week == i] = 'week2'
}

for (i in 14:20){
     MyGreen$Week[MyGreen$Week == i] = 'week3'
}

for (i in 21:27){
     MyGreen$Week[MyGreen$Week == i] = 'week4'
}

for (i in 28:30){
     MyGreen$Week[MyGreen$Week == i] = 'week5'
}

MyGreen$Week = as.factor(MyGreen$Week)


#change infinite value to 0
mydata$average_speed[which(mydata$average_speed == Inf)] = 0


fit <- aov(average_speed ~ Week, data=mydata)
summary(fit)

#                  Df    Sum Sq Mean Sq F value Pr(>F)  
#Week              4 4.165e+05  104114   2.234 0.0627 
#Residuals   1493984 6.963e+10   46606       

#conclusion: no significant evidence to show avg speed for every week is same. 


fit2 <- lm(average_speed ~ day, data=mydata)
summary(fit2)

#F-statistic: 2.004 on 29 and 1493959 DF,  p-value: 0.00105



