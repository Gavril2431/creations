                  ##** ANALYSIS OF PREFERRED ENGINES **##
                  #_____________________________________#

  ## these are the packages needed for the project ##
 
install.packages("ggplot2")
install.packages("magrittr")
install.packages("grid")
install.packages("gridExtra")
install.packages("plyr")
install.packages("dplyr")
install.packages("readr")
  library(ggplot2)
  library(magrittr)
  library(grid)
  library(gridExtra)
  library(plyr)
  library(dplyr)
  library(readr)

  ## importing the data set and examining it ##
cardata<- read.csv('D:/Projects/R_Project_Data/cardata_i.csv', header = TRUE)
  View(cardata)
  head(cardata)
  summary(cardata)

  ## pre-processing the data ##
  #---------------------------#
str(cardata)
cardata[complete.cases(cardata),]
cardata$Cylinders = cardata$Cylinders %>%
  factor(labels = sort(unique(cardata$Cylinders)))
table(cardata$Cylinders)

  ## analysis of the following data set ##
  #--------------------------------------#
qplot(cardata$Cylinders, xlab = 'No. Of Cylinders',ylab = 'Count', 
      main='Histogram of NO. of Cylinders') 
# more of V6 engine cars were produced 

qplot(cardata$Type, xlab = 'Type of cars', ylab = 'Count',
      main = 'Histogram of type of cars')
# most companies made sedans

qplot(cardata$Origin, xlab = 'origin of the cars', ylab = 'count',
      main = 'histogram of origin places of the cars')
# most car were produced in Asia

ggplot(data = cardata, aes(x = Cylinders, fill =  ï..Make )) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars of each Brand by their Number of Cylinders')
# this shows which brand mostly used how many cyl in their cars

ggplot(data = cardata, aes(x = Cylinders, fill =  Type )) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars of each car type by their Number of Cylinders')
# here e can see which type of cars consisted of how many cyl

ggplot(data = cardata, aes(x = DriveTrain, fill =Type)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars of each car type by their Number of Cylinders')
# this shows the type of cars that had what type of drivetrains

ggplot(data = cardata,
       aes(x =Weight , y = MPG_City)) +
       geom_point() +
       geom_smooth(method='lm') +
       xlab('MPG') +
       ylab('Weight')+
       ggtitle('effect of weight on the MPG in city of cars')
# we can see here that the more heavier the car 
# the lesser the MPG it provides

ggplot(data = cardata,
       aes(x = EngineSize, y =MPG_City )) +
        geom_point() +
        geom_smooth(method='lm')+
        xlab('engine size') +
        ylab('MPG in city')+
        ggtitle('Effect of engine size on the MPG in city of cars')
# we can see that the MPG in the city is mostly In between 20 and 40
# when the engine is of 2 to 4 liters

#here is the prediction of how the sales will go on of
#cars
my_data<-read.csv("D:/Projects/R_Project_Data/Car_sales (1).csv")

#process the data
my_data$Resale_value<- ifelse(is.na(my_data$Resale_value),
                              ave(my_data$Resale_value,FUN=function(x) mean(x,na.rm= TRUE)),
                              my_data$Resale_value)

my_data$Sales_in_thousands <- ifelse(is.na(my_data$Sales_in_thousands),
                                     ave(my_data$Sales_in_thousands,FUN=function(x) mean(x,na.rm= TRUE)),
                                     my_data$Sales_in_thousands)

my_data$Price_in_thousands <- ifelse(is.na(my_data$Price_in_thousands),
                                     ave(my_data$Price_in_thousands,FUN=function(x) mean(x,na.rm= TRUE)),
                                     my_data$Price_in_thousands)

my_data$Engine_size <- ifelse(is.na(my_data$Engine_size),
                              ave(my_data$Engine_size,FUN=function(x) mean(x,na.rm= TRUE)),
                              my_data$Engine_size)

my_data$Horsepower <- ifelse(is.na(my_data$Horsepower),
                             ave(my_data$Horsepower,FUN=function(x) mean(x,na.rm= TRUE)),
                             my_data$Horsepower)

my_data$Wheelbase <- ifelse(is.na(my_data$Wheelbase),
                            ave(my_data$Wheelbase,FUN=function(x) mean(x,na.rm= TRUE)),
                            my_data$Wheelbase)

my_data$Width <- ifelse(is.na(my_data$Width),
                        ave(my_data$Width,FUN=function(x) mean(x,na.rm= TRUE)),
                        my_data$Width)

my_data$Power_perf_factor <- ifelse(is.na(my_data$Power_perf_factor),
                                    ave(my_data$Power_perf_factor,FUN=function(x) mean(x,na.rm= TRUE)),
                                    my_data$Power_perf_factor)

my_data$Length <- ifelse(is.na(my_data$Length),
                         ave(my_data$Length,FUN=function(x) mean(x,na.rm= TRUE)),
                         my_data$Length)

my_data$Curb_weight <- ifelse(is.na(my_data$Curb_weight),
                              ave(my_data$Curb_weight,FUN=function(x) mean(x,na.rm= TRUE)),
                              my_data$Curb_weight)

my_data$Fuel_capacity <- ifelse(is.na(my_data$Fuel_capacity),
                                ave(my_data$Fuel_capacity,FUN=function(x) mean(x,na.rm= TRUE)),
                                my_data$Fuel_capacity)

my_data$Fuel_efficiency <- ifelse(is.na(my_data$Fuel_efficiency),
                                  ave(my_data$Fuel_efficiency,FUN=function(x) mean(x,na.rm= TRUE)),
                                  my_data$Fuel_efficiency)

#Encoding categorical data
my_data$Vehicle_type=factor(my_data$Vehicle_type,
                            levels=c('Passenger','Car'),
                            labels=c(1,2))

#Splitting the data into training and testing set
set.seed(123)
split=caTools::sample.split(my_data$Sales_in_thousands,SplitRatio = 0.8)
training_set=subset(my_data,split==TRUE)
test_set=subset(my_data,split==FALSE)

#Fitting regression model to the training set
regressor=lm(formula=Sales_in_thousands ~.,
              data=training_set)

summary(regressor)
#From the summary we see that Wheelbase is highly statistically significant on dependent variable

#Predicting the test set
y_pred=predict(regressor,newdata=test_set)


test_set
y_pred







  










      
