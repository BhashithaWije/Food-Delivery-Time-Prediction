#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("xlsx")
#install.packages("REdaS")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("FactoMineR")
install.packages("tidyverse")
install.packages("clustMixType")
install.packages("fpc")
library(fpc)
library(clustMixType)
library(tidyverse)
library(FactoMineR)
library(lubridate)
library(stringr)
library(REdaS)
library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(xlsx)

Food = read.csv("train.csv")

Food = Food %>% select(-c(ID,Delivery_person_ID,Order_Date))

Food$Weatherconditions = factor(Food$Weatherconditions)
Food$Road_traffic_density = factor(Food$Road_traffic_density)
Food$Vehicle_condition = factor(Food$Vehicle_condition)
Food$Type_of_order = factor(Food$Type_of_order)
Food$Type_of_vehicle = factor(Food$Type_of_vehicle)
Food$multiple_deliveries = factor(Food$multiple_deliveries)
Food$Festival = factor(Food$Festival)
Food$City = factor(Food$City)

str(Food)
summary(Food)
sum(is.na(Food$Delivery_person_Age) & is.na(Food$Delivery_person_Ratings))
#1854 NA values were in Delivery Person Age and Delivert Person Ratings
#removing them in the main dataset

Food = Food[!(is.na(Food$Delivery_person_Age) & is.na(Food$Delivery_person_Ratings)),]
summary(Food)
# still 54 observations were missing in the Delivery Person Ratings
#removing those

Food = Food[!(is.na(Food$Delivery_person_Ratings)),]
summary(Food)
#totally 1854 + 54 missing values were there in these two variables so it was 4% of the total data
#therefore we can removed them

Food = Food %>% select(-c(Time_Orderd,Time_Order_picked))

summary(Food)

#Formatting the response varibale and weather condition
Food$Time_taken.min. = as.numeric(str_extract(Food$Time_taken.min., "\\S+$"))
Food$Weatherconditions = as.factor(str_extract(Food$Weatherconditions, "\\S+$"))

summary(Food)
 
#calculating the distance variable
lon1 = Food$Restaurant_longitude
lat1 = Food$Restaurant_latitude
lon2 = Food$Delivery_location_longitude
lat2 = Food$Delivery_location_latitude

phi_1 = deg2rad(lat1)
phi_2 = deg2rad(lat2)

delta_phi = deg2rad(lat2 - lat1)
delta_lambda =deg2rad(lon2 - lon1)

a = sin(delta_phi / 2.0)^2 + cos(phi_1)*cos(phi_2)*sin(delta_lambda / 2.0)^2

c = 2*atan2(sqrt(a),sqrt(1 - a))

R = 6371000  # radius of Earth in meters
meters = R * c
km = meters / 1000.0 
km = round(km, 3)
km

Food$Distance = km #storing the distances again in the dataframe
#boxplot of distances 
ggplot(Food, aes(y = Distance)) +
  geom_boxplot() +
  labs(title = "Boxplot of Distance",
       y = "Distance") +
  theme_minimal()

summary(Food$Distance)
#75% of the distances are less than 13.68km which indicates there are few outliers in data

#Removing the outliers in the distance variables using IQR range
Q3 = summary(Food$Distance)[[5]]
Q1 = summary(Food$Distance)[[2]]
IQR = Q3-Q1
Upper = Q3 + 1.5*(IQR)
Lower = Q1 - 1.5*IQR

#updating Food with removed individuals
Food = Food[(Food$Distance>Lower)&(Food$Distance<Upper),]
#183 orders were identified as outliers considering the distance varibale and was removed from the dataset

summary(Food)


#removing longtitudes and laltitudes
Food = Food %>% select(-c(Restaurant_latitude,Restaurant_longitude,Delivery_location_latitude,Delivery_location_longitude))


summary(Food)

#now should randomly select training and testing sets.
set.seed(100)
Food$id = 1:nrow(Food)
 
train = Food %>% dplyr::sample_frac(0.8)
test = dplyr::anti_join(Food, train, by = 'id')
 
train = train %>% select(-id)
test = test %>% select(-id)
 
#write.xlsx(train, "train_processed.xlsx")
#write.xlsx(test, "test_processed.xlsx")

Food = train
###### UNIVARIATE ANALYSIS ###############
# Create a histogram on "time taken" varibale
ggplot(Food, aes(x =Time_taken.min.)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(x = "Time Taken", y = "Frequency", title = "Histogram of Time Taken on a delivery")

#histogram of delivery person age
ggplot(Food, aes(x = Delivery_person_Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(x = "Delivery Person Age", y = "Frequency", title = "Histogram of Delivery Persons Age")

#histogram of delivery person rating
ggplot(Food, aes(x = Delivery_person_Ratings)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Delivery Person Rating", y = "Frequency", title = "Histogram of Delivery Persons Rating")

#Histogram of distance b/w resturant and delivery point
ggplot(Food, aes(x = Distance)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(x = "Delivery distance", y = "Frequency", title = "Histogram of Delivery distance")

#Creating Box plots for all the numeric varibales
# Create the box plot on "time taken" varibale
boxplot(Food$Time_taken.min.,
        main = "Box plots of Time taken(min)",
        xlab = " ",
        ylab = "Time taken(min)",
        col = "skyblue",  # Custom colors for each box
        border = "black"  # Border color
        #notch = TRUE,  # Add notches to the box plots
        #notchwidth = 0.5,  # Width of the notches
        #horizontal = FALSE,  # Vertical box plots
        #las = 2  # Rotate x-axis labels vertically
)

# Create the box plot on "delivery person age" varibale
boxplot(Food$Delivery_person_Age,
        main = "Box plots of Delivery Person Age",
        xlab = " ",
        ylab = "Age",
        col = "skyblue",  # Custom colors for each box
        border = "black"  # Border color
        #notch = TRUE,  # Add notches to the box plots
        #notchwidth = 0.5,  # Width of the notches
        #horizontal = FALSE,  # Vertical box plots
        #las = 2  # Rotate x-axis labels vertically
)

# Create the box plot on "delivery person rating" varibale
boxplot(Food$Delivery_person_Ratings,
        main = "Box plots of Delivery Person Rating",
        xlab = " ",
        ylab = "Rating",
        col = "skyblue",  # Custom colors for each box
        border = "black"  # Border color
        #notch = TRUE,  # Add notches to the box plots
        #notchwidth = 0.5,  # Width of the notches
        #horizontal = FALSE,  # Vertical box plots
        #las = 2  # Rotate x-axis labels vertically
)

# Create the box plot on "distance" varibale
boxplot(Food$Distance,
        main = "Box plot of Distance",
        xlab = " ",
        ylab = "Distance",
        col = "skyblue",  # Custom colors for each box
        border = "black"  # Border color
        #notch = TRUE,  # Add notches to the box plots
        #notchwidth = 0.5,  # Width of the notches
        #horizontal = FALSE,  # Vertical box plots
        #las = 2  # Rotate x-axis labels vertically
)

#bar plot of Weather Conditions 
ggplot(Food, aes(x = Weatherconditions)) +
  geom_bar(fill = "skyblue") +  # Customize fill color
  labs(title = "Bar Plot of Type of order",
       x = "Type of order",
       y = "Count") +  # Customize axis labels
  theme_minimal()


############# BIVARIATE ANALYSIS ###############
#Time Taken Vs Weatherconditions
#ggplot(data = Food, aes(x = Weatherconditions, y = Time_taken.min., fill = Weatherconditions)) +
#  geom_boxplot() +
#  labs(title = "Boxplot Weatherconditions vs Time taken", x = "Weather Condition", y = "Time taken") +
#  scale_fill_brewer(palette = "Blues") +
#  guides(fill = guide_legend(title = "Weather condition"))

#ggplot(data = Food, aes(x = Weatherconditions, y = Time_taken.min., fill = Weatherconditions)) +
#  geom_boxplot() +
#  labs(title = "Boxplot Weatherconditions vs Time taken", x = "Weather Condition", y = "Time taken") +
#  scale_fill_brewer(palette = "Blues") +
#  scale_x_discrete(labels = c("Cloudy" = "Cloudy", "Fog" = "Fog", "NaN" = "Missing",
#                              "Standstorms" = "Standstorms", "Stormy" = "Stormy", "Sunny" = "Sunny", "Windy" = "Windy")) +
#  guides(fill = guide_legend(title = "Weather condition"))

ggplot(data = Food, aes(x = Weatherconditions, y = Time_taken.min., fill = Weatherconditions)) +
  geom_boxplot() +
  labs(title = "Boxplot Weatherconditions vs Time taken", x = "Weather Condition", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("Cloudy" = "Cloudy", "Fog" = "Fog", "NaN" = "Missing",
                                                  "Standstorms" = "Standstorms", "Stormy" = "Stormy", "Sunny" = "Sunny", "Windy" = "Windy")) +
  scale_x_discrete(labels = c("Cloudy" = "Cloudy", "Fog" = "Fog", "NaN" = "Missing",
                              "Standstorms" = "Standstorms", "Stormy" = "Stormy", "Sunny" = "Sunny", "Windy" = "Windy")) +
  guides(fill = guide_legend(title = "Weather condition"))

#Time Taken Vs Road_Traffic_density
ggplot(data = Food, aes(x = Road_traffic_density, y = Time_taken.min., fill = Road_traffic_density)) +
  geom_boxplot() +
  labs(title = "Boxplot Traffic_density vs Time taken", x = "Traffic Density", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                                                  "Medium" = "Medium", "NaN" = "Missing")) +
  scale_x_discrete(labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                              "Medium" = "Medium", "NaN" = "Missing")) +
  guides(fill = guide_legend(title = "Traffic"))

summary(Food)

#Time Taken Vs Vehicle condition
ggplot(data = Food, aes(x = Vehicle_condition, y = Time_taken.min., fill = Vehicle_condition)) +
  geom_boxplot() +
  labs(title = "Boxplot of Vehicle condition vs Time taken", x = "Vehicle Condition", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("0" = "High", "Jam" = "Jam", "Low" = "Low",
                                                  "Medium" = "Medium", "NaN" = "Missing")) +
  scale_x_discrete(labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                              "Medium" = "Medium", "NaN" = "Missing")) +
  guides(fill = guide_legend(title = "Vehicle Condition"))

#Time Taken vs Type of order
ggplot(data = Food, aes(x = Type_of_order, y = Time_taken.min., fill = Type_of_order)) +
  geom_boxplot() +
  labs(title = "Boxplot OF Type of order vs Time taken", x = "Order type", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                                                  "Medium" = "Medium", "NaN" = "Missing")) +
  scale_x_discrete(labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                              "Medium" = "Medium", "NaN" = "Missing")) +
  guides(fill = guide_legend(title = "Order type"))

#Time taken vs Type of vehicle
ggplot(data = Food, aes(x = Type_of_vehicle, y = Time_taken.min., fill = Type_of_vehicle)) +
  geom_boxplot() +
  labs(title = "Boxplot of Type of Vehicle vs Time taken", x = "Vehicle Type", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                                                  "Medium" = "Medium", "NaN" = "Missing")) +
  scale_x_discrete(labels = c("High" = "High", "Jam" = "Jam", "Low" = "Low",
                              "Medium" = "Medium", "NaN" = "Missing")) +
  guides(fill = guide_legend(title = "Vehicle"))

#time taken vs Multiple Deliveries
ggplot(data = Food, aes(x = multiple_deliveries, y = Time_taken.min., fill = multiple_deliveries)) +
  geom_boxplot() +
  labs(title = "Boxplot of multiple deliveries vs Time taken", x = "multiple deliveries", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c("0" = "0", "1" = "1", "2" = "2",
                                                  "3" = "3", "NaN" = "Missing")) +
  scale_x_discrete(labels = c("0" = "0", "1" = "1", "2" = "2",
                              "3" = "3", "NaN" = "Missing")) +
  guides(fill = guide_legend(title = "# deliveries"))

#time taken vs Festival
ggplot(data = Food, aes(x = Festival, y = Time_taken.min., fill = Festival)) +
  geom_boxplot() +
  labs(title = "Boxplot of Festival vs Time taken", x = "Festival Season", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c( "NaN " = "Missing", "No" = "No", "Yes" = "Yes")) +
  scale_x_discrete(labels = c("NaN " = "Missing", "No" = "No", "Yes" = "Yes")) +
  guides(fill = guide_legend(title = "Festival Season or Not"))

#Time taken vs City
ggplot(data = Food, aes(x = City, y = Time_taken.min., fill = City)) +
  geom_boxplot() +
  labs(title = "Boxplot City type vs Time taken", x = "City", y = "Time taken") +
  scale_fill_brewer(palette = "Blues", labels = c( "Metropolitian" = "Metropolitian", "NaN " = "Missing", "Semi-Urban" = "Semi-Urban","Urban"="Urban")) +
  scale_x_discrete(labels = c( "Metropolitian" = "Metropolitian", "NaN " = "Missing", "Semi-Urban" = "Semi-Urban","Urban"="Urban")) +
  guides(fill = guide_legend(title = "Festival Season or Not"))

#Correlation heat map for numerical variables in
num = Food %>%
  select_if(is.numeric)
cor_matrix = cor(num)

ggplot(data = as.data.frame(as.table(cor_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(high = "#064D51", mid ="#48CBC5", low = "#C0F2F3", midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  ggtitle("Correlation Heat Map of Numerical Variables")+labs(x="",y="") +
  geom_text(aes(label = round(Freq,2)),color = "white") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############## FAMD ################
#FA = FAMD(test, ncp=3)
#summary(FA)

#### K prototype clustering ###############
# Install and load required packages
#install.packages("cluster")
library(cluster)
set.seed(7)
total_withinss = numeric(0)
mean_silhouette_score = numeric(0)

for (i in 2:7){
  
  kproto = clustMixType::kproto(test,
                                k=i,
                                nstart=26)
  
  total_withinss[i-1] = kproto$tot.withinss
  clusters = kproto$cluster
  
  silhouette_scores <- silhouette(clusters, dist(test), keep.data = TRUE)
  silhouette_scores = as.data.frame(silhouette_scores)[,3]
  mean_silhouette_score[i-1] = mean(silhouette_scores)
  cat("iteration:", i)
  
}

total_withinss
mean_silhouette_score


tibble(k=2:7,
       total_error = total_withinss) %>%
  ggplot(aes(x=k,
             y=total_error)) +
           geom_point(size=2)+
           geom_line() +
           theme_bw()+
           labs(x = "Number of Clusters",
                y = "tot.withinss")#+
           #geom_text(x=3,
            #       y=total_withinss[3],
            #        label = "ELBOW",
            #        alpha = 0.5,
            #        color = "blue",
            #        size = 5)

clusters = kproto$cluster

# Install and load required packages
install.packages("cluster")
library(cluster)

# Compute silhouette scores manually for mixed-type data
silhouette_scores <- silhouette(clusters, dist(test), keep.data = TRUE)
silhouette_scores = as.data.frame(silhouette_scores)[,3]
mean_silhouette_score = mean(abs(silhouette_scores))
