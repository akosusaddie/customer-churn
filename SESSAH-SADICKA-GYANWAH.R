#Data used for this analysis was obtained from Kaggle. 
#The data ranges from demographic information to types of services being provided.
#Using this data, clusters or groups of customers can be identified.

setwd('c:/users/sobob/Downloads')

myData <- read.csv("telecommunication_customer_churn.csv")

myData

#Loading the all package required for the analysis
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Boruta)
library(gridExtra)
library(cluster)
library(factoextra)
library(NbClust)

#Limiting my data set to the first 1500 rows using the pipe operator
newData<- myData%>%
  head(myData, n= 1500)

#Removing Unwanted Features/columns
newData$CaseOrder <- NULL
newData$Customer_id <- NULL
newData$Interaction <- NULL
newData$UID <- NULL
newData$County <- NULL
newData$Zip <- NULL
newData$Lat <- NULL
newData$Lng <- NULL
newData$Population <- NULL
newData$TimeZone <- NULL
newData$Job <- NULL

#Factoring categorical variables
newData <- within (newData,
{
  Area <- factor(Area, labels = c("Urban", "Suburban","Rural"))
  Marital <- factor(Marital,labels= c("Never Married", "Married","Divorced" ,"Separated", "Widowed"))
  Gender <- factor(Gender,labels = c("Male", "Female", "Nonbinary"))
  Churn <- factor(Churn, labels = c("No", "Yes"))
  Contract <- factor(Contract, labels = c("Month-to-month", "One year", "Two Year"))
  PaymentMethod <- factor(PaymentMethod, labels= c("Bank Transfer(automatic)",
                                                    "Mailed Check",
                                                    "Electronic Check",
                                                    "Credit Card (automatic)"))
  Techie <- factor(Techie, labels = c("No", "Yes"))
  Port_modem <- factor(Port_modem, labels = c("No", "Yes"))
  Tablet <- factor(Tablet, labels = c("No", "Yes"))
  Phone <- factor(Phone, labels = c("No", "Yes"))
  Multiple <- factor(Multiple, labels = c("No", "Yes"))
  InternetService <- factor(InternetService, labels = c("Fiber Optic", "DSL", "None"))
  OnlineSecurity <- factor(OnlineSecurity, labels = c("No", "Yes"))
  OnlineBackup <- factor(OnlineBackup, labels = c("No", "Yes"))
  DeviceProtection <- factor(DeviceProtection, labels = c("No", "Yes"))
  TechSupport <- factor(TechSupport, labels = c("No", "Yes"))
  StreamingTV <- factor(StreamingTV, labels = c("No", "Yes"))
  StreamingMovies <- factor(StreamingMovies, labels = c("No", "Yes"))
  PaperlessBilling <- factor(PaperlessBilling, labels = c("No", "Yes"))
})


#To see what type of object the data set is
str(newData) #The data set is a data frame of 1500 objects and 32 variables

#To view the column headers in the data set
names(newData)  #The data set has column headers like Age, Children,etc

#To have a glimpse of the data
glimpse(newData) 
#The data set has 1500 rows and 32 columns with data types,integer,character and double.


#To view a summary of the data
summary(newData)
#All variables with the character data type have a length of 1500, the others have 
#minimum and maximum values, mean, median, and 1st and 3rd quantile values

#Plotting categorical variables
p1 <- ggplot(newData, aes(x=Gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(newData, aes(x=Phone)) + ggtitle("Phone") + xlab("Phone") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(newData, aes(x=Techie)) + ggtitle("Techie") + xlab("Techie") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(newData, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(newData, aes(x=PaperlessBilling)) + ggtitle("PaperlessBilling") + xlab("PaperlessBilling") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(newData, aes(x=PaymentMethod)) + ggtitle("PaymentMethod") + xlab("PaymentMethod") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(newData, aes(x=StreamingTV)) + ggtitle("StreamingTV") + xlab("StreamingTV") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(newData, aes(x=StreamingMovies)) + ggtitle("StreamingMovies") + xlab("StreamingMovies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(newData, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(newData, aes(x=Port_modem)) + ggtitle("Port_modem") + xlab("Port_modem") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(newData, aes(x=Tablet)) + ggtitle("Tablet") + xlab("Tablet") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(newData, aes(x=Multiple)) + ggtitle("Multiple") + xlab("Multiple") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(newData, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(newData, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(newData, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(newData, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, ncol=2)

#All of the categorical variables seem to have a reasonably broad distribution


#Feature Selection
#Using Boruta to select the most important features
fs <- Boruta(Churn~., data = newData, doTrace = 2)
print(fs)

#Boruta performed 99 iterations in 32.82013 secs.
#10 attributes confirmed important: Bandwidth_GB_Year, Contract,
#InternetService, MonthlyCharge, Multiple and 5 more;
#20 attributes confirmed unimportant: Age, Area, Children, City, Contacts
#and 15 more;
#1 tentative attributes left: Income;

#creating a subset of unlabeled data with selected Features using the pipe operator
ud  <- newData  %>% 
   select(MonthlyCharge, Bandwidth_GB_Year)
ud

glimpse(ud)

#Standardizing the unlabeled data
scaled <- scale(ud)
scaled

#Application of KMeans Algorithm
set.seed(123)
#Finding the optimal number of clusters
# Elbow method
fviz_nbclust(scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE )+
  labs(subtitle = "Gap statistic method")

#NbClust() function: 30 indices for choosing the best number of clusters
nb <- NbClust(scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

#The location of a bend (knee) in the plot
#is generally considered as an indicator of the appropriate number of clusters.
#The optimal number of clusters selected is 3.


# Compute k-means with k = 3
set.seed(123)
km.results <- kmeans(scaled, centers = 3, nstart = 50)

# Print the results
print(km.results)

#Visualizing  results
# plot the clusters
fviz_cluster(km.results, data = scaled, geom = c("point"),ellipse.type = "euclid")


#Hierarchical Clustering

#Compute the euclidean distance

set.seed(123)
hc.dist <- dist(scaled, method = "euclidean")

# Print the results
print(hc.dist)

#Visualizing  results using the average method
hc <- hclust(hc.dist, method = 'average')
plot(hc)

