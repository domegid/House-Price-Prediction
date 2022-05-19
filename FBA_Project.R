#Setting the working directory, in which the data is stored
setwd("F:/FBA Project")
#Reading the dataset as a data frame
data = read.csv("house_data.csv")
#Getting dimensions of the dataset
dim(data)

#Pre processing + Data Cleaning:

#Getting the summary of the data
summary(data)
#Cleaning the date column to remove extra characters
data$date = substr(data$date,1,8)
#converting the string YYYYMMDD to YYYY-MM-DD date format
data$date <- as.Date(data$date, "%Y%m%d")


#checking the first few rows of the data
head(data)
#Checking for missing values in the dataset
summary(is.na(data))
#Checking for outliers
boxplot(data[,'price'])
boxplot(data[,c('sqft_living','sqft_above')])
boxplot(data[,c('sqft_living','sqft_lot','sqft_above')])

#Replacing outliers with NA
for(x in c('price','sqft_living','sqft_lot','sqft_above','bedrooms')){
  val = data[,x][data[,x] %in% boxplot.stats(data[,x])$out]
  data[,x][data[,x] %in% val] = NA
}
#Dropping rows with null values
data=tidyr::drop_na(data)
#Checking if all NA values have been dealt with
summary(is.na(data))
#Checking the boxplot after removing outliers
boxplot(data[,'price'])
boxplot(data[,c('sqft_living','sqft_above')])
boxplot(data[,c('sqft_living','sqft_lot','sqft_above')])
#Dropping redundant columns
data = subset(data, select = -c(id,lat,long) )
#Checking new dimesnions of the data
dim(data)

#applying multiple linear regression before dimensionality reduction
index <- sample(1:nrow(data), size=0.8*nrow(data))
train <- data[index, ]
test <- data[-index, ]
e5=lm(price~.,data=train)
p5 = predict(e5, test)
rms[5]<-rmse(test$price,p5)
rms

#applying random forest regressor before dimensionality reduction
rf.fit <- randomForest(price ~ ., data=data, ntree=1000,keep.forest=FALSE, importance=TRUE)
print(rf.fit)

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#Data Visualization:
library(ggplot2)
library(ggpubr)
#Plotting the Frequency of Unique values for various columns
table(data$bedrooms)
barplot(table(data$bedrooms),col='blue')
table(data$bathrooms)
barplot(table(data$bathrooms),col='red')
table(data$waterfront)
barplot(table(data$waterfront),col='green')
table(data$floors)
barplot(table(data$floors),col='black')
table(data$condition)
barplot(table(data$condition),col='pink')
table(data$view)
barplot(table(data$view),col='yellow')
table(data$grade)
barplot(table(data$grade),col='orange')
#For Correlation HeatMap
library(heatmaply)
# Plotting corr heatmap
df = subset(data, select = -c(date,zipcode,yr_renovated,sqft_lot15,sqft_living15) )
heatmaply_cor(x = cor(df), xlab = "Features", ylab = "Features", k_col = 2, k_row = 2)
#dropping redundant columns based on heatmap of correlation
data=subset(data,select=-c(sqft_above))
#Checking Correlations of features with target i.e price
cor(df,df$price)
#Dropping Factors that do not have much effect on the price
data=subset(data,select=-c(yr_built,condition,waterfront,sqft_lot))
dim(data)
#Checking Heatmap after dropping redundant columns
df = subset(data, select = -c(date,zipcode,yr_renovated,sqft_lot15,sqft_living15) )
heatmaply_cor(x = cor(df), xlab = "Features", ylab = "Features", k_col = 2, k_row = 2)





#splitting data into train-test (80%-20%)
index <- sample(1:nrow(data), size=0.8*nrow(data))
train <- data[index, ]
test <- data[-index, ]

library(tidyverse)
library(caret)
library(Metrics)
library(randomForest)
#Building the model with polynomial regression
model <- lm(price ~ poly(bedrooms+bathrooms+sqft_living+floors+grade, 5, raw = TRUE),data = train)
# Making predictions
predictions <- model %>% predict(test)
modelPerfomance = data.frame(
  RMSE = RMSE(predictions, test$price),
  R2 = R2(predictions, test$price)
)
print(modelPerfomance)
ggplot(train, aes(bedrooms+bathrooms+sqft_living+floors+view+grade, price))+geom_point()+stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
ggplot(train, aes(sqft_lot15, price))+geom_point()+stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
ggplot(train, aes(grade, price))+geom_point()+stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

rms = 0

e0 <- lm(price ~ grade + I(grade^2), data = train)
print(e0)
p0 <- predict(e0,test)
rmse(test$price,e0)

e1=lm(price~grade,data=train)
p1 = predict(e1, test)
rms[1]<-rmse(test$price,p1)

e2=lm(price~sqft_living+grade+floors+bathrooms+bedrooms+view+sqft_basement+sqft_lot15+zipcode,data=train)
p2 = predict(e2, test)
rms[2]<-rmse(test$price,p2)

e3=lm(price~.,data=train)
p3 = predict(e3, test)
rms[3]<-rmse(test$price,p3)

e4=lm(price~sqft_lot15,data=train)
p4 = predict(e4, test)
rms[4]<-rmse(test$price,p4)

rms

rf.fit1 <- randomForest(price ~ ., data=train, ntree=1000,keep.forest=FALSE, importance=TRUE)
print(rf.fit1)

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#The Random Forest Regressor applied before dimensionality reduction gives the best prediction

