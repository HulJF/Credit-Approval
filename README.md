# Credit-Approval
---
title: "R Notebook"
output: html_notebook
---

#Libraries
```{r}
library(tidyverse)
library(lubridate)
library(caTools)
library(class)
library(gmodels)
library(corrplot)
library(Metrics)
library(olsrr)
library(gganimate)
library(tidyverse)
library(ggthemes)
library(plotly)
library(neuralnet)
library(dplyr) 
library(tidyverse)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
```


#Datasource : https://www.kaggle.com/datasets/rikdifos/credit-card-approval-prediction

#Import Dataset1
```{r}
Application <- read.csv("~/Data Visualization/Final Project/application_record.csv", stringsAsFactors = TRUE )
head (Application)
```
#Import Dataset2
```{r}
Record <- read.csv("~/Data Visualization/Final Project/credit_record.csv", stringsAsFactors = TRUE )
head (Record)
```

#Merge Datasets
```{r}
Credit <- merge(Application, Record, # dataset names
      by = "ID" , # by common variable
      all = FALSE, 
      all.Application = all, 
      all.Record = all,
      sort = TRUE, # Should the result be sorted on the by columns
      suffixes = c(".Application",".Record")
      )
```

#Overview of the dataset

```{r}
#Overview of the NA 
sapply(Credit, function(x)sum(is.na(x)))

```
```{r}
#Variables presents in the dataset
summary(Credit)
```

#Create a new column defining people with good credit situattion

```{r}
Credit$IncreaseCL <- as.factor(
  ifelse ( Credit$AMT_INCOME_TOTAL <= -109.73 & 
             Credit$STATUS == "X"|
             Credit$STATUS == "C", "Yes", ifelse( Credit$STATUS == "4" | 
                                                  Credit$STATUS == "5", "No", "Possible") ))
head(Credit)
```




```{r}
str(Credit)
```

```{r}
Credit$MONTHS_BIRTH <- Credit$DAYS_BIRTH/30
Credit$MONTHS_EMPLOYED <- Credit$DAYS_EMPLOYED/30
```

# Overview of the people with excellent credit
```{r}
ExcellentCredit<-subset(Credit, IncreaseCL == "Yes")
head(ExcellentCredit)

#Housing 
Housing <-group_by(ExcellentCredit,NAME_HOUSING_TYPE)
arrange(summarise (Housing, Number = n()) ,desc(Number)) %>% ggplot(aes(x=NAME_HOUSING_TYPE, y=Number ,fill= NAME_HOUSING_TYPE))+  labs( title = " Housing type of excellent credit persons  ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")

#Own Estate
Estate <-group_by(ExcellentCredit,FLAG_OWN_REALTY )
arrange(summarise (Estate, Number = n()) ,desc(Number)) %>% ggplot(aes(x=FLAG_OWN_REALTY, y=Number ,fill= FLAG_OWN_REALTY))+  labs( title = " Property ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")

#Own Car
Car <-group_by(ExcellentCredit,FLAG_OWN_CAR)
arrange(summarise (Car, Number = n()) ,desc(Number)) %>% ggplot(aes(x=FLAG_OWN_CAR, y=Number ,fill= FLAG_OWN_CAR))+  labs( title = " Car  ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")


#Work Experience
plot<- ggplot(data = ExcellentCredit)+
  geom_point(aes(x = MONTHS_EMPLOYED, y = MONTHS_BIRTH, color = MONTHS_BALANCE))+ coord_flip()+theme_clean()+labs(title = "Age vs Experience")
plot

#Education
ggplot(data = ExcellentCredit)+
  geom_histogram(mapping = aes(x = NAME_EDUCATION_TYPE ), stat = "count" , binwidth = 3, fill = "blue")+ coord_flip()+theme_base()+labs(title = "Education Level", x= " ")

#Family Status
Status <-group_by(ExcellentCredit, NAME_FAMILY_STATUS)
arrange(summarise (Status, Number = n()),desc(Number))
ggplot(data = Status)+
  geom_histogram(mapping = aes(x = NAME_FAMILY_STATUS), stat = "count" , binwidth = 3, fill = "green")+ coord_flip()+theme_calc()+ labs(title = "Marital Status", x= " ")

print(" The persons eligible to credit line increase are mostly married and they own a property ")
```
![image](https://user-images.githubusercontent.com/95668517/166390482-2442e05e-48f7-4f96-9f89-ed91475a74e4.png)



###Naive Bayes

```{r}

## Prepare Training and Testing Sets

set.seed(123) #to guarantee repeatable results

# Split the data
Sample1 <- sample.split(Credit, SplitRatio = .75)
Train1 <- subset(Credit, Sample1 == TRUE)
Test1<- subset(Credit, Sample1 == FALSE)

```


```{r}
#Build the model with train

NBModel<-naiveBayes(IncreaseCL ~ ., data = Train1)

```

```{r}
NBPrediction<- predict(NBModel, Test1, type = "class")

```

```{r}
#create matrix

NBMatrix <- table(Test1$IncreaseCL, NBPrediction, dnn = c("Actual", "Prediction"))
NBMatrix


```


```{r}
# Accuracy
Accuracy<- sum( diag(NBMatrix)/ sum(NBMatrix))
Accuracy
```

###Decision Trees###


```{r}
#Build a CART Tree
cartTreeModel <- rpart(IncreaseCL ~., data = Train1)
cartTreeModel

```
```{r}
# Visualize the tree
rpart.plot(cartTreeModel,extra = 3, under = TRUE)
```
#Prediction
```{r}
pred.cart <- predict(cartTreeModel, newdata = Test1, type = "class")
CART.matrix <- table(Test1$IncreaseCL, pred.cart, dnn = c("Actual", "Prediction"))
CART.matrix
```

````{r}
# Accuracy function
Model_Accuracy <- function(matrix){
  acc <- sum(diag(matrix)) / sum(matrix)
  cat("The accuracy for the model with CART is", acc, "\n")
}
Model_Accuracy(CART.matrix)
```



##.
##SVM
##.

```{r}

## Prepare Training and Testing Sets

set.seed(123) #to guarantee repeatable results

# Split the data
Sample2 <- sample.split(Credit, SplitRatio = .75)
Train2 <- subset(Credit, Sample2 == TRUE)
Test2<- subset(Credit,Sample2 == FALSE)

```

#SVM Model
Train a simple linear SVM
```{r}
modelSVM <- ksvm(IncreaseCL ~., data = Train2, kernel = "vanilladot")

# look at basic information about the model
modelSVM
```

#Predict
```{r}
pred <- predict(modelSVM, Credit_test)

#Generate Confusion Matrix
confusionMatrix <- table (
  pred,
  Credit_test$Approved ,
  dnn = c("Prediction", "Actual"))

#Calculate accuracy
accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)
cat("Vanilla Kernel Accuracy:",accuracy)
```

#Train a rbfdot SVM
```{r}
set.seed(12345)
modelSVM2 <- ksvm(Approved ~., data = Credit_train, kernel = "rbfdot")

#look at basic information about the model
modelSVM2
```

#Predict
```{r}
pred2 <- predict(modelSVM2, Credit_test)

#Generate Confusion Matrix
confusionMatrix2 <- table (
  pred2,
  Credit_test$Approved ,
  dnn = c("Prediction", "Actual"))

#Calculate accuracy
accuracy2 <- sum(diag(confusionMatrix2))/sum(confusionMatrix2)
cat(" Gaussian Kernel Accuracy:",accuracy2)

```
#Train a polydot SVM
```{r}
set.seed(12345)
modelSVM3 <- ksvm(primary_energy_consumption ~., data = EnergyConsumption_train, kernel = "polydot")

#look at basic information about the model
modelSVM3
```

#Predict
```{r}
pred3 <- predict(modelSVM3, EnergyConsumption_test)

#Generate Confusion Matrix
confusionMatrix3 <- table (
  pred3,
  EnergyConsumption_test$primary_energy_consumption ,
  dnn = c("Prediction", "Actual"))

#Calculate accuracy
accuracy3 <- sum(diag(confusionMatrix3))/sum(confusionMatrix3)
cat(" Polydot Kernel Accuracy:",accuracy3)

```

#Train a tanhdot SVM
```{r}
set.seed(12345)
modelSVM4 <- ksvm(Approved ~., data = Credit_train, kernel = "tanhdot", cost = 5)

#look at basic information about the model
modelSVM4
```

#Predict
```{r}
pred4 <- predict(modelSVM4, Credit_test)

#Generate Confusion Matrix
confusionMatrix4 <- table (
  pred4,
  Credit_test$Approved ,
  dnn = c("Prediction", "Actual"))

#Calculate accuracy
accuracy4 <- sum(diag(confusionMatrix4))/sum(confusionMatrix4)
cat(" tanhdot Kernel Accuracy:",accuracy4)

```
