# Credit-Approval
---
title: "Credit Situation"
author: "HJF
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
![image](https://user-images.githubusercontent.com/95668517/166390625-08ec1888-be5b-46b7-a375-2673cebd1625.png)

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
![image](https://user-images.githubusercontent.com/95668517/166390761-84b40411-6f07-428e-8a00-c89085e46c35.png)


```{r}
Credit$MONTHS_BIRTH <- Credit$DAYS_BIRTH/30
Credit$MONTHS_EMPLOYED <- Credit$DAYS_EMPLOYED/30
```

# Overview of the people with excellent credit
```{r}
ExcellentCredit<-subset(Credit, IncreaseCL == "Yes")
head(ExcellentCredit)
```


```{r}
#Housing 
Housing <-group_by(ExcellentCredit,NAME_HOUSING_TYPE)
arrange(summarise (Housing, Number = n()) ,desc(Number)) %>% ggplot(aes(x=NAME_HOUSING_TYPE, y=Number ,fill= NAME_HOUSING_TYPE))+  labs( title = " Housing type of excellent credit persons  ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")
```
![image](https://user-images.githubusercontent.com/95668517/166390853-b18fbbd5-6d70-479f-bd5d-2cfe980d5ac3.png)

```{r}
#Own Estate
Estate <-group_by(ExcellentCredit,FLAG_OWN_REALTY )
arrange(summarise (Estate, Number = n()) ,desc(Number)) %>% ggplot(aes(x=FLAG_OWN_REALTY, y=Number ,fill= FLAG_OWN_REALTY))+  labs( title = " Property ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")
```
![image](https://user-images.githubusercontent.com/95668517/166390894-c6723c86-8c37-4ddd-9df9-19fca9a2b2d4.png)

```{r}
#Own Car
Car <-group_by(ExcellentCredit,FLAG_OWN_CAR)
arrange(summarise (Car, Number = n()) ,desc(Number)) %>% ggplot(aes(x=FLAG_OWN_CAR, y=Number ,fill= FLAG_OWN_CAR))+  labs( title = " Car  ", x= " ", y =" Quantity")+geom_bar(stat="identity", angle =45)+coord_flip()+theme_economist()+theme(legend.position = "none")
```
![image](https://user-images.githubusercontent.com/95668517/166390950-362fe092-e5e1-4da5-9c27-18fdef76fe24.png)

```{r}
#Work Experience
plot<- ggplot(data = ExcellentCredit)+
  geom_point(aes(x = MONTHS_EMPLOYED, y = MONTHS_BIRTH, color = MONTHS_BALANCE))+ coord_flip()+theme_clean()+labs(title = "Age vs Experience")
plot
```
![image](https://user-images.githubusercontent.com/95668517/166390988-a760785d-f9b7-4b29-b8f1-ae901a9a6047.png)

```{r}
#Education
ggplot(data = ExcellentCredit)+
  geom_histogram(mapping = aes(x = NAME_EDUCATION_TYPE ), stat = "count" , binwidth = 3, fill = "blue")+ coord_flip()+theme_base()+labs(title = "Education Level", x= " ")
```
![image](https://user-images.githubusercontent.com/95668517/166391041-96a544c3-458d-4b1e-a38d-c0ab6b6520a7.png)
  
```{r}
#Family Status
Status <-group_by(ExcellentCredit, NAME_FAMILY_STATUS)
arrange(summarise (Status, Number = n()),desc(Number))
ggplot(data = Status)+
  geom_histogram(mapping = aes(x = NAME_FAMILY_STATUS), stat = "count" , binwidth = 3, fill = "green")+ coord_flip()+theme_calc()+ labs(title = "Marital Status", x= " ")
 print(" The persons eligible to credit line increase are mostly married and they own a property ")
```
![image](https://user-images.githubusercontent.com/95668517/166397332-a026424e-4e46-4aab-9756-3c876d78ecb9.png)

# Machine Learning Models

# Naive Bayes

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
![image](https://user-images.githubusercontent.com/95668517/166397895-6f2fd74b-6e02-4db3-8c9b-74792b2049a5.png)


```{r}
# Accuracy
Accuracy<- sum( diag(NBMatrix)/ sum(NBMatrix))
Accuracy
```
![image](https://user-images.githubusercontent.com/95668517/166398054-5d3ab0c4-4e7c-4d6e-bd80-934e9cad86df.png)


# Decision Trees


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
![image](https://user-images.githubusercontent.com/95668517/166398318-f1c4d467-2d5c-4125-aaa7-14b2227fa121.png)


````{r}
# Accuracy function
Model_Accuracy <- function(matrix){
  acc <- sum(diag(matrix)) / sum(matrix)
  cat("The accuracy for the model with CART is", acc, "\n")
}
Model_Accuracy(CART.matrix)
```
![image](https://user-images.githubusercontent.com/95668517/166398500-4e0c33d6-6f62-48d4-b667-ae8b1e40141f.png)

---
# Other Visualization
---
Shiny Dashboard     : *https://hjeanfr.shinyapps.io/Credit_Project/
Tableau Dashboard1  : *https://public.tableau.com/authoring/CreditStatus/Dashboard1#1
Tableau Dashboard2  : *https://public.tableau.com/authoring/CreditStatus/Dashboard2#2



