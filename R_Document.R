# require package
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if (!require(package)) install.packages('psych', repos = "http://cran.us.r-project.org")
library(caret)
library(tidyverse)
library(ggplot2)
library(psych)

## **Step 1:** Load the data base
database <- read.csv("Data/indian_liver_patient.csv")
head(database)

## **Step 2:** Exploratory Data Analusis 
### Summay Statistics
describeBy(database, digits= 2)

### Visualization
database.gathered <- database %>% as.data.frame() %>% 
  gather(key = "variable", value = "value", - Gender) 

ggplot(data = database.gathered , mapping = aes(x = value, color = Gender)) +
  geom_histogram() +
  facet_wrap(facets =  vars(variable ))
# Standardize
scale_database <- database %>% select(-Gender,-Dataset) %>% scale() %>% as.data.frame() %>% 
  cbind(Gender = database$Gender)

database.gathered <- scale_database  %>% as.data.frame() %>% 
  gather(key = "variable", value = "value", - Gender)

ggplot(data = database.gathered , mapping = aes(x = value, color = Gender)) +
  geom_histogram() +
  facet_wrap(facets =  vars(variable ))
# qq-norm
ggplot(data = database.gathered , mapping = aes(sample = value, color = Gender)) +
  stat_qq() +  stat_qq_line(color = "black") +
  facet_wrap(facets =  vars(variable )) 


### **Step 3:**  Split the database in training and testing
#### Split the database in training and testing
set.seed(755)
test_index <- createDataPartition(y = database$Dataset, times = 1,
                                  p = 0.2, list = FALSE)

train_set <- database[-test_index,]
test_set <- database[test_index,]


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
names(train_set)

# Modify the response variable.
train_set$Dataset[train_set$Dataset == 1] = 1
train_set$Dataset[train_set$Dataset == 2] = 0
test_set$Dataset[test_set$Dataset == 1] = 1
test_set$Dataset[test_set$Dataset == 2] = 0

# convert the variable into factor
train_set$Gender <- as.factor(train_set$Gender)
test_set$Gender <- as.factor(test_set$Gender)

# Step  4: Choose the Model and train the model with the training base

#### Model 1: Using all the variables
mol_1 <- glm(Dataset ~. , data = train_set, family = binomial() )

#### Model 2: Variables + not-correlation
round(cor(database[,-2]),2)
train_set_mol_2 = train_set %>% select(-Total_Bilirubin,-Total_Protiens)
mol_2 <- glm(Dataset ~. , data = train_set_mol_2, family = binomial())

#### Model 3: Significant variables
summary(mol_1)
train_set_mol_3 = train_set %>% select(Age,Alamine_Aminotransferase)
mol_3 <- glm(Dataset ~. , data = train_set, family = binomial())

### **Step  5:** Predict the possible ratings for the test base
y_hat_1 <- predict(mol_1,test_set,type = "response")
glm.pred_1  <- ifelse(y_hat_1 > 0.5, "1", "0")
accuracy <- data_frame(method="Using all the variables",
                       Accuracy = mean(glm.pred_1 == test_set$Dataset))

y_hat_2 <- predict(mol_2,test_set,type = "response")
glm.pred_2  <- ifelse(y_hat_2 > 0.5, "1", "0")
accuracy <- bind_rows(accuracy,data_frame(method="Variables + correlation",
                       Accuracy = mean(glm.pred_2 == test_set$Dataset)))

y_hat_3 <- predict(mol_3,test_set,type = "response")
glm.pred_3  <- ifelse(y_hat_3 > 0.5, "1", "0")
accuracy <- bind_rows(accuracy,data_frame(method="Significant variables",
                                          Accuracy = mean(glm.pred_3 == test_set$Dataset)))

### **Step  6:** Accuracy
accuracy





