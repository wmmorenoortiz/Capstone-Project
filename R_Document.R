# require package
# if (!require(package)) install.packages('psych')
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
library(tidyverse)
# library(ggplot2)
# library(tidyverse)
# library(psych)

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

scale_database <- database %>% select(-Gender,-Dataset) %>% scale() %>% as.data.frame() %>% 
  cbind(Gender = database$Gender)

database.gathered <- scale_database  %>% as.data.frame() %>% 
  gather(key = "variable", value = "value", - Gender)

ggplot(data = database.gathered , mapping = aes(x = value, color = Gender)) +
  geom_histogram() +
  facet_wrap(facets =  vars(variable ))

ggplot(data = database.gathered , mapping = aes(sample = value, color = Gender)) +
  stat_qq() +  stat_qq_line(color = "black") +
  facet_wrap(facets =  vars(variable )) 


### **Step 3:**  Split the database in training and testing

#### Choose only the variables of interest.
#new_edx <- edx %>% select(rating,userId,movieId)
#head(new_edx)

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

# Step  4: Choose the Model and train the model with the training base
train_set$Dataset[train_set$Dataset == 1] = 1
train_set$Dataset[train_set$Dataset == 2] = 0
train_set$Gender <- as.factor(train_set$Gender)


test_set$Dataset[test_set$Dataset == 1] = 1
test_set$Dataset[test_set$Dataset == 2] = 0
test_set$Gender <- as.factor(test_set$Gender)



mol <- glm(Dataset ~ Age + Alamine_Aminotransferase , data = train_set, family = binomial() )
summary(mol)

y_hat <- predict(mol,test_set,type = "response")
glm.pred  <- ifelse(y_hat > 0.5, "1", "0")

table(glm.pred,test_set$Dataset )

mean(glm.pred == test_set$Dataset)

cor(database[,-2])
