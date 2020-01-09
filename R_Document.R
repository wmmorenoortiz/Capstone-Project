# require package
# if (!require(package)) install.packages('pastecs')
# if (!require(package)) install.packages('psych')
# library(pastecs)
# library(ggplot2)
# library(tidyverse)
# library(psych)

## **Step 1:** Load the data base
database <- read.csv("Data/indian_liver_patient.csv")
head(database)


## **Step 2:** Exploratory Data Analusis 

### Summay Statistics

t <- describeBy(database,
           digits= 2)
h = data.frame(t$Female)
round(h, 2)

ext <- with(database.gathered, Map(function(x) ave(value, Gender, FUN=x), 
                    list(avg=mean, min=min, max=max, SD=sd)))
cbind(database.gathered, ext)

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

