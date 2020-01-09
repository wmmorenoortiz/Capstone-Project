# require package
if (!require(package)) install.packages('pastecs')
if (!require(package)) install.packages('psycho')

library(pastecs)
library(ggplot2)
library(tidyverse)


head(database)


database.gathered <- database %>% as.data.frame() %>% 
  gather(key = "variable", value = "value", - Gender)

ggplot(data = database.gathered , mapping = aes(x = value, color = Gender)) +
  geom_histogram() +
  facet_wrap(facets =  vars(variable ))

scale_database <- database %>% select(-Gender,-Dataset) %>% scale() %>% as.data.frame() %>% 
  cbind(Gender = database$Gender)

database.gathered <- scale_database  %>% as.data.frame() %>% 
  gather(key = "variable", value = "value", - Gender)

head(scale_database)

