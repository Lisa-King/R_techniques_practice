#Introduction to tidyverse
#We are going to explore the Titanic data set, where each observation is a person and each variable is a feature such as Name, Age and Survived (or not).

# Import the Tidyverse
#install.packages("tidyverse")
library(tidyverse)

# Import input data
setwd("~/Desktop/data-5/39/8cb82054b06d221bd12080581cf3d088")
passengers <- read.csv("train.csv")

#show the first 6 rows of observations.
head(passengers)
#using summary() function to get an statistical overview of your data
summary(passengers)

#Now do the same using a pipe %>%, one of the handiest tools in the tidyverse:
passengers %>%
  summary()

#Do the same after dropping observations that have missing values. You can concatenate pipes!
passengers %>%
  drop_na() %>%
  summary()

#Wrangle your data
#Now it's time to explore your data and get some initial insight into the dataset. 
#You'll be using dplyr verbs such as filter(), arrange() and mutate(), which do # exactly what they say.
#Filter all 'male' passengers:
passengers %>%
  filter(Sex == 'male')

# Arrange by decreasing Fare
passengers %>%
  arrange(desc(Fare))
#arrange by ascending Fare, only when descending order by adding desc() function;
passengers %>%
  arrange(Fare)

# Create new column FamSize (size of family) = siblings/Spouse + parents/children
passengers %>%
  mutate(FamSize = SibSp + Parch)

#using mutat() function to create a new column through the original columns
# Create new column FamSize (size of family) = siblings/Spouse + parents/children
# Arrange by decreasing FamSize
passengers %>%
  mutate(FamSize = Parch + SibSp) %>%
  arrange(desc(FamSize))

#mutate() function help transfer column values, eg. 0/1 -> No/Yes, No/Yes -> 0/1 etc.
#transfer numerical values of Survived column to "No" & "Yes" (new data frame) and set to a new DataFrame passengers1
passengers1 <- passengers %>%
  mutate(Survived = ifelse(Survived == 0, "No", "Yes"))
passengers1

# Plot barplot of passenger Sex
#geom_bar() uses stat_count() by default: it counts the number of cases at each x position. 
#geom_col() uses stat_identity(): it leaves the data as is.
ggplot(passengers, aes(x = Sex)) +
  geom_bar()

# Scatter plot of Age vs Fare
#The point geom is used to create scatterplots. The scatterplot is most useful for displaying the relationship between two continuous variables. 
#It can be used to compare one continuous and one categorical variable, or two categorical variables,
ggplot(passengers, aes(x = Age, y = Fare)) +
  geom_point()

# Scatter plot of Age vs Fare colored by Sex
ggplot(passengers %>% drop_na(), aes(x = Age, y = Fare, color = Sex)) +
  geom_point()

# Scatter plot of Age vs Fare colored by Sex faceted by Survived
#through this plot, we can observed that more female than male are survived
ggplot(passengers1, aes(x = Age, y = Fare, color = Sex)) +
  geom_point() +
  facet_grid(~Survived)

# Plot barplot of passenger Sex & fill according to Survival
#we can observed that female has a higher proportion to be survived in Titanic event
ggplot(passengers1, aes(x = Sex, fill = Survived)) +
  geom_bar() 


