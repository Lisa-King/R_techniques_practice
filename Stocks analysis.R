## Stocks analysis

library(tidyverse)
# read stocks.csv file into a dataframe called stocks
stocks <- read.csv('stocks.csv')
head(stocks)
summary(stocks)
str(stocks) # 36936 obs. of  8 variables

# drop NA values using drop_na function
stocks <- stocks %>%
  drop_na()

# create a new column called MONEY_VOLUME, the formula is ((OPEN + CLOSE) * VOLUME)/2
stocks <- stocks  %>% 
  mutate(MONEY_VOLUME = ((OPEN + CLOSE) * VOLUME)/2)

head(stocks)

# Order MONEY_VOLUME by descending and check head() again
stocks %>% 
  arrange(desc(MONEY_VOLUME)) %>% 
  head()

# read industry file into a dataframe called ind
ind <- read.csv('industry.csv')
# add .AX sufix into Code column so it can join with stocks data
ind$Code <- paste0(ind$Code,'.AX')
head(ind)
summary(ind)

# find out the top 5 stock codes with the largest MarketCap, assign it to top_ind dataframe
top_ind <- ind %>% 
  arrange(desc(MarketCap))  %>% 
  head(5)

# plot a barchart for top_ind dataframe, with x axis = Code, y axis = MarketCap
ggplot(top_ind, aes(x=Code, y=MarketCap)) + geom_bar(stat="identity")


## use stocks dataframe to left join with ind dataframe, by ax_ticker and Code column
df <- stocks %>% left_join(ind, by=c('ax_ticker' = 'Code'))

# have a look at the new data frame
head(df)

# create a new column called CLOSE_PRE which shows the previous day's CLOSE value for each stocks
df <- df  %>% 
  group_by(ax_ticker) %>% 
  arrange(ax_ticker,Date) %>% 
  mutate(CLOSE_PRE = lag(CLOSE))

# create a new column called chg which shows the percentage change on CLOSE from previous day
df <- df  %>% 
  mutate(chg = (CLOSE - CLOSE_PRE)/CLOSE)

head(df)

# plot the distribution of the column chg, only filter on abs(chg) < 20%
ggplot(aes(x=chg), data = df %>% filter(abs(chg) < 0.2))+
  geom_histogram(color =I('black'),fill = "red")+
  ggtitle('daily change percentage')


library(randomForest)
library(precrec)

## add columns chg1, which is the chg value for previous day, chg2 which is the chg value at two days ago, chg3 which is the chg value at three days ago and chg4 which is the chg value at four days ago
## add a column called target which has value 1 if CLOSE value increased for the next day, otherwise set it to 0. Replace xxx with your code 
## hint: use lag, lead and ifelse function
df <- df %>% 
  group_by(ax_ticker) %>% 
  arrange(ax_ticker, Date) %>%
  mutate(chg1 = lag(chg)) %>% 
  mutate(chg2 = lag(chg1)) %>% 
  mutate(chg3 = lag(chg2)) %>% 
  mutate(chg4 = lag(chg3))

head(df)

df <- df %>% 
  mutate(target = ifelse((CLOSE > CLOSE_PRE),1,0))

head(df)

# build a dataframe which only select ax_ticker, Date, Weight, chg, chg1, chg2, chg3, chg4, target, drop any rows have NA value
df_model <- df %>% 
  subset(select=c('ax_ticker','Date','Weight', 'chg','chg1','chg2','chg3','chg4','target')) %>% 
  drop_na()
#or using  df_model <- df %>% data.frame('ax_ticker','Date','Weight', 'chg','chg1','chg2','chg3','chg4','target') %>% na.omit()
#or using  df_model <- df %>% select(ax_ticker, Date, Weight, chg, chg1, chg2, chg3, chg4, target) %>% na.omit()

# change target variable to factor data type so we can build a classification model later on
df_model$target <- as.factor(df_model$target)

# split data into training and test by index
train = sample(1: nrow(df_model), nrow(df_model)/2)

# build a randomForest model on training data, note you need to drop ax_ticker and Date column before putting into model
set.seed (1)
df_train <- df_model[train,]
df_test <- df_model[-train,]
model = randomForest(target ~ Weight + chg + chg1 + chg2 + chg3 + chg4, data = df_train, importance = TRUE ) 

# make a prediction on test data
tree.pred = predict(model, df_test %>% 
                      select(-ax_ticker, -Date), type="prob")

library(ggplot2)
# plot ROC curve, replace xxx with your code
precre_obj <- evalmod(scores = tree.pred[,2], labels = as.vector(df_test$target))

# to solve Error in graphics::plot.new() : figure margins too large
par(mar=c(1,1,1,1))
#This can happen when your plot panel in RStudio is too small for the margins of the plot 
#you are trying to create. Try making expanding it and then run your code again.
autoplot(precre_obj)



