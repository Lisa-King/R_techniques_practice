# Wine Quality Analysis - Exploratory Data Analysis (EDA)
# The scope of this analysis is to understand relationship of various parameters which impact the quality ratings for both Red and White wine.

# read two dataset with read.csv()
red <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', header = TRUE, sep = ';')
white <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', header = TRUE, sep = ';')
# Save dataframe wine as Wine.csv dataset 
write.csv(red, "Red.csv",row.names=FALSE)
write.csv(white, "White.csv",row.names=FALSE)
# Check data structure
# the dimension of data set: how many records and how many variables does exist? what is the name of variables? What is the attribute of each variable?

dim(red)  #return a pair which means rows and columns: [1] 1599   12
head(red)

dim(white) #[1] 4898   12
head(white)

dim(red)[1] #which show the first item of dim(red): [1] 1599
dim(white)[1] + dim(red)[1] #[1] 6497, sum the first item of both two pairs together

# There are 6497 records of wine evaluation, and 12 variables are in the data set. Two datasets have the same variable names.
# Append two datasets together to create a master dataset for further analysis. A factor variable "type" is introduced as the indicator of wine type.
# We want 'type' to be 'Red' & 'White' ('red' & 'white', to match red/white type of wine in each dataframe)
red[, 'type'] <- 'Red'
white[, 'type'] <- 'White'
# Combine the two datasets and change type to a factor variable from character type
wine <- rbind(red, white) 
wine$type <- as.factor(wine$type) 
head(wine)

# Some of names were too long, so we use some of common abbreviation to rename the columns as below:
# RS - residual sugar 
# ABV - (percentage of )alcohol 
# SO2 - sulfur.dioxide
names(wine)[c(4,6,7,11)]<-c("RS","free.SO2", "total.SO2","ABV")
head(wine)
# Here is the cleaned data, save the master data to a csv file for easy access later.Save dataframe wine as Wine.csv dataset.
write.csv(wine, "Wine.csv",row.names=FALSE)

# check the structure of wine: Only one variable "type" is factor variable, all other variables are numeric variables.
str(wine)
# statistical summary of dataframe wine
summary(wine)

#install.packages("psych")
library("psych")
# describe() from the psych library can give you an advanced descriptive statistics.
describe(wine)
round(describe(wine), 3) # keep precision as 3

# Some observations from the Summary:
# There is a big range for sulfur dioxide (SO2, both free and total) across the samples.
# Citric acid min is 0, is this actually 0, too low to register, or missing data?
# The alcohol content(ABV)varies quite a bit from 8.00 to 14.90
# The quality of the samples range from 3 (yuck) to 9
# The range for fixed acidity is quite high with minimum being 3.8 and maximum being 15.9
# PH value varies from 2.720 to 4.010, seem quite low, given that pH 7 is neutral, 4 is acidic?
# Density has an extremely small range

#We can compare the standard deviation for variables
sort(apply(wine[-13], 2, sd))
#The lowest standard deviation is for "density" variable, and "total SO2" has the highest standard deviation.

# additional information
# Generate a boxplot for all variables, boxplots for variables two by two, and separate boxplots for each variable.
# boxplot: show the line of  minimum, first quartile, second quartile(mean), third quartile and maximum;
# But we can't see all the names so flip them and make them smaller
boxplot(wine, las=2, cex.axis = 1)
# las: 'label axis' or similar

# The bigger problem is the scale, most attributes are overshadowed by the biggest (the SO2)
# You can specify the ones you want:
boxplot(wine$free.SO2, wine$total.SO2)
# also by index
boxplot(wine[, 2:3]) 

# or specify a list
boxplot(wine[, c(2,3,5,8,9,10)], las = 2, cex.axis = 0.5)
# and even with this 'zoom' view, density is still upstaged by others

# ggplot will give you a slightly prettier mess
library(ggplot2)
library(reshape2)
m1 <- melt(as.data.frame(wine[,-13]))
ggplot(m1,aes(x = variable,y = value)) + 
  facet_wrap(~variable) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# The lowest standard deviation is for "density" variable, and "total SO2" has the highest standard deviation.

# or you can plot them all in a loop, notice the different scales now
par(mfrow = c(2,6)) # 6 x 2 grid
for (i in 1:(length(wine) - 1)) {
  #for (i in 1:length(wine)) {z
  boxplot(wine[,i], main = names(wine[i]), type="l", col = 'lightblue') # I like blue
}
# ignore the error, it's 'type', or use length - 1 in loop


# check the correlation
plot(wine$free.SO2, wine$total.SO2)
# Scatterplots are useful to examine in bi-variate data such as linearity, slope, and strength.

pairs(wine[1:12],cex=0.2)
round(cor(wine[1:12]),3)

library("car") # another matrix type plot
scatterplotMatrix(wine[1:12],cex=0.2) # ignore type

# Find the distribution of variables, and describe the shape of the distribution.
hist(wine$ABV, xlab="Alcohol content", main="Histogram of Alcohol")
# The distribution of alcohol data is skewed to the right.
hist(wine$ABV[wine$type=="Red"], xlab="Alcohol content", main="Histogram of Alcohol content, Red wine")
# The distribution of alcohol content data for red wine is skewed to the right.

# Alternatively ggplot can be used.
# Compare four indicators for red wine and wihte wine separately.
library(gridExtra) 
library(tidyverse)

p1<-ggplot(aes(x=pH), data =  wine  %>% filter(type == "Red")) +
  geom_histogram(color = I('black'), fill = "red") + 
  ggtitle('pH distribution, red wine')

p2<-ggplot(aes(x=free.SO2), data =  subset(wine,type %in% c("Red"))) +
  geom_histogram(color = I('black'), fill = "red") +
  ggtitle('Free SO2 distribution, red wine')

p3<-ggplot(aes(x=total.SO2), data =  subset(wine,type %in% c("Red"))) +
  geom_histogram(color = I('black'), fill = "red") +
  ggtitle('Total SO2 distribution, red wine')

p4<-ggplot(aes(x=ABV), data =  subset(wine,type %in% c("Red"))) +
  geom_histogram(color = I('black'), fill = "red") +
  ggtitle('Alcohol content distribution, red wine')
# plot all 4, 2 x 2 

grid.arrange(p1, p2, p3, p4, ncol = 2)
# The following observations are obtained from the above plot
# The pH value seems to dispaly a normal distribution with major samples exhibiting values between 3.0 and 3.5, the others are skewed to the left
# The alcohol content seems to vary from 8 to 14 with major peaks around 9 with a lower count between 13 and 14.

#A similar distributon analysis for white wine is given below
q1<-ggplot(aes(x=pH), data =  subset(wine,type %in% c("White")))+
  geom_histogram(color =I('black'),fill = "White")+
  ggtitle('pH distribution, white wine')

q2<-ggplot(aes(x=free.SO2), data =  subset(wine,type %in% c("White")))+
  geom_histogram(color =I('black'),fill = "white")+
  ggtitle('Free SO2 distribution, white wine')

q3<-ggplot(aes(x=total.SO2), data =  subset(wine,type %in% c("White")))+
  geom_histogram(color =I('black'),fill = "white")+
  ggtitle('Total SO2 distribution, white wine')

q4<-ggplot(aes(x=ABV), data =  subset(wine,type %in% c("White")))+
  geom_histogram(color =I('black'),fill = "white")+
  ggtitle('Alcohol content distribution, white wine')

grid.arrange(q1,q2,q3,q4,ncol=2)
# Some observations from the plot are as below:
# In this case too the pH value exhibits close to normal distribution too, but so does total SO2, the alcohol 'shape' is similar to that for red wine


#The spread of the quality analysis for the Red and White types are given as below
r1<-ggplot(aes(x=quality), data =  subset(wine,type %in% c("Red")))+
  geom_histogram(color =I('black'),fill = "red")+
  ggtitle('Quality distribution for Red wine')

r2<-ggplot(aes(x=quality), data =  subset(wine,type %in% c("White")))+
  geom_histogram(color =I('black'),fill = "white")+
  ggtitle('Quality distribution for White wine')

grid.arrange(r1, r2, ncol = 1)


# Correlation plots
# as below, installing and loading GGally fails, could be an R version issue, not all libraries work all the time.
# There are alternatives:
#library(corrgram)
#library(corrplot)
#install.packages("GGally")
library(GGally)

# DIY correlation plot
# http://stackoverflow.com/questions/31709982/how-to-plot-in-r-a-correlogram-on-top-of-a-correlation-matrix
# there's some truth to the quote that modern programming is often stitching together pieces from SO 

colorRange <- c('#69091e', '#e37f65', 'white', '#aed2e6', '#042f60')
## colorRamp() returns a function which takes as an argument a number
## on [0,1] and returns a color in the gradient in colorRange
myColorRampFunc <- colorRamp(colorRange)

panel.cor <- function(w, z, ...) {
  correlation <- cor(w, z)
  
  ## because the func needs [0,1] and cor gives [-1,1], we need to shift and scale it
  col <- rgb(myColorRampFunc((1 + correlation) / 2 ) / 255 )
  
  ## square it to avoid visual bias due to "area vs diameter"
  radius <- sqrt(abs(correlation))
  radians <- seq(0, 2*pi, len = 50) # 50 is arbitrary
  x <- radius * cos(radians)
  y <- radius * sin(radians)
  ## make them full loops
  x <- c(x, tail(x,n=1))
  y <- c(y, tail(y,n=1))
  
  ## trick: "don't create a new plot" thing by following the
  ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
  ## This allows
  par(new=TRUE)
  plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, asp=1)
  polygon(x, y, border=col, col=col)
}
# usage e.g.:
# pairs(mtcars, upper.panel = panel.cor)


pairs(wine[sample.int(nrow(wine),1000),], upper.panel=panel.cor)
pairs(wine[wine$type=="Red",-13], upper.panel = panel.cor) # just red
pairs(wine[wine$type=="White",-13], upper.panel = panel.cor) # just white

# Let's have a look at red and white wine separately.
ggplot(aes(x=ABV),data =wine) + 
  geom_density(aes(fill = type)) +
  facet_wrap(~quality) +
  ggtitle('Alcohol and Quality Relationship')

# There seems to be no significant bias of the alcohol content even though there are samples with higher alcohol 
# content for red wine exhibiting a higher density reading for the quality levels of 3 and 5 as compared to white wine.
# From our earlier scatterplot matrices, alcohol seems to exhibit a strong correlation with pH value.

ggplot(aes(x=ABV, y=pH),data = wine) + 
  geom_jitter(aes(color = type, bg = type), alpha=2/10,pch=21, cex=4) +
  facet_wrap(~quality) +
  scale_color_brewer(type = 'div') +
  ggtitle('Alcohol content and pH Relationship')


# Above indicates an interestng observation: as quality rating increases (from 3 to 9), 
# red wine has instances of more pH value than white wine for similar alcohol content.

# Alcohol also exhibited a strong -ve correlation with density and a further analysis on this is provided below.
ggplot(aes(x=ABV,y=density), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median) +
  ggtitle('Alcohol content across Wine types')


# As expected,there is seems to dip in density with increase in the Alcohol content 
# and the white wines exhibit a more prominent dip.

# The negative correlation of Alcohol with Total and Free SO2 and Chlorides are analysed as below:

a1<-ggplot(aes(x=ABV,y=total.SO2), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

a2<-ggplot(aes(x=ABV,y=free.SO2), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

a3<-ggplot(aes(x=ABV,y=chlorides), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

grid.arrange(a1,a2,a3,ncol=2)

# The observations from the above analysis are as follows:
# Total SO2 White wine exhibits higher Total SO2 contents than Red wine across all Alcohol levels. Total SO2 content decreases with Alcohol content for White wine
# Free SO2 Again White wine exhibits higher Free SO2 levels across all Alcohol content though the unit difference between Red and White wine seems to be lower as compared to the Total SO2 difference. 
# The Free SO2 content decrease as the alcohol content increases for White wine.
# Chloride Red wine indicated a higher chloride content than white wine with increasing Alcohol content.
# The Chloride content is quite high at lower Alcohol content between 8 and 9 but then exhibits steady reduction till Alcohol content level of 13 before a spike. 
# White wine exhibits lower Chloride levels across Alcohol content levels and holds a steady pattern throughout
# Sulphur Dioxide Usage of SO2 in Wines has been topic of discussion for long time due to the health related issues.
# It will be intresting to see the distribution of SO2 across Red and White wine and their final impact on quality.

#Analysis of Free SO2 across the Red and White wine is provided below
ggplot(aes(x = quality, y = free.SO2), data = wine) + 
  geom_point(aes(color=type),alpha=1/10, position = 'jitter') +
  ggtitle(' Free SO2 and  Quality Relationship')

# This indicates a existence of samples with higher free SO2 for white wines for the same quality ratings with the 
# Red wine samples exhibiting lower free SO2 across the quality ratings

# Analysis of Total SO2 is provided below
ggplot(aes(x = quality, y = total.SO2), data = wine) + 
  geom_point(aes(color=type),alpha=1/4, position = 'jitter') +
  ggtitle('Total SO2 and Quality Relationship')

# The analysis plot indicates again existence of higher total SO2 for the White wine sample as compared to Red wine
# The relationship of the Total SO2 with sulpahtes and residual sugar is analysed below:

b1<-ggplot(aes(x=total.SO2,y=density), data = wine) +
  geom_density(aes(color=type))

b2<-ggplot(aes(x=total.SO2,y=RS), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

grid.arrange(b1,b2,ncol=2)
# The observations from the above analysis is provided below:
#  density
# density level is quite high for the red wine as compared to white wine with a huge spike around 150 unit mark of total.sulfur.dioxide.
# For a higher total SO2 level of around 250, the density level of white wine is higher.
# White wine seems to exhibit a total SO2 level higher than 280 units
# Residual Sugar
# White wine exhibits high level of Residual sugar around 250 unit mark for Total SO2 as compared to Red wine and generally the quantity of Residual sugar seems to be on higher after the 150 unit level for Total SO2
# The relationship of Sulphate and Residual Sugar is analysed as below:


#The relationship of Sulphate and Residual Sugar is analysed as below:
c1<-ggplot(aes(x=free.SO2,y=sulphates), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

c2<-ggplot(aes(x=free.SO2,y=RS), data = wine) +
  geom_density(aes(color=type),stat='summary',fun.y=median)

grid.arrange(c1,c2,ncol=2)
#The analysis from the above plot is provided as below:
#Sulphate level is quite high for the red wine as compared to white wine. Red wines do not exhibit a Free SO2 level beyond 70 units
#Residual Sugar: white wine exhibits higher level of residual sugar and has peaks aroud 150 mark.


# A final comparison is done between the Red and White wine to understand the difference between the two variants 
# for the parameter of Total and Free SO2 and the PH values

s1<-ggplot(aes(x=pH,y=free.SO2), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median)

s2<-ggplot(aes(x=pH,y=total.SO2), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median)

grid.arrange(s1,s2,ncol=2)

# The above plot indicates that white wine does exhibit higher SO2 components as compared to Red Wine for 
# similar pH values across all pH values within the sample.There seems to be higher variation for both SO2 values in 
# both Red and White wines between a pH value of 3.5 and 4.0. 
# A closer look at these pH interval is given below

t1<-ggplot(aes(x=pH,y=free.SO2), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median) +
  xlim(3.5,4.0)

t2<-ggplot(aes(x=pH,y=total.SO2), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median) +
  xlim(3.5,4.0)

grid.arrange(t1,t2,ncol=2)

# The above analysis plot indicates a high peak for free SO2 (60 units) for a pH value of 3.65 while a high peak for 
# red wine for a pH value of 3.75 (41 units). In the case of Total SO2, the peak of around 180 units for white at a pH level 
# around 3.62 while Red wine exhibits a peak of around 105 units at a pH level of 3.85. Also, it is observed that only 
# red wines in the sample have a pH value beyond 3.85 and the Total and Free SO2 levels at this level is low.

ggplot(aes(x=ABV,y=free.SO2), data = wine) +
  geom_line(aes(color=type),stat='summary',fun.y=median) +
  ggtitle('Alcohol and Free SO2 relationship')

# The above plot indicates that for the same alcholo content, free SO2 is higher for white wine than 
# red wine and also the free SO2 decreases quite significantly with increase in the alcohol content
ggplot(aes(x = pH),data = wine) + 
  geom_density(aes(fill = type)) +
  facet_wrap(~quality) +
  ggtitle('pH values  relationship with Quality')

# From the above analysis plot,there doesn't seem to be any specific relations between pH values and quality in terms of the spread.
# However the Red wine tends to exhibit a higher pH value density then wine for quality rating till 7 while quality rating of 8 has more similar values of density. 
# The quality rating of 9 exhibits a more narrower spread for pH value between 3.1 and 3.6.

# Summary
# The analysis performed on the sample datasets can be summarized as below:
# pH value is considered an important parameter when determining the quality of wine.
# The analysis over the samples however indicate that there is no specific values of pH which provides bias for quality ratings, 
# and a higher density of red wine samples did indicate a higher pH values as compared to white wine samples for the same quality ratings.
# These pH value however was found to be optimum between a value of 3.0 and 3.5. A pH value of higher than 3.5 tends to exhibit higher SO2 values 
# which can be concern for people with concerns of health issues with SO2. 
# Samples with higher alcohol content did exhibit lower SO2 counts and also white wine samples exhibited a higher level of SO2 components as compared to red wine for the same level of Alcohol.
# Some learning from the analysis were as follows:
# The understanding that red wine generally exhibits more SO2 properties than white wine seems to be not true as per the samples considered. 
# The analysis proves that white wine exhibits a higher level of SO2 properties. 
# It always seemed that pH value was a key factor in determining the quality of the wines but from the analysis, 
# it seems that pH value do not exhibit any patterns which can be utilized as a key deterministic variable for wine quality testing by sensory analysis. 
# From the samples analyzed,the wines with higher Alcohol content exhibited lower SO2 content as compared with samples with lower Alcohol content. 
# For the buyer conscious of the sugar content in the wines, white wine exhibits more residual sugar
# and as we have seen spikes in the residual sugar for certain ranges of the Free and Total SO2 primarily with white wine. 
# A limitation of the current analysis is that the current data consists of samples collected from a specific region of Portugal. 
# It will be interesting to obtain datasets across various wine making regions to eliminate any bias created by any specific qualities of the product

