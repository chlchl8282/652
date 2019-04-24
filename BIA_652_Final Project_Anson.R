#### Name ####
# Name    : Team 2
# ID      : 10444564 
# Date    : 04/18/2019
# Comments: BIA652 Final Project R script 
rm(list = ls()) 

#### Data Preparation #### 
### Exploratory Data Analysis ###
df.train <- read.csv("D://googledrive/Stevens/2019 Spring Semester/10003 BIA652 Multivariate Analysis/BIA-652 Team Project/hmeq.csv", header = TRUE)
print(head(df.train))
print(str(df.train))
View(df.train)

### Check missing data ### 
install.packages('Amelia')
library(Amelia)
missmap(df.train, main = "Missing Map", col = c('yellow','black'), legend = FALSE ) #Yellow is missing
summary(df.train)
table(df.train.nomiss$BAD)

### Generate the dataset to ignore all rows with missing data
df.train.nomiss <- na.omit(df.train)
summary(df.train.nomiss)
missmap(df.train.nomiss, main = "Missing Map", col = c('yellow','black'), legend = FALSE )
View(df.train.nomiss)
str(df.train.nomiss)

### Explore the data ### 
library(ggplot2)
ggplot(df.train.nomiss, aes(BAD)) + geom_bar(aes(fill=factor(BAD)))
ggplot(df.train, aes(JOB)) + geom_bar(aes(fill=factor(JOB)))
ggplot(df.train.nomiss, aes(JOB)) + geom_bar(aes(fill=factor(JOB)))
ggplot(df.train.nomiss, aes(LOAN)) + geom_histogram(bins=20, alpha = 0.5, fill='blue', color = 'black')
ggplot(df.train, aes(MORTDUE)) + geom_histogram(bins=20, alpha = 0.5, fill='red')
ggplot(df.train, aes(YOJ)) + geom_histogram(bins=20, alpha = 0.5, fill = 'green')

#Boxplot between loan and JOB# 
pl <- ggplot(df.train.nomiss, aes(JOB,LOAN))
pl <- pl + geom_boxplot(aes(group=JOB, fill=factor(JOB), alpha =0.5))
pl + scale_y_continuous(breaks = seq(min(0),max(100000), by = 5000)) + theme_bw()

#Split the data set into training set and test set 
install.packages('caTools')
library(caTools)
set.seed(101)
split <- sample.split(df.train.nomiss$BAD, SplitRatio = 0.7)
final.train <- subset(df.train.nomiss, split == TRUE)
final.test <- subset(df.train.nomiss, split == FALSE)

##Building the model 
library(dplyr)
str(df.train.nomiss)
df.train.nomiss$NINQ <- factor(df.train.nomiss$NINQ )
df.train.nomiss$BAD <- factor(df.train.nomiss$BAD )

#logistic regression model 
final.log.model <- glm(BAD~. , family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)
fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClassError <- mean(fitted.results != final.test$BAD)
1 - misClassError
# confusion matrix
table(final.test$BAD, fitted.results)
summary(final.test$BAD)

