### predictive team project
### random forest
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('rpart')        # fill out missing value


setwd("C:\\Users\\danny\\Desktop\\STAT 745\\Team project\\New data\\")
train <- read.csv("traindata1.csv", stringsAsFactors = F)
test <- read.csv("testdata1.csv", stringsAsFactors = F)

str(test)
str(train)

### combine test data and train data
data <- rbind(train,test)

### check missing values
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) sum(x==""))


## There are 418 NA in data(for test subset), and 2 missing values in Pclass and Embarked. Since we wanna do random forest on Pclass, Sex, Age, Title, Mother, Embarked  and FamilySize, so we want to fill in the missing values.

# Embarked missing

Embarked.miss <- which(data$Embarked=="")
data[Embarked.miss,]

library('dplyr')
# remove 2 rows with missing values to do the plot
embark_fare <- data %>% filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + 
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# fill out the missing value with C
data$Embarked[c(62, 830)] <- 'C'
data$Embarked <- factor(data$Embarked)

### check missing values
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) sum(x==""))


data$Sex <- factor(data$Sex)
data$Title <- factor(data$Title)
data$Pclass <- factor(data$Pclass)
data$Embarked <- factor(data$Embarked)
data$Age_group <- factor(data$Age_group)
data$Mother <- factor(data$Mother)



train <- data[1:891,]
test <- data[892:1309,]

# trainning model

sapply(train,function(x) sum(is.na(x)))
sapply(train,function(x) sum(x==""))

rf_model <- randomForest(factor(Survived) ~ Sex + Title + Fare+ Pclass  +  Embarked + Age_group  + FamilySize,data = train)


importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

pred.rf <- predict(rf_model, newdata=test)

write.table(pred.rf, "C:\\Users\\danny\\Desktop\\STAT 745\\my.txt", sep="\t")

