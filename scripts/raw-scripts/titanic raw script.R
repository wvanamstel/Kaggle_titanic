source('./titanic-functions.R')
require(ggplot2)
require(caret)
require(rattle)
require(Hmisc)
#######################################################################
### Read the data
#######################################################################
column.types <- c('integer', #passengerID
                 'factor', #Survived?
                 'factor', #class travelled
                 'character', #name of passenger
                 'factor', #sex
                 'numeric', #age
                 'integer', #number of siblings or spouse
                 'integer', #number of parents/children
                 'character', #ticket number
                 'numeric', #fare paid
                 'character', #cabin number
                 'factor' #port of embarcation
                 )
missing.types <- c("NA","")
orig_train <- readData('../../data/raw-data/train.csv', column.types, missing.types)
column.types <- column.types[-2]
orig_test <- readData('../../data/raw-data/test.csv', column.types, missing.types)

#######################################################################
### examine the data
#######################################################################
ti_train <- orig_train
names(ti_train)
str(ti_train)
head(ti_train)
table(df.train$Pclass)   #examine in which class
table(df.train$Survived)
barplot(table(df.train$Survived, df.train$Age))
hist(df.train$Age, breaks = 20)
prop.table(table(df.train$Survived, df.train$Sex))   #### proportion table

plot(density(ti_train$Age, na.rm=TRUE))
plot(density(ti_train$Fare, na.rm=TRUE))
count_sex <-table(ti_train$Survived, ti_train$Sex)
count_age <- table(ti_train$Survived, ti_train$Age)
count_sex[2]/(count_sex[1] + count_sex[2])  #percent women survivers
count_sex[4]/(count_sex[3] + count_sex[4])  #percent men

df.train <- ti_train
df.test <- ti_test
require(Amelia)    #plot missing values in data set
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

#featureplot, not too informative
featurePlot(x=ti_train[,c("Pclass", "Sex", "Age")], 
            y=ti_train$Survived, plot="pairs")

#young psng survive, higher fare means better chances? looks like it
qplot(Fare, Age, colour=Survived, data=ti_train)
cutAge <- cut(ti_train$Age, breaks=3)
qplot(cutAge, Fare, data=ti_train, fill=cutAge, geom=c("boxplot"))
table(cutAge, ti_train$Survived)
#density plots
qplot(Age, colour=Survived, data=ti_train, geom=c("density"))
qplot(Age, colour=Pclass, data=ti_train, geom=c("density"))

#######################################################################
### dealing with missing data points
#######################################################################
#only 2 missing values in the embarked feature
ti_train$Embarked[which(is.na(ti_train$Embarked))] <- 'S' 

#impute the missing ages
summary(ti_train$Age)
#replace missing values by average age
# accuracy is 0.7857 in RF, slightly beter than without impute, but no
#improvement on the test data
ti_train$Age[which(is.na(ti_train$Age))] <- mean(ti_train$Age, na.rm=TRUE)

#based on Class (see density plot), no dramatic improvement
age_class1 <- mean(ti_train$Age[which(ti_train$Pclass==1)], na.rm=TRUE)
age_class2 <- mean(ti_train$Age[which(ti_train$Pclass==2)], na.rm=TRUE)
age_class3 <- mean(ti_train$Age[which(ti_train$Pclass==3)], na.rm=TRUE)
ti_train$Age[which(ti_train$Pclass==1 & is.na(ti_train$Age))] <- age_class1
ti_train$Age[which(ti_train$Pclass==2 & is.na(ti_train$Age))] <- age_class2
ti_train$Age[which(ti_train$Pclass==3 & is.na(ti_train$Age))] <- age_class3

#try to use titles as an indication of age
ti_train$Title <- getTitle(ti_train)
require(Hmisc)
bystats(ti_train$Age, ti_train$Title,
        fun=function(x) c(Mean=mean(x), Median=median(x)))

missing_titles <- c('Dr', 'Master', 'Miss', 'Mr', 'Mrs')

#replace missing ages with median age based on title
ti_train$Age <- imputeMedian(ti_train$Age, ti_train$Title, missing_titles)

#investigate fare feature
#replace 0 fares for median fare paid in applicable class
ti_train$Fare[which(ti_train$Fare == 0)] <- NA
ti_train$Fare <- imputeMedian(ti_train$Fare, ti_train$Pclass, 
                              as.numeric(levels(ti_train$Pclass)))

#investigate the title feature a little more
ti_train$Title <- factor(ti_train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(ti_train$Age ~ ti_train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

#group the titles
ti_train$Title <- changeTitles(ti_train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir", "the Countess"),
                               "Noble")
ti_train$Title <- changeTitles(ti_train, c("Mlle", "Ms"), "Miss")
ti_train$Title <- changeTitles(ti_train, c("Mme"), "Mrs")
ti_train$Title <- as.factor(ti_train$Title)

#more feature engineering?
ti_train$Surname <- getSurname(ti_train)
ti_train$Surname <- as.factor(ti_train$Surname)

temp <- ti_train[with(ti_train, order(ti_train$Surname)),]

ti_train <- extraFeatures(ti_train)

features_to_use <- c("Survived", "Sex", "Priority", "Age", "Title", "Pclass",
                     "Deck", "Side", "Fare", "Fare_pp", "Embarked", "Family")

ti_train_mung <- ti_train[features_to_use]

#######################################################################
### Simple regression tree
#######################################################################
set.seed(23)
in_train <- createDataPartition(y=ti_train_mung$Survived, p=0.75, list=FALSE)
training <- ti_train_mung[in_train,]
vali <- ti_train_mung[-in_train,]

##fit regression tree model
mod_rpart <- train(Survived ~ ., method='rpart', data=training)
print(mod_rpart$finalModel)
require(rattle)
fancyRpartPlot(mod_rpart$finalModel)
#something odd going on?
pred_rpart <- predict(mod_rpart, newdata=vali)
vali$rpart_right <- pred_rpart == vali$Survived
table(pred_rpart, vali$Fate)
confusionMatrix(pred_rpart, vali$Survived)
## accuracy = 0.7782 only sex feature is used

##using plain random forest
require(doMC)
registerDoMC(2)
mod_rf <- train(Survived ~ ., method='rf', data=training)
pred_rf <- predict(mod_rf, newdata=vali)
confusionMatrix(pred_rf, vali$Survived)

#######################################################################
### Submissions
#######################################################################
#######################################################################

#######################################################################
### Munge test data
#######################################################################
ti_test <- orig_test
ti_test$Title <- getTitle(ti_test)

missing_titles <- c('Master', 'Miss', 'Mr', 'Mrs')
#replace missing ages with median age based on title
ti_test$Title <- changeTitles(ti_test, c("Ms"), "Miss")
ti_test$Age <- imputeMedian(ti_test$Age, ti_test$Title, missing_titles)
#replace 'Ms' title with median age from training set, only 1 Ms data point in test

#investigate fare feature
#replace 0 fares for median fare paid in applicable class
ti_test$Fare[which(ti_test$Fare == 0)] <- NA
ti_test$Fare <- imputeMedian(ti_test$Fare, ti_test$Pclass, 
                              as.numeric(levels(ti_test$Pclass)))

#Group titles
ti_test$Title <- changeTitles(ti_test, c("Col", "Dona", "Dr", "Rev"),"Noble")
ti_test$Title <- as.factor(ti_test$Title)

#more feature engineering?
ti_test$Surname <- getSurname(ti_test)
ti_test$Surname <- as.factor(ti_test$Surname)

ti_test <- extraFeatures(ti_test)

features_to_use <- c("PassengerId", "Sex", "Priority", "Age", "Title", "Pclass",
                     "Deck", "Side", "Fare", "Fare_pp", "Embarked", "Family")

ti_test <- ti_test[features_to_use]

#######################################################################
### Run model on test data
#######################################################################
pred_test <- predict(mod_rf, newdata=ti_test)
Submission(ti_test$PassengerId, pred_test, 'rfmunged')
