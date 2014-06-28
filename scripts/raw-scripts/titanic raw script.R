#######################################################################
### Read the data
#######################################################################
readData <- function(file.name, column.types, missing.types){
  read.csv(file.name, na.strings = missing.types, colClasses = column.types)
}

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
ti_train <- readData('../../data/raw-data/train.csv', column.types, missing.types)
column.types <- column.types[-2]
ti_test <- readData('../../data/raw-data/test.csv', column.types, missing.types)

#######################################################################
### examine the data
#######################################################################
require(ggplot2)
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

#######################################################################
### dealing with missing data points
#######################################################################
#only 2 missing values in the embarked feature
ti_train$Embarked[which(is.na(ti_train$Embarked))] <- 'S' 

#impute the missing ages based on titles
summary(ti_train$Age)


#######################################################################
### Simple regression tree
#######################################################################
require(caret)
set.seed(23)
in_train <- createDataPartition(y=ti_train$Survived, p=0.70, list=FALSE)
training <- ti_train[in_train,]
vali <- ti_train[-in_train,]

##fit regression tree model
mod_rpart <- train(Survived ~ SibSp + Sex, method='rpart', data=training)
print(mod_rpart$finalModel)
require(rattle)
fancyRpartPlot(mod_rpart$finalModel)
#something odd going on?
pred_rpart <- predict(mod_rpart, newdata=vali)
vali$rpart_right <- pred_rpart == vali$Survived
table(pred_rpart, vali$Survived)
confusionMatrix(pred_rpart, vali$Survived)
## accuracy = 0.7782 only sex feature is used

##using plain random forest
require(doMC)
registerDoMC(2)
tr_control <- trainControl(allowParallel=TRUE)
mod_rf <- train(Survived ~ Sex + Pclass, method='rf', data=training)
pred_rf <- predict(mod_rf, newdata=vali)
confusionMatrix(pred_rf, vali$Survived)

## apply model to test data
pred_rpart_test <- predict(mod_rpart, newdata=ti_test)
Submission(ti_test$PassengerId, pred_rpart_test, "simpletree.cvs")

#######################################################################
### Submissions
#######################################################################
Submission <- function(passengerID, fate, filename){
  submit<- data.frame(PassengerId = passengerID, Survived = fate)
  target <- paste0("../../data/processed-data/", filename)
  write.csv(submit, file = target, row.names=FALSE)
}

