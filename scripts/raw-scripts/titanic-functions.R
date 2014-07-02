## READ DATA FUNCTION
readData <- function(file.name, column.types, missing.types){
  read.csv(file.name, na.strings = missing.types, colClasses = column.types)
}

## EXTRACT TITLE FROM NAME ENTRY
getTitle <- function(data) {
  title_start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title_end <- title_start + attr(title_start, "match.length")-1
  data$Title <- substr(data$Name, title_start+2, title_end-1)
  return (data$Title)
}   

## IMPUTE MEDIAN VALUES IN FEATURE FOR MISSING DATA POINTS
imputeMedian <- function(impute_var, filter_var, var_levels) {
  for (v in var_levels) {
    impute_var[which(filter_var == v)] <- impute(impute_var[ 
      which(filter_var == v)])
  }
  return (impute_var)
}

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old_titles, new_title) {
  for (honorific in old_titles) {
    data$Title[ which( data$Title == honorific)] <- new_title
  }
  return (data$Title)
}

##extract surname, could be used to fill in missing cabin/ticket number
getSurname <- function(data){
  require(plyr)
  require(stringr)
  surname_end <- regexpr("\\, [A-Z]{1,20}", data$Name, TRUE)
  data$Surname <- substr(data$Name, start=1, stop=surname_end-1)
  return(data$Surname)
}

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

extraFeatures <- function(data){
  require(plyr)
  require(stringr)
  data$Fate <- data$Survived
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Dead"))
  #women and children first in lifeboats
  data$Priority <- "No"
  data$Priority[which(data$Sex == "female" | data$Age < 16)] <- "Yes"
  data$Priority <- as.factor(data$Priority)
  #family = spouses/siblings + children
  data$Family <- data$SibSp + data$Parch
  #average fare
  data$Fare_pp <- data$Fare/(data$Family + 1)
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## WRITE SUBMISSION FILE
Submission <- function(passengerID, fate, filename){
  submit<- data.frame(PassengerId = passengerID, Survived = fate)
  target <- paste0("../../data/processed-data/", filename)
  write.csv(submit, file = target, row.names=FALSE)
}