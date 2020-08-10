# Loading default Libraries

library("ggplot2")
library("datasets")
library("graphics")
library("grDevices")
library("methods")
library("stats")
library("utils")

# Loading data

train <- read.csv("/Users/sabbirhassan/Dropbox/ML_stuff/titanic/train.csv", header = TRUE)
test <- read.csv("/Users/sabbirhassan/Dropbox/ML_stuff/titanic/test.csv", header = TRUE)

#train <- read.csv("D:\\Dropbox\\ML_stuff\\titanic\\train.csv", header = TRUE)
#test <- read.csv("D:\\Dropbox\\ML_stuff\\titanic\\test.csv", header = TRUE)

# Adding Survived empty valued column in test set

test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])

# Combining the two data sets

data.combined <- rbind(train, test.survived)

#look at the datatypes

str(data.combined)

# Pclass seems like int but actually its an factor (or categorical) data
# Survived also seems char type but its supposed to be a factortype

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Take a look at survival frequencies

table(data.combined$Survived)
table(data.combined$Pclass)

# But instead of tabular representation we visualize for more insight

train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
test$Pclass <- as.factor(test$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# This shows the society class does play a role in survival

head(as.character((train$Name)))

length(unique(as.character(data.combined$Name)))

# Lets find the duplicate names

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Now lets pullout this name and check their records

data.combined[which(data.combined$Name %in% dup.names),]

# So we can decide they are different people
# Now we see Miss , Mr, Mrs are inside the Names, So lets extract them out

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
summary(misses)
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
males <- data.combined[which(train$Sex == "male"),]


# Do something New with function
# extract title using a function

extractTitle <- function(Name) {
  name <- as.character(Name)
  if (length(grep("Miss.", name))>0) {
    return ("Miss.")
  } else if (length(grep("Master.", name))>0) {
    return("Master.")
  } else if (length(grep("Mrs.", name))>0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name))>0) {
    return("Mr.")
  } else {
    return("Other")
  }
  
}


titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  
}
data.combined$title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x = title, fill= factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("total count")+
  labs(fill = "Survived")


# look at sex
 table(data.combined$Sex)

 ggplot(data.combined[1:891,], aes(x = Sex, fill= factor(Survived))) +
   geom_bar(width = 0.5) +
   facet_wrap(~Pclass) +
   ggtitle("Pclass") +
   xlab("Sex") +
   ylab("Total Count")+
   labs(fill = "Survived")

# look at Age
summary(data.combined$Age) 
summary(data.combined[1:891,"Age"]) 
# we see a lot of missing values; so how to deal with them?
# what are the imputation methods? Gradient boosting trees: xgboost, lightgbm ???


ggplot(data.combined[1:891,], aes(x = Age, fill= factor(Survived))) +
  geom_bar(width = 10) +
  facet_wrap(~Sex+Pclass) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

# validate that master is a good proxy of male choldren
  
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

girls <- data.combined[which(data.combined$title == "Miss."),]
summary(girls$Age)

adultmales <- data.combined[which(data.combined$title == "Mr."),]
summary(adultmales$Age)

adultfemales <- data.combined[which(data.combined$title == "Mrs."),]
summary(adultfemales$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill= factor(Survived))) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count") 
  labs(fill = "Survived")

# looking closer into female misses
  
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# this insight is helpful for feature engineering

# look look into Sibsp variable
summary(data.combined$SibSp) # can we treat this as a categorical variable? as its 
#finite value, and make a dropdown list to select or sth.

length(unique(data.combined$SibSp))

# only 7, so we can turn it into factors

ggplot(data.combined[1:891,], aes(x = SibSp, fill= factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x = as.factor(Parch), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Create a family size feature

temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family <- as.factor(temp.Parch+temp.SibSp+1)

#Now lets look into this new feature

ggplot(data.combined[1:891,], aes(x = as.factor(family), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Looking into tickets and fares



str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)

# lets start looking at just the frst character for each

Ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket,1,1))
unique(Ticket.first.char) #we can make it a factor

data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

ggplot(data.combined[1:891,], aes(x = as.factor(Ticket.first.char), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Ticket First Char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = as.factor(Ticket.first.char), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  #facet_wrap(~Pclass + title) +
  #ggtitle("Pclass, title") +
  xlab("Ticket First Char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

# there probably very strng correlation betn class and fare price. it's obvious, but lets take a look at it
str(data.combined$Fare)
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined[1:891,], aes(x = Fare, fill= as.factor(Survived))) +
  geom_bar(width = 5, position = "stack") +
  #facet_wrap(~Pclass + title) +
  ggtitle("Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

#probably fare doesnt affet much?

#analysis in caboin variable

str(data.combined$Cabin)
length(unique(as.character(data.combined$Cabin)))

# random forest is looking to be useful here, but default factor levels RF can use is 32 

summary(data.combined$Cabin)
# seems that some blank maybe there
data.combined$Cabin[1:100]

#replace empty cabin with U
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined[(as.character(data.combined$Cabin) == ""),"Cabin"] <-"U"

#from cabin names we can see that the frst char might be importatn

Cabin.first.char <- substr(data.combined$Cabin,1,1)
str(Cabin.first.char)
unique(Cabin.first.char) #we can make it a factor

data.combined$Cabin.first.char <- as.factor(Cabin.first.char)

ggplot(data.combined[1:891,], aes(x = as.factor(Cabin.first.char), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Cabin First Char") +
  ylab("Total Count") +
  ylim(0,700) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = as.factor(Cabin.first.char), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  #facet_wrap(~Pclass + title) +
  #ggtitle("Pclass, title") +
  xlab("Cabin First Char") +
  ylab("Total Count") +
  ylim(0,700) +
  labs(fill = "Survived")

#seemes like strong correlationwit Pclass, so no new info, not thathelpful !!

# what about ppl with multiple cabins

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "),"Y","N"))

ggplot(data.combined[1:891,], aes(x = as.factor(Cabin.multiple), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") +
  xlab("Cabin multiple") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

#seems not that important , even though RF algorithm will tell us later

# checking Embraked column

ggplot(data.combined[1:891,], aes(x = as.factor(Embarked), fill= as.factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass + title, nrow = 3, ncol = 5) +
  ggtitle("Pclass, title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,100) +
  labs(fill = "Survived")

# So exploratory analysis tells us that some variables asre important and some can be replaced by our 
#derived features, but what evidence can we use to rule a column out?

#Now how we can get the idea of feature importance? some models do it implicitly : RF
#sometimes we need to do PCA to know importance.


#Lets Do Random forest and then upgrade to Xgboost later . (also show that Logistic is not good)
#what about neural network for categorical data

#install.packages("randomForest")
library(randomForest)

#tarin a random forest with default parameters

#lets check all the vars

# rf.train.1 <- data.combined[1:891,c("Pclass","Sex","SibSp","Parch","Fare","Embarked","title","family","Ticket.first.char","Cabin.first.char","Cabin.multiple")] #taking just two important cols that we think
# #we drop Age due to missing values
# rf.label <- as.factor(train$Survived)
# 
# set.seed(1) #seting seed to verify a random run
# rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
# 
# rf.1 #just this output gives some info esp the confusion matrix and oob (out of the bag)
# importance(rf.1) #getting importance values
# varImpPlot(rf.1)

#taking the frst 4 variables

rf.train.2 <- data.combined[1:891,c("title","family","Ticket.first.char","Cabin.first.char")] #taking just two important cols that we think
#we drop Age due to missing values
rf.label <- as.factor(train$Survived)

set.seed(1) #seting seed to verify a random run
rf.2 <- randomForest(x=rf.train.2, y=rf.label, importance = TRUE, ntree = 1000)

rf.2 #just this output gives some info esp the confusion matrix and oob (out of the bag)
importance(rf.2) #getting importance values
varImpPlot(rf.2)

#Making predictions
# Ommiting NA and predicting
test.submit.df <- data.combined[892:1309, c("title","family","Ticket.first.char","Cabin.first.char")]
rf.2.pred <- predict(rf.2, test.submit.df)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.2.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190205_1.csv", row.names = FALSE)

#installed.packages(caret)
library(caret)  # good for Crossvalidation process; but its really expensive in terms of computaitons
#install.packages(doSNOW)
library(doSNOW)

# default cross validation settings is 10-fold cross validation repeated 10 times is standard
# caret works parallely and uses cores of the PC.
#doSNOW works on both MAc and Windows using parallelizinf the work for caret

# so we need 10 * 10 = 100 fols that are random.

# 
# 
# set.seed(5)
# cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10) #creates bunch of indexes
# 
# #check stratification , caret can do it for you.
# #its usefull for small amount of imbalancing
# 
# table(rf.label)
# 342/549
# 
# table(rf.label[cv.10.folds[[32]]])
# 308/494
# 
# #seems like the ratio is okay
# library("e1071")
# cntrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
#                         index = cv.10.folds)
# # setting up doSNOW package for multi-core training; doMC only works on Linux
# cl <- makeCluster(4, type = "SOCK")
# registerDoSNOW(cl)
# 
# set.seed(10)
# rf.2.cv.1 <- train(x = rf.train.2, y = rf.label, method = "rf", tuneLength = 3,
#                    ntree = 1000, trControl = cntrl.1)
# 
# #Shut down cluster
# stopCluster(cl)
# 
# #checkout results
# 
# rf.2.cv.1
# 
# #maybe 10 folds overfit... what happens if we do 5 fold?
# set.seed(15)
# cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10) #creates bunch of indexes
# 
# #check stratification , caret can do it for you.
# #its usefull for small amount of imbalancing
# 
# 
# cntrl.2 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
#                         index = cv.5.folds)
# # setting up doSNOW package for multi-core training; doMC only works on Linux
# cl <- makeCluster(4, type = "SOCK")
# registerDoSNOW(cl)
# 
# set.seed(20)
# rf.2.cv.2 <- train(x = rf.train.2, y = rf.label, method = "rf", tuneLength = 3,
#                    ntree = 1000, trControl = cntrl.2)
# 
# #Shut down cluster
# stopCluster(cl)
# 
# #checkout results
# 
# rf.2.cv.2

# 
set.seed(25)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10) #creates bunch of indexes

#check stratification , caret can do it for you.
#its usefull for small amount of imbalancing


cntrl.3 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                        index = cv.3.folds)
# setting up doSNOW package for multi-core training; doMC only works on Linux
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

set.seed(30)
rf.2.cv.3 <- train(x = rf.train.2, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = cntrl.3) #also reducing the trees

#Shut down cluster
stopCluster(cl)

#checkout results

rf.2.cv.3

#so this 3-fold maybe more generalized and better.
test.submit.df <- data.combined[892:1309, c("title","family","Ticket.first.char","Cabin.first.char")]
rf.2.cv.3.pred <- predict(rf.2.cv.3, test.submit.df)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.2.cv.3.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190205_2.csv", row.names = FALSE)

# a small improvement in Kaggle  0.6 %
# the link has a basic tutorial using caret
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/



# Let's look into the details of a decision tree to understand 

# install.packages("rpart")
# install.packages("rpart.plot")
# library("rpart")
# library("rpart.plot")

# LEts update the titles and look at it more closely


# Parse out last name and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1) # paralell and vectorize applies a function on all elements
last.names[1:10] # "[" is the indexing element


# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'?
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$New.title <- as.factor(titles)

# Visualize new version of title
ggplot(data.combined[1:891,], aes(x = New.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) + 
  ggtitle("Surival Rates for new.title by pclass")



# Collapse titles based on visual analysis
indexes <- which(data.combined$New.title == "Lady.")
data.combined$New.title[indexes] <- "Mrs."

indexes <- which(data.combined$New.title == "Dr." | 
                   data.combined$New.title == "Rev." |
                   data.combined$New.title == "Sir." |
                   data.combined$New.title == "Officer")
data.combined$New.title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = New.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")

# Grab features
features <- c("Pclass", "New.title","family","Ticket.first.char","Cabin.first.char")



# Dive in on 1st class Mr."
indexes.first.mr <- which(data.combined$New.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female? so this Dr is a woman
first.mr.df[first.mr.df$Sex == "female",]

# Update new.title feature
indexes <- which(data.combined$New.title == "Mr." & 
                   data.combined$Sex == "female")
data.combined$New.title[indexes] <- "Mrs."

# Any other gender slip ups?
length(which(data.combined$Sex == "female" & 
               (data.combined$New.title == "Master." |
                  data.combined$New.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$New.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Ssurvived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])



# Take a look at some of the high fares


# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
# getting per person avg fare. instead of whole pclass fare

ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")




# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# One missing value, take a look
data.combined[is.na(data.combined$avg.fare), ] 

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & title == "Mr." & family == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840


#summary(similar.na.passengers$Age) will give you NA in age. and its a lot.
#it requires a lot of imputation. so we ignore it now


# normalizing data is very important to esp for models like support vector machine and others
# Leverage caret's preProcess function to normalize data using z-score

preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data; seesm like uncorrelated so we can use these as features

cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again


# OK, let's see if our feature engineering has made any difference
#including previous plus the new and checking the importance

features <- c("Pclass", "New.title","family","Ticket.first.char","Cabin.first.char", "ticket.party.size", "avg.fare")
rf.train.3 <- data.combined[1:891, features]
rf.label <- as.factor(train$Survived)

set.seed(40) #seting seed to verify a random run
rf.3 <- randomForest(x=rf.train.3, y=rf.label, importance = TRUE, ntree = 1000)

rf.3 #just this output gives some info esp the confusion matrix and oob (out of the bag)
importance(rf.3) #getting importance values
varImpPlot(rf.3)

test.submit.df <- data.combined[892:1309, features]
rf.3.pred <- predict(rf.3, test.submit.df)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.3.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_03.csv", row.names = FALSE)


#lets reduce the features and check

features <- c("Pclass", "New.title","family","ticket.party.size", "avg.fare")
rf.train.4 <- data.combined[1:891, features]
rf.label <- as.factor(train$Survived)

set.seed(45) #seting seed to verify a random run
rf.4 <- randomForest(x=rf.train.4, y=rf.label, importance = TRUE, ntree = 1000)

rf.4 #just this output gives some info esp the confusion matrix and oob (out of the bag)
importance(rf.4) #getting importance values
varImpPlot(rf.4)

test.submit.df <- data.combined[892:1309, features]
rf.4.pred <- predict(rf.4, test.submit.df)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.4.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_04.csv", row.names = FALSE)

#seems like the less features inceased the test scores




#Lets try crossvalidation 3 fold and try

set.seed(50)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10) #creates bunch of indexes

cntrl.3 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                        index = cv.3.folds)
# setting up doSNOW package for multi-core training; doMC only works on Linux
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

set.seed(55)
rf.5.cv.3 <- train(x = rf.train.4, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = cntrl.3) #also reducing the trees

#Shut down cluster
stopCluster(cl)

#checkout results

rf.5.cv.3

#so this 3-fold maybe more generalized and better.
test.submit.df <- data.combined[892:1309, features]
rf.5.cv.3.pred <- predict(rf.5.cv.3, test.submit.df)


submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.5.cv.3.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_05.csv", row.names = FALSE)


#some how CV 3 fold doesnt work well. rather just RF is working well
#lets see by CV 10


set.seed(60)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10) #creates bunch of indexes

#check stratification , caret can do it for you.
#its usefull for small amount of imbalancing


cntrl.10 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                        index = cv.10.folds)
# setting up doSNOW package for multi-core training; doMC only works on Linux
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

set.seed(65)
rf.5.cv.10 <- train(x = rf.train.4, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = cntrl.10) #also reducing the trees

#Shut down cluster
stopCluster(cl)

#checkout results
rf.5.cv.10

test.submit.df <- data.combined[892:1309, features]
rf.5.cv.10.pred <- predict(rf.5.cv.10, test.submit.df)


submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.5.cv.10.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_06.csv", row.names = FALSE)



#using random forest package instead of package

features <- c("Pclass", "New.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_07.csv", row.names = FALSE)


# feature analysis very important 


# First, let's explore our collection of features using mutual information to
# gain some additional insight. Our intuition is that the plot of our tree
# should align well to the definition of mutual information.
#install.packages("infotheo")

#using mutual information / entropy for feature selection : use stanford link

library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family[1:891])
mutinformation(rf.label, data.combined$Ticket.first.char[1:891])
#mutinformation(rf.label, data.combined$Age[1:891])
mutinformation(rf.label, data.combined$New.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))



# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than 'Mr."
#install.packages("Rtsne")

# Also keep dimension reduction in mind. not really needed in trees, but other algorithms might need it.
# PCA, ICA can generate the correspoonding reduced important features. 

library #dimensionality reduction library

most.correct <- data.combined[data.combined$New.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")


# NOTE - Bug fix for original version. Rtsne needs a seed to ensure consistent
# output between runs.
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")


# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.

condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass

condinformation(rf.label, data.combined[1:891, c("New.title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$New.title == "Mr.",]
indexes <- which(misters$Survived != "None")

set.seed(98437)
tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")

# very poor for males misters
# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?

set.seed(987)
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building

data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]


# ----------------------------- #
# we did visualization in training only but we use tsne for both train and test

features <- c("Pclass", "New.title","family","ticket.party.size", "avg.fare","tsne.x","tsne.y")
rf.train.7 <- data.combined[1:891, features]
rf.label <- as.factor(train$Survived)

set.seed(450) #seting seed to verify a random run
rf.7 <- randomForest(x=rf.train.7, y=rf.label, importance = TRUE, ntree = 1000)

rf.7 #just this output gives some info esp the confusion matrix and oob (out of the bag)
importance(rf.7) #getting importance values
varImpPlot(rf.7)

test.submit.df <- data.combined[892:1309, features]
rf.7.pred <- predict(rf.7, test.submit.df)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived = rf.7.pred)
write.csv(submit.df,file = "/Users/sabbirhassan/Dropbox/ML_stuff/titanic/RF_SUB_20190207_08.csv", row.names = FALSE)


