library(ggplot2)
library(rpart)
library(randomForest)

#Read in necessary data
data <- read.csv("train.csv")
test <- data.frame(read.csv("test.csv"))
train <- data.frame(data)

#Analysis, seeing how the features of train correlate with surival
genderSurvive <- prop.table(table(train$Survived, train$Sex),1)
classSurvive <- prop.table(table(train$Survived, train$Pclass),1)
farePlot <- ggplot(train,aes(Age,Fare)) + geom_point() + geom_point(aes(colour = factor(Survived)))
sibSpSurvive <- prop.table(table(train$Survived,train$SibSp))
ParchSurvive <- prop.table(table(train$Survived,train$Parch))

#Create column to represent family size
train$familySize <- train$SibSp + train$Parch + 1
test$familySize <- test$SibSp + test$Parch + 1

#Fix blank embarked cells to where most people left from
train$Embarked[is.na(train$Embarked == TRUE)] <- "S"
test$Embarked[is.na(test$Embarked) == TRUE] <- "S"


#making a list of all possible titles for later use
titleDict <- c("Mr.", "Mrs.", "Miss.", "Master."
               , "Rev.", "Dr.", "Major.", "Mme."
               , "Lady.", "Sir.")

#Creating title columns based on binary variable if title is contained within Name column

train$Mr[grepl(titleDict[1], train$Name, fixed = TRUE) == TRUE] <- 1
train$Mr[is.na(train$Mr) == TRUE] <- 0

train$Mrs[grepl(titleDict[2], train$Name, fixed = TRUE) == TRUE] <- 1
train$Mrs[is.na(train$Mrs) == TRUE] <- 0

train$Miss[grepl(titleDict[3], train$Name, fixed = TRUE) == TRUE] <- 1
train$Miss[is.na(train$Miss) == TRUE] <- 0

train$Master[grepl(titleDict[4], train$Name, fixed = TRUE) == TRUE] <- 1
train$Master[is.na(train$Master) == TRUE] <- 0

train$Rev[grepl(titleDict[5], train$Name, fixed = TRUE) == TRUE] <- 1
train$Rev[is.na(train$Rev) == TRUE] <- 0

train$Dr[grepl(titleDict[6], train$Name, fixed = TRUE) == TRUE] <- 1
train$Dr[is.na(train$Dr) == TRUE] <- 0

train$Major[grepl(titleDict[7], train$Name, fixed = TRUE) == TRUE] <- 1
train$Major[is.na(train$Major) == TRUE] <- 0

train$Mme[grepl(titleDict[8], train$Name, fixed = TRUE) == TRUE] <- 1
train$Mme[is.na(train$Mme) == TRUE] <- 0

train$Lady[grepl(titleDict[9], train$Name, fixed = TRUE) == TRUE] <- 1
train$Lady[is.na(train$Lady) == TRUE] <- 0

train$Sir[grepl(titleDict[10], train$Name, fixed = TRUE) == TRUE] <- 1
train$Sir[is.na(train$Sir) == TRUE] <- 0

test$Mr[grepl(titleDict[1], test$Name, fixed = TRUE) == TRUE] <- 1
test$Mr[is.na(test$Mr) == TRUE] <- 0

test$Mrs[grepl(titleDict[2], test$Name, fixed = TRUE) == TRUE] <- 1
test$Mrs[is.na(test$Mrs) == TRUE] <- 0

test$Miss[grepl(titleDict[3], test$Name, fixed = TRUE) == TRUE] <- 1
test$Miss[is.na(test$Miss) == TRUE] <- 0

test$Master[grepl(titleDict[4], test$Name, fixed = TRUE) == TRUE] <- 1
test$Master[is.na(test$Master) == TRUE] <- 0

test$Rev[grepl(titleDict[5], test$Name, fixed = TRUE) == TRUE] <- 1
test$Rev[is.na(test$Rev) == TRUE] <- 0

test$Dr[grepl(titleDict[6], test$Name, fixed = TRUE) == TRUE] <- 1
test$Dr[is.na(test$Dr) == TRUE] <- 0

test$Major[grepl(titleDict[7], test$Name, fixed = TRUE) == TRUE] <- 1
test$Major[is.na(test$Major) == TRUE] <- 0

test$Mme[grepl(titleDict[8], test$Name, fixed = TRUE) == TRUE] <- 1
test$Mme[is.na(test$Mme) == TRUE] <- 0

test$Lady[grepl(titleDict[9], test$Name, fixed = TRUE) == TRUE] <- 1
test$Lady[is.na(test$Lady) == TRUE] <- 0

test$Sir[grepl(titleDict[10], test$Name, fixed = TRUE) == TRUE] <- 1
test$Sir[is.na(test$Sir) == TRUE] <- 0

#sort out ages, bit lazy
train$Age[is.na(train$Age) == TRUE] <- median(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age) == TRUE] <- median(test$Age, na.rm = TRUE)

test$Fare[is.na(test$Fare) == TRUE] <- mean(test$Fare, na.rm = TRUE)

#Training a Random Forest tree using data from train
tree2 <- randomForest(as.factor(Survived) ~ Age+Sex+Fare+Pclass+SibSp+ familySize+
                        Mr+ Mrs+ Miss+ Master+ Dr+ Rev+ Major+ Mme+ Lady+ Sir, train, importance = TRUE, ntree = 1000)

#Predicting survival of test data using the tree trained
my_prediction <- predict(tree2,newdata = test, type = "class")
my_solution <- data.frame(test$PassengerId, my_prediction2)
colnames(my_solution) <- c("PassengerId", "Survived")

