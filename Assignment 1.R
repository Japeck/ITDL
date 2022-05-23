#installed
#install.packages("readr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("e1071")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("ggplots")
#install.packages("caTools")

#libraries
library(readr)
library(stringr)
library(lubridate)
library(e1071)
library(dplyr)
library(caret)
library(ggplot2)
library(caTools)

#Task 1-------------------------------------------------------------------------

#reading csv and creating dataframe

appstore_games <- read.csv("appstore_games.csv")

#Task 2-------------------------------------------------------------------------

#removing columns URL and Name
appstore_games <- subset(appstore_games, select = -c(URL, Name))

#creating new binary column and removing Subtitle column
appstore_games$Recorded.Subtitle <- ifelse(appstore_games$Subtitle>0, 1, 0)
appstore_games <- subset(appstore_games, select = -c(Subtitle))

#divide text, make it into a new dataframe, and convert them into numeric values
IAP <- str_split(appstore_games$In.app.Purchases, ',', simplify=TRUE)
IAP.df <- as.data.frame(IAP)
IAP.df = data.frame(apply(IAP.df, 2, function(x) as.numeric(as.character(x))))

#count occurrences, then get min, max, sum and mean as new columns
appstore_games$IAP.Values <- apply(IAP.df>0, MARGIN=1, FUN=sum, na.rm=TRUE)

#Minimum IAP
appstore_games$Minimum.IAP <- ifelse(appstore_games$IAP.Values>0,
                                     apply(IAP.df, MARGIN=1, FUN=min,
                                           na.rm=TRUE), NA)
#Maximum IAP
appstore_games$Maximum.IAP <- ifelse(appstore_games$IAP.Values>0,
                                     apply(IAP.df, MARGIN=1,
                                           FUN=max, na.rm=TRUE), NA)
#Sum IAP
appstore_games$Sum.IAP <- ifelse(appstore_games$IAP.Values>0,
                                 apply(IAP.df, MARGIN=1, FUN=sum,
                                       na.rm=TRUE), NA)
#Mean IAP
appstore_games$Average.IAP <- ifelse(appstore_games$IAP.Values>0,
                                     round(apply(IAP.df, MARGIN=1, FUN=mean,
                                                 na.rm=TRUE), 2), NA)

#counting words by splitting string by spaces and removing the Description column
appstore_games$Description.Word.Count <- str_count(appstore_games$Description, '\\W+')
appstore_games = subset(appstore_games, select = -c(Description))

#creating a new dataframe of the developers frequencies
Developers <- table(appstore_games$Developer)
Developers.df <- as.data.frame(Developers)

#renaming the column titles
colnames(Developers.df) <- c('Developer', 'Developer.Game.Count')

#creating a new column with 2 categories
Developers.df$Developer.Category <- ifelse(Developers.df$Developer.Game.Count<4, 'Newbie', 'Professional')

#merging the new dataframe with the big dataframe and removing the Developer.Game.Count column
appstore_games = merge(appstore_games, Developers.df, by='Developer')
appstore_games = subset(appstore_games, select = -c(Developer.Game.Count))

#a Second Age Rating column to divide games into just 4+ and 9+
appstore_games$Second.Age.Rating <- ifelse(appstore_games$Age.Rating=='4+', '4+', '9+')

#new Number.of.Languages column divided into Single or Many and games with no Languages or categorized as single
appstore_games$Number.of.Languages <- ifelse(nchar(appstore_games$Languages)<=2, 'Single', 'Many')

#new Is.Available.In.English column divided into Yes or No
appstore_games$Is.Available.In.English <- { ifelse(lengths(strsplit(appstore_games$Languages,
                                                                    'EN'))==2 | startsWith(appstore_games$Languages,
                                                                                           'EN') | endsWith(appstore_games$Languages, 'EN'),
                                                   'Yes', 'No')
}

#removing Primary.Genre
appstore_games = subset(appstore_games, select = -c(Primary.Genre))

#creating new Number.of.Genres column
appstore_games$Number.of.Genres <- lengths(strsplit(appstore_games$Genres, ','))

#converting Original.Release.Date to date format and creating new Release.Month column
appstore_games$Original.Release.Date = as.Date(appstore_games$Original.Release.Date, "%d/%m/%Y")
appstore_games$Release.Month <- strftime(appstore_games$Original.Release.Date,"%m")

#converting Current.Version.Release.Date to date format and creating new Elapsed.Months column
appstore_games$Current.Version.Release.Date = as.Date(appstore_games$Current.Version.Release.Date, "%d/%m/%Y")
data_collection_date <- as.Date('2019-08-03')
appstore_games$Elapsed.Months <- interval(appstore_games$Current.Version.Release.Date, data_collection_date) %/% months(1) 

#creating new Game.Free column
appstore_games$Game.Free <- ifelse(appstore_games$IAP.Values==0, 1, 0)

#creating new Categorical.Rating.Count column by dividing User.Rating.Count into lower or higher than the median
appstore_games$Categorical.Rating.Count <- {  
  ifelse(appstore_games$User.Rating.Count<median(
    appstore_games$User.Rating.Count, na.rm=TRUE), 'Low', 'High')
}

#Task 3-------------------------------------------------------------------------
#Assign every NA for User Rating Count 5 rating counts
appstore_games$User.Rating.Count[is.na(appstore_games$User.Rating.Count)] <- 5

#Assign every NA for Average User Rating a 0 average
appstore_games$Average.User.Rating[is.na(appstore_games$Average.User.Rating)] <- 0

#Remove all rows with NA for Price and Size
appstore_games <- appstore_games[!is.na(appstore_games$Price),]
appstore_games <- appstore_games[!is.na(appstore_games$Size),]

#Store all empty language cells as NA so its easier to work with and change them to EN
appstore_games$Languages[appstore_games$Languages == ""] <- NA
appstore_games$Languages[is.na(appstore_games$Languages)] <- "EN"

#Convert data that should be categorical to categorical data
appstore_games$Average.User.Rating <- as.factor(appstore_games$Average.User.Rating)
appstore_games$Recorded.Subtitle <- as.factor(appstore_games$Recorded.Subtitle)
appstore_games$Price <- as.factor(appstore_games$Price)
appstore_games$Game.Free <- as.factor(appstore_games$Game.Free)
appstore_games$Developer <- as.factor(appstore_games$Developer)
appstore_games$Developer.Category <- as.factor(appstore_games$Developer.Category)
appstore_games$Second.Age.Rating <- as.factor(appstore_games$Second.Age.Rating)
appstore_games$Number.of.Languages <- as.factor(appstore_games$Number.of.Languages)
appstore_games$Is.Available.In.English <- as.factor(appstore_games$Is.Available.In.English)
appstore_games$Release.Month <- as.factor(appstore_games$Release.Month)

#Task 4-------------------------------------------------------------------------
#Create data frame with bottom and top 200 user ratings
appstore_games.order.user.ratings <- appstore_games
appstore_games.order.user.ratings <- appstore_games.order.user.ratings[order(appstore_games.order.user.ratings$Average.User.Rating),]
appstore_games.ext.user.ratings <- appstore_games.order.user.ratings
appstore_games.ext.user.ratings <- appstore_games.ext.user.ratings[0:200,]
appstore_games.ext.user.ratings <- rbind(appstore_games.ext.user.ratings, tail(appstore_games.order.user.ratings, 200))

for (i in 1:nrow(appstore_games.ext.user.ratings)) {
  myurl <- paste(appstore_games.ext.user.ratings[i,3], sep = "")
  z <- tempfile()
  download.file(myurl, z, mode = "Wb", cache0K = F)
  pic <- readJPEG
  writeJPEG(pic, paste("image", "i", ".jpg", sep = ""))
  file.remove(z)
}

#Task 5-------------------------------------------------------------------------

#Creating the training observations
set.seed(1234)
nObservations <- nrow(appstore_games)
ptraining <- 0.8
ntraining <- round(ptraining * nObservations)

obs_training <-  sample(1:nObservations, ntraining, replace = FALSE)

#Training and test data set with user.rating.count------------------------------

appstore_games_sub <- { subset(appstore_games, select = 
                                 -c(ID, In.app.Purchases, Age.Rating, Genres, 
                                    Languages, Original.Release.Date, 
                                    Current.Version.Release.Date))
}

train_appstore_games <- appstore_games_sub[obs_training,]
test_appstore_games <- appstore_games_sub[-obs_training,]

#Checking distribution
summary(train_appstore_games)
class(train_appstore_games)
head(train_appstore_games)
summary(test_appstore_games)
class(test_appstore_games)
head(test_appstore_games)


#training and test data set without user.rating.count---------------------------

appstore_games_removed_user_count <- { subset(appstore_games, select = 
                                                -c(User.Rating.Count, ID, 
                                                   In.app.Purchases, Age.Rating, 
                                                   Genres, Languages, 
                                                   Original.Release.Date, 
                                                   Current.Version.Release.Date))
}

train_appstore_games_removed_user_count <- appstore_games_removed_user_count[obs_training,]
test_appstore_games_removed_user_count <- appstore_games_removed_user_count[-obs_training,]

#Checking distribution
summary(train_appstore_games_removed_user_count)
class(train_appstore_games_removed_user_count)
head(train_appstore_games_removed_user_count)
summary(test_appstore_games_removed_user_count)
class(test_appstore_games_removed_user_count)
head(test_appstore_games_removed_user_count)

#Task 6-------------------------------------------------------------------------

#Standardisation and normalisation of train_appstore_games----------------------

#Making the attributes the correct class so they can be scaled if necessary
train_appstore_games$User.Rating.Count <- as.numeric(train_appstore_games$User.Rating.Count)
train_appstore_games$Description.Word.Count <- as.numeric(train_appstore_games$Description.Word.Count)

# parameters of Standardisation and normalisation of train_appstore_games
standardize_params_appstore_games <- preProcess(train_appstore_games, method = c("center", "scale"))
normalize_params_appstore_games <- preProcess(train_appstore_games, method = c("range"))

#apply train scaling to appstore_games training data
scaled_train_appstore_games_standardisation <- predict(standardize_params_appstore_games, train_appstore_games)
scaled_train_appstore_games_normalisations <- predict(normalize_params_appstore_games, train_appstore_games)

#apply train scaling to appstore_games test data 
scaled_test_appstore_games_standardisation <- predict(standardize_params_appstore_games, test_appstore_games)
scaled_test_appstore_games_normalisations <- predict(normalize_params_appstore_games, test_appstore_games)


#Standardisation and normalisation of train_appstore_games_removed_user_Count----

#Making the attributes the correct class so they can be scaled if necessary
train_appstore_games_removed_user_count$Description.Word.Count <-
  as.numeric(train_appstore_games_removed_user_count$Description.Word.Count)

#parameters of Standardisation and normalisation of train_appstore_games_removed_user_Count
standardize_params_appstore_games_removed_user_count <-
  preProcess(train_appstore_games_removed_user_count, method = c("center", "scale"))
normalize_params_appstore_games_removed_user_count <-
  preProcess(train_appstore_games_removed_user_count, method = c("range"))

# appl train scaling to appstore_games_removed_user_count train data
scaled_train_appstore_games_removed_user_Count_standardisation <- 
  predict(standardize_params_appstore_games_removed_user_count, train_appstore_games_removed_user_count)
scaled_train_appstore_games_removed_user_Count_normalisation <-
  predict(normalize_params_appstore_games_removed_user_count, train_appstore_games_removed_user_count)

#apply train scaling to appstore_games_removed_user_count test data
scaled_test_appstore_games_removed_user_Count_standardisation <- 
  predict(standardize_params_appstore_games_removed_user_count, test_appstore_games_removed_user_count)
scaled_test_appstore_games_removed_user_Count_normalisation <-
  predict(normalize_params_appstore_games_removed_user_count, test_appstore_games_removed_user_count)

#Task 7-------------------------------------------------------------------------

#Naive Bayes--------------------------------------------------------------------

#model training
train_NB <- naiveBayes(Categorical.Rating.Count ~.,
                       data = scaled_train_appstore_games_removed_user_Count_standardisation)

#model testing
pre_NB <- predict(train_NB, scaled_test_appstore_games_removed_user_Count_standardisation)

#model results
rslt_NB <- table(predicted = pre_NB, 
                 Observed = scaled_test_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count)

CM_NB <- confusionMatrix(rslt_NB)
CM_NB

#The Accuracy is 0.6927
#The sensitivity is 0.5232
#The Specificity is 0.8523
#
#

#GKNN---------------------------------------------------------------------------

#model training
set.seed(1234)

train_GKNN <- { gknn(Categorical.Rating.Count ~ Developer + Average.User.Rating +
                       Price + Size + Recorded.Subtitle + IAP.Values + Minimum.IAP +
                       Maximum.IAP + Sum.IAP + Average.IAP + Description.Word.Count +
                       Developer.Category + Second.Age.Rating + Number.of.Languages +
                       Is.Available.In.English + Number.of.Genres + Release.Month +
                       Elapsed.Months + Game.Free, 
                     data = scaled_train_appstore_games_removed_user_Count_normalisation, 
                     method = "Manhattan", 
                     k = 5)
}

#model testing
pre_GKNN <- predict(train_GKNN, scaled_test_appstore_games_removed_user_Count_normalisation)

#model results
rslt_GKNN <- table(prediction = pre_GKNN,
                   Observed = scaled_test_appstore_games_removed_user_Count_normalisation$Categorical.Rating.Count)

acc_GKNN <- mean(pre_GKNN == scaled_test_appstore_games_removed_user_Count_normalisation)

rslt_GKNN
acc_GKNN

#SVM----------------------------------------------------------------------------


#Change dependent variable to numerical as is required for SVM
scaled_train_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count <- ifelse(scaled_train_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count == "High", 1, 0)
scaled_test_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count <- ifelse(scaled_test_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count == "High", 1, 0)

#only selecting complete cases otherwise the confusion matrix does not work

scaled_test_appstore_games_removed_user_Count_standardisation <- scaled_test_appstore_games_removed_user_Count_standardisation[complete.cases(scaled_test_appstore_games_removed_user_Count_standardisation),]

#model training
train_SVM <- svm (Categorical.Rating.Count ~ Developer + Average.User.Rating +
                    Price + Size + Recorded.Subtitle + IAP.Values + Minimum.IAP +
                    Maximum.IAP + Sum.IAP + Average.IAP + Description.Word.Count +
                    Developer.Category + Second.Age.Rating + Number.of.Languages +
                    Is.Available.In.English + Number.of.Genres + Release.Month +
                    Elapsed.Months + Game.Free,
                  data = scaled_train_appstore_games_removed_user_Count_standardisation,
                  kernel = "linear", type = "C-classification", )

#model testing
pre_SVM <- predict(train_SVM, scaled_test_appstore_games_removed_user_Count_standardisation)

#model results
rslt_SVM <- table( prediction = pre_SVM,
                   Observed = scaled_test_appstore_games_removed_user_Count_standardisation$Categorical.Rating.Count)

CM_SVM <- confusionMatrix(rslt_SVM, positive = "1")

CM_SVM

#The Accuracy is 0.7451
#The Sensitivity is 0.7695
#The Specificity is 0.7125
#
#
