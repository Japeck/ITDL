#libraries
library(readr)
library(stringr)
library(lubridate)
library(e1071)
library(dplyr)

#Task 1
#reading csv and creating dataframe
appstore_games <- read.csv("appstore_games.csv")

#Task 2
#removing columns URL and Name
appstore_games = subset(appstore_games, select = -c(URL, Name))

#creating new binary column and removing Subtitle column
appstore_games$Recorded.Subtitle <- ifelse(appstore_games$Subtitle>0, 1, 0)
appstore_games = subset(appstore_games, select = -c(Subtitle))

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

#creating a new column of 2 categories by making a dataframe of the developers frequencies and merging them
Developers <- table(appstore_games$Developer)
Developers.df <- as.data.frame(Developers)
colnames(Developers.df) <- c('Developer', 'Developer.Game.Count')
Developers.df$Developer.Category <- ifelse(Developers.df$Developer.Game.Count<4, 'Newbie', 'Professional')
appstore_games = merge(appstore_games, Developers.df, by='Developer')
appstore_games = subset(appstore_games, select = -c(Developer.Game.Count))

#a Second Age Rating column to divide games into just 4+ and 9+
appstore_games$Second.Age.Rating <- ifelse(appstore_games$Age.Rating=='4+', '4+', '9+')

#new Number.of.Languages column divided into Single or Many and games with no Languages or categorized as single
appstore_games$Number.of.Languages <- ifelse(nchar(appstore_games$Languages)<=2, 'Single', 'Many')
#new Is.Available.In.English column divided into Yes or No
appstore_games$Is.Available.In.English <- ifelse(lengths(strsplit(appstore_games$Languages,
                                                                  'EN'))==2 | startsWith(appstore_games$Languages,
                                                                  'EN') | endsWith(appstore_games$Languages, 'EN'),
                                      
                                                                             'Yes', 'No')

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

#creating new Categorical.Rating.Count column
appstore_games$Categorical.Rating.Count <- ifelse(appstore_games$User.Rating.Count<median(appstore_games$User.Rating.Count, na.rm=TRUE), 'Low', 'High')

#Task 3

#Task 4

#Task 5
#Creating the training observations
nObservations <- nrow(appstore_games)
ptraining <- 0.8
ntraining <- round(ptraining * nObservations)
  
obs_training <-  sample(1:nObservations, ntraining, replace = FALSE)

#Training and test data set with user.rating.count
appstore_games_sub <- subset(appstore_games, select = -c(ID, In.app.Purchases, Age.Rating, Genres, Languages, Original.Release.Date, Current.Version.Release.Date))

train_appstore_games <- appstore_games_sub[obs_training,]
test_appstore_games <- appstore_games_sub[-obs_training,]

#training and test data set without user.rating.count
appstore_games_removed_user_count <- subset(appstore_games, select = -c(ID, User.Rating.Count, In.app.Purchases, Age.rating, Genres, Languages, Original.Release.Date, Current.Version.Release.Date))

train_appstore_games_removed_user_count <- appstore_games_removed_user_count[obs_training,]
test_appstore_games_removed_user_count <- appstore_games_removed_user_count[-obs_training,]

#Task 6
#Making the attributes the correct class so they can be scaled if necessary
appstore_games$User.Rating.Count <- as.numeric(train_appstore_games$User.Rating.Count)
appstore_games$Description.Word.Count <- as.numeric(train_appstore_games$Description.Word.Count)

#scaling all numeric attributes
scaled_appstore_games <- appstore_games %>% mutate_if(is.numeric,scale)
scaled_appstore_games_removed_user_count <- appstore_games_removed_user_count %>% mutate_if(is.numeric, scale)

#Task 7
#models

#Naive Bayes

#GKNN

#SVM


