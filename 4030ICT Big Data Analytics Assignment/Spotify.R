#loading packages for spotify
pkgs<- c("Rspotify", "spotifyr", "httpuv","ggridges", "highcharter", "knitr","tm", "tidyverse", "igraph", "ggplot2", "stringr", "scales", "dplyr", "plyr", "caret")
install.packages(pkgs)
install.packages('e1071', dependencies=TRUE)
lapply(pkgs, library, character.only= TRUE)

library(devtools)
install_github("tiagomendesdantas/Rspotify")
install_github("charlie86/spotifyr")
library(Rspotify)
library(spotifyr)

options(httr_oauth_cache=T)

#spotify app id and secret
app_id <- "55712209b7d34801be8ba9a0e519f563"
app_secret<- "3058046509a5488b8b8f2247737066fb"
token <- "1"
#putting spotify info in singluar variable
keys <- spotifyOAuth(token, app_id, app_secret)
#connecting to spotify
Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get Songs from Blackbear (and their audio features)

blackbearsongs <- get_artist_audio_features('blackbear')
View(blackbearsongs)


# Get Blackbear Songs that have a full record associated with them

getScore <- subset(blackbearsongs, track_name!="NA")
median(getScore$valence)
mean(getScore$valence)
median(getScore$speechiness)
mean(getScore$speechiness)
median(getScore$instrumentalness)
mean(getScore$instrumentalness)
names(getScore)


# Plot Blackbear Song Valence Data.

ggplot(data=getScore,
       aes(x=getScore$valence, y=getScore$track_name)) +
  geom_point() +
  theme(axis.text.y = element_text(size = 5)) +
  xlab("Valence Score") +
  ylab("Album") + 
  ggtitle("Blackbear's Valence Score")


# Get top 100 songs playlist audio features

top100 <- get_playlist_audio_features('spotify', "4hOKQuZbraPDIfaGbM3lKI")
View(top100)


# Add the isDrake column to each score array (to indicate which songs are drake and which are not)

top100["isBlackbear"] <- 0
getScore["isBlackbear"] <- 1


# Combine the score arrays and remove duplicate songs if present

dataSet = rbind.fill(top100, getScore)
dataSet <- dataSet[!duplicated(dataSet),]


# Select only the feature columns and isDrake column.

cols <- c(6:16, 62)
dataSet <- dataSet[,cols]
View(dataSet[1,])


# Change the isDrake column into a factor (for both testing and training sets)
dataSet$isBlackbear <- factor(dataSet$isBlackbear)


# Randomize data set

dataSet <- dataSet[sample(1:nrow(dataSet)),]


# Split the data set into training and testing sets (80% training set, 20% testing)

splitPoint <- as.integer(nrow(dataSet)*0.8)
trainingSet <- dataSet[1:splitPoint,]
testingSet <- dataSet[(splitPoint+1):nrow(dataSet),]


# Train the CART Model

prediction.r <- train(isBlackbear~ ., data=trainingSet, method="rpart")
prediction.r


# Sample a single prediction (can repeat)

prediction_row = 3 # MUST be smaller or equal to than training data set size
if (predict(prediction.r, testingSet[prediction_row,]) == 
    testingSet[prediction_row, 12]){
  print("Correct!")
} else{
  ("Wrong.")
}


# Analyse the model accuracy using the Confusion Matrix

confusionMatrix(prediction.r, reference = testingSet$isBlackbear)
