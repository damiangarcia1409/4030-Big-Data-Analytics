# Import and attach libraries/packages

pkgs <- c("rtweet","ggplot2", "tm", 'Rspotify', "spotifyr", "dplyr",
          "plyr", "caret")
install.packages(pkgs)
lapply(pkgs, library, character.only=TRUE)


appname <- "UniApp4030"
my_api_key <- "oQs4rckGtFTyd8N1x6f5mhI8p"
my_api_secret <- "cGmJNjeogfghhaU7wKpYpP9K9ZLK7JqAoivBULQnYbCmaRIHG8"
my_access_token <- "443705119-m0XeJ6yHiArmKPmdmNou2PGUAIpGrPrVkfHv4kj1"
my_access_token_secret <- "2kQ1fFj9KeYteDsJiUvBWU31wl58hGh6qYv12PqvyPYb3"


# Authenticate and get data


# Use twitter API to get the followers of Griffith Uni and information about each.

modsun_followers <- get_followers("MODSUN", n = 10000)
user_data.df <- lookup_users(modsun_followers$user_id)
View(user_data.df)


# Plot scatter of Friends Count vs Followers Count.

ggplot(data=user_data.df, aes(x=followers_count, y=friends_count)) + 
  geom_point(stat = "identity") + 
  xlab("Followers Count") + 
  ylab("Friends Count") + 
  ggtitle("Friends vs Followers")


# Plot scatter of Friends Count vs Followers Count (using logarithmic scales).

ggplot(data=user_data.df, aes(x=log(followers_count), y=log(friends_count))) + 
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")


# Get Friends Count and Followers Count under logarithmic functions.

logFriendsCount <- log(user_data.df$friends_count)
logFollowersCount <- log(user_data.df$followers_count)


# Create data frame with log data derived above

kObject.log <- data.frame(user_data.df$name, logFollowersCount, logFriendsCount)
View(kObject.log)


# Remove invalid records from the data frame.

kObject.log <- subset(kObject.log, kObject.log$logFriendsCount!="-Inf")
kObject.log <- subset(kObject.log, kObject.log$logFollowersCount!="-Inf")


# Run KMeans Clustering Algorithm.
user2Means.log <- kmeans(kObject.log[,2:ncol(kObject.log)], centers=5, iter.max=10, nstart=10)
kObject.log$cluster=factor(user2Means.log$cluster)
View(kObject.log)


# Plot cluster data.
ggplot(data=kObject.log,
       aes(x=logFollowersCount, y=logFriendsCount, colour=cluster)) +
  geom_point(stat = "identity") + 
  xlab("Log Followers Count") + 
  ylab("Log Friends Count") + 
  ggtitle("Friends vs Followers - Log 10 Scale")

user2Means.log$centers


## Repeat for other Users! ##