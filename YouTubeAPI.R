# Load required libraries
library(rjson)
library(RCurl)
library(sentiment)
library(ggplot2)
library(wordcloud)

#Generate your own key as per the instructions in document and paste it here to configure the API
key<- "AIzaSyCtXqcXod2qpAaDpWs5-PKYT7dymMTBbUA"


#Funtion to check connection. This getStats function will fetch the statistics of any video given the video ID and key. 
#Read instructions on how to get the video ID under the environment setup section

getStats <- function(id,key){
  url=paste("https://www.googleapis.com/youtube/v3/videos?id=",id,"&key=",key,"&part=statistics,snippet",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data,unexpected.escape = "skip")
  if(length(rd$items)!= 0){
    title<- rd$items[[1]]$snippet$title
    channelTitle<- rd$items[[1]]$snippet$channelTitle
    description<-rd$items[[1]]$snippet$description
    views<- rd$items[[1]]$statistics$viewCount
    likes<- rd$items[[1]]$statistics$likeCount
    if(is.null(likes)){
      likes<-"Info Not available"
    }
    dislikes<- rd$items[[1]]$statistics$dislikeCount
    if(is.null(dislikes)){
      dislikes<-"Info Not available"
    }
    fav<- rd$items[[1]]$statistics$favoriteCount
    if(is.null(fav)){
      fav<-"Info Not available"
    }
    comments<- rd$items[[1]]$statistics$commentCount
    if(is.null(comments)){
      comments<-"Info Not available"
    }
    
    return(data.frame(title,description,channelTitle,views,likes,dislikes,fav,comments))
  }
}




#getVideos function return the list of videos along with their statistics given the channelID and key.
# Read the instructions on how to get channelID under the environment setup section

getVideos<- function(channelID,key){
  url=paste("https://www.googleapis.com/youtube/v3/search?key=",key,"&channelId=",channelID,"&part=snippet,id&order=date&maxResults=10",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data)
  perPage<- rd$pageInfo$resultsPerPage
  totalResults<-rd$pageInfo$totalResults
  totalVideos<-min(perPage,totalResults)
  stats<-c(as.character(),as.character(),as.character(),as.integer(),as.integer(),as.integer(),as.integer(),as.integer())
  for (i in 1:totalVideos){
    kind<- rd$items[[i]]$id$kind 
    if(kind == "youtube#video"){
      videoID<- rd$items[[i]]$id$videoId 
      print(videoID)
      stats<-rbind(stats,getStats(videoID,key))
      
    }
    else if(kind == "youtube#playlist"){
      playlistID<- rd$items[[i]]$id$playlistId
      url=paste("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet%2CcontentDetails&maxResults=10&playlistId=", playlistID,"&key=",key,sep="")
      raw.data <- getURL(url) 
      rd1  <- fromJSON(raw.data)
      perPage<- rd1$pageInfo$resultsPerPage
      totalResults<-rd1$pageInfo$totalResults
      totalVideos<-min(perPage,totalResults)
      for(i in 1:totalVideos){
        videoID<-rd1$items[[i]]$contentDetails$videoId
        print(videoID)
        stats<-rbind(stats,getStats(videoID,key))
      }
    }
    
  }
  
  return(stats)
}


#getChannelsOrPlaylists function return the list of videos and their statistics associated with a keyword search on YouTube.
# When you search a keyword on youtube sometimes playlists also end up in search and we fetch the data from those playlists as well which might not be directly related to our search keyword, but it will fetch the data of similar searches.

getChannelsOrPlaylists<- function(search, key){
  search<-URLencode(search)
  url<-paste("https://www.googleapis.com/youtube/v3/search?q=",search,"&key=",key,"&type=channel&part=snippet&maxResults=50",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data)
  perPage<- rd$pageInfo$resultsPerPage
  totalResults<-rd$pageInfo$totalResults
  totalChannels<- min(perPage,totalResults)
  data<-c(as.character(),as.character(),as.character(),as.integer(),as.integer(),as.integer(),as.integer(),as.integer())
  for(i in 1:totalChannels){
    channelID<- rd$items[[i]]$id$channelId
    print(channelID)
    if(!is.null(channelID)){
      data<-rbind(data,getVideos(channelID,key))
    }
  }
  data<- data[complete.cases(data),]
  data<-unique(data)
  write.csv(data,"dataTest.csv",row.names=F)
  return(data)
}

search<-"Miley cyrus"
data<-getChannelsOrPlaylists(search,key)


some_txt = as.character(data$description)
head(some_txt)


if (!require("pacman")) install.packages("pacman")
#pacman::p_load(devtools, installr)
##please install the following to run this code
#install.Rtools()
#install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(sentiment)


# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL



class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)


sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of videos") +
  ggtitle("Analysis of videos of Miley Cyrus\n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of videos") +
  ggtitle("Analysis of videos of Miley Cyrus\n(classification by polarity)") +
  theme(plot.title = element_text(size=12, face="bold"))



df<- data.frame(likes=as.numeric(levels(data$likes)[data$likes]),emotion=data$emotion)
head(df)

likesData<-aggregate(likes~emotion,df,sum)

ggplot(likesData, aes(x=emotion, y=likes,fill=emotion))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="blues")+
  labs(x="Emotions", y="number of Likes") +
  ggtitle("Analysis of Number of likes based on emotions") +
  theme(plot.title = element_text(size=12, face="bold"))


