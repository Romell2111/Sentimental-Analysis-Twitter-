#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny) 
library(tm)
library(wordcloud)
library(twitteR)
shinyServer(function(input, output, session) {
  setup_twitter_oauth(consumer_key = "741531364399316992-O9j5c42qe0SHQ1cEBMUaXh5iN1jiJKf", consumer_secret = "ohtUp8YwqsuiJ8jUFv49iZpjs9DqtgbL8M4VCcyccfK02") 
  token <- get("oauth_token", twitteR:::oauth_cache) #Save the credentials info
  token$cache()
  output$currentTime <- renderText({invalidateLater(1000, session) #Here I will show the current time
    paste("Current time is: ",Sys.time())})
  observe({
    invalidateLater(60000,session)
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    vector_users <- vector()
    vector_sentiments <- vector()
    tweets_result = ""
    tweets_result = searchTwitter("word-or-expression-to-evaluate") #Here I use the searchTwitter function to extract the tweets
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      vector_users <- c(vector_users, as.character(tweet$screenName)); #save the user name
      if (grepl("I love it", tweet$text, ignore.case = TRUE) == TRUE | grepl("Wonderful", tweet$text, ignore.case = TRUE) | grepl("Awesome", tweet$text, ignore.case = TRUE)){ #if positive words match...
        count_positive = count_positive + 1 # Add the positive counts
        vector_sentiments <- c(vector_sentiments, "Positive") #Add the positive sentiment
        positive_text <- c(positive_text, as.character(tweet$text)) # Add the positive text
      } else if (grepl("Boring", tweet$text, ignore.case = TRUE) | grepl("I'm sleeping", tweet$text, ignore.case = TRUE)) { # Do the same for negatives 
        count_negative = count_negative + 1
        vector_sentiments <- c(vector_sentiments, "Negative")
        negative_text <- c(negative_text, as.character(tweet$text))
      } else { #Do the same for neutrals
        count_neutral = count_neutral + 1
        print("neutral")
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(neutral_text))
      }
    }
    df_users_sentiment <- data.frame(vector_users, vector_sentiments) 
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    output$distPlot  0){
    output$positive_wordcloud  0) {
  output$negative_wordcloud  0){
    output$neutral_wordcloud <- renderPlot({ wordcloud(paste(neutral_text, collapse=" "), min.freq = 0, random.color=TRUE , max.words=100 ,colors=brewer.pal(8, "Dark2"))  }) 
  }
})
  })
  })
})