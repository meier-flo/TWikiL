######
# This file shows how to work with TWikiL 
# especially how to collect tweet full-text for a subset of tweets
# also known as hydrating
#####################

library(tidyverse)
library(rtweet)
library(RSQLite)

# This example uses the rtweet package which allows different ways of 
# connecting to Twitter's API  e.g. via a user account or a app.
# This example assumes a fresh install and connection with a regular user account


# Create a database connection
db_connection <- dbConnect(RSQLite::SQLite(), "TWikiL_curated.sqlite")

# Create a table connection 
tweets_urls_df<-tbl(db_connection,'tweets_urls')

# Get all Tweets that link to Donald Trump (wikidata id = Q22686)
tweets_urls_df%>%
        filter(wikidata_id == 'Q22686')%>%
          collect()->trump_wiki_links

#Hydrate all Tweets i.e. get full text for those tweets 
trump_tweets_hydrated<-lookup_statuses(trump_wiki_links$tweet_id)


