####################################
# This file is can be used to re-create all the descriptive statistics and visualizations 
# that are presented in the paper "TWikiL - The Twitter Wikipedia Link dataset" 
# submitted to the dataset track at ICWSM 2022

# author: Florian Meier (fmeier@ikp.aau.dk)

##################################

library(RSQLite)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(scales)
library(ggforce)
library(ggstream)
library(patchwork)
library(plotly)
library(paletteer)

### Helpers 

#color palette used for plotting LE 
palette_OkabeIto_black <- c("Other"="#E69F00", "nl"="#56B4E9", "ru"="#009E73",
                                "fr"="#F0E442","de"="#0072B2", 
                                "es"="#D55E00", "ja"="#CC79A7", 
                                      "en"="#000000")

# colors associated with top seven language editions 
palette_OkabeIto_black_labels <- c("Other","Dutch (NL)","Russian (RU)",
                                      "French (FR)","German (DE)","Spanish (ES)",
                                          "Japanese (JA)","English (EN)")


######################################

# Create a database connection
db_connection <- dbConnect(RSQLite::SQLite(), "TWikiL_curated.sqlite")


# Get a random set of 10 000 tweets
DBI::dbGetQuery(db_connection, 
  "SELECT * FROM tweets_urls ORDER BY RANDOM() LIMIT 10000;")%>%
            as_tibble()->data



# Get the first 10 000 entries 
DBI::dbGetQuery(db_connection, 
                "SELECT * FROM tweets_urls LIMIT 10000;")%>%
  as_tibble()->data_first_10000


#The first 10.000 entries were submitted with the paper as sample for review
write_csv(data_first_10000,'TWikiL_curated_first10.csv')


# Create a table connection 
tweets_urls_df<-tbl(db_connection,'tweets_urls')


#==================
# The following lines can be used to recreate the descriptive statistics presented in Table 1 of the paper

# 1. What is the size of the dataset?
DBI::dbGetQuery(db_connection, 
                "SELECT COUNT(*) FROM tweets_urls;")%>%
                    as_tibble()->dataset_size

dataset_size
# i.e. is the number of Wikipedia concepts/articles linked to 
# 35,252,782


#==== Variable Tweet ID

# What is the number of unique tweets 
tweets_urls_df%>%
  summarise(n_distint_tweet_id = n_distinct(tweet_id))%>%
    collect()->n_distinct_tweet_id
# 34,543,612 unique tweets 


#What is the number of tweets with 2 or more concepts?
tweets_urls_df%>%
        group_by(tweet_id)%>%
          summarise(n_concepts_per_tweet = n())%>%
            collect()-> n_concepts_per_tweet


n_concepts_per_tweet%>%filter(n_concepts_per_tweet>1)%>%nrow()
# 474577 Tweets have two or more links in it

#Summary statistics 
summary(n_concepts_per_tweet$n_concepts_per_tweet)
sd(n_concepts_per_tweet$n_concepts_per_tweet)


#==== Variable Conversation ID
tweets_urls_df%>%
  select(tweet_id,conversation_id)%>%
    mutate(part_of_conv = ifelse(tweet_id==conversation_id,0,1))%>%
      collect()->part_of_conv

part_of_conv%>%
        group_by(part_of_conv)%>%
          summarise(count = n())%>%
            mutate(percentage = count/sum(count))%>%view()



#==== Variable Author ID 
tweets_urls_df%>%
  group_by(author_id)%>%
  summarise(n_tweets_per_author = n())%>%
  collect()-> n_tweets_per_author

# How many different authors/users posted an original tweet with a URL
n_tweets_per_author%>%nrow()
# 5467385

# What is min,max etc.
summary(n_tweets_per_author$n_tweets_per_author)
sd(n_tweets_per_author$n_tweets_per_author)

#How many users posted more than one wikipedia link?
n_tweets_per_author%>%filter(n_tweets_per_author>1)%>%nrow()


#==== Variable in_reply_to_userid

#What is the percentage of links in replys to others?
tweets_urls_df%>%
  group_by(in_reply_to_user_id)%>%
      summarise(count = n())%>%
        collect()-> links_as_replys


links_as_replys%>%arrange(desc(count))%>%head(1)%>%pull(count)
# 21,698,439 entries are NA i.e are not a reply
links_as_replys%>%distinct()%>%summarise(total = sum(count))%>%pull(total) - links_as_replys%>%arrange(desc(count))%>%head(1)%>%pull(count)
# 13,554,343 are in reply to another user 



#==== Tweet Lang
tweets_urls_df%>%
    group_by(tweet_lang)%>%
        summarise(count = n())%>%
            collect()-> tweet_lang_count

summary(tweet_lang_count$count)
sd(tweet_lang_count$count)


tweet_lang_count%>%arrange(desc(count))%>%
          mutate(percentage = count/sum(count)*100)
# top 3 en, ja, und 38.0 / 25.8 / 12.4 


# ==== Reply binary
tweets_urls_df%>%
  group_by(reply_binary)%>%
        summarise(count = n())%>%
            collect()-> reply_count

reply_count%>%mutate(percentage = count/sum(count)*100)
# 6722393 did get a reply
# 28530389 did not get a reply


#==== Attention Index Scaled

tweets_urls_df%>%
          select(attention_index_scaled)%>%
          summarise(mean_ais = mean(attention_index_scaled),
                    sd_ais = sd(attention_index_scaled))%>%
                      collect()->ais_values

# mean is only 0.002 - means there are quite many that didn't get any like or RT

# How many did get a reply, rt, like or quote?
tweets_urls_df%>%
  select(attention_index_scaled)%>%
    filter(attention_index_scaled>0)%>%
      collect()->ais_greater_0

# 11,706,639 received 'attention'



#==== Variable wiki_language_edition

# How many different LE do tweets link to? 
tweets_urls_df%>%
        group_by(wiki_language_edition)%>%
          summarise(count_le = n())%>%
            collect()->wiki_le_count

# Links to 310 different language editions 
summary(wiki_le_count$count_le)
sd(wiki_le_count$count_le)

wiki_le_count%>%mutate(percentage = count_le/sum(count_le)*100)%>%arrange(desc(percentage))
# Top 3 are en: 19037434 (54%), ja: 8627176 (24%), es: 1891589 (5%)


#==== Wikidata ID 

tweets_urls_df%>%
  group_by(wikidata_id)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%collect()->wikidata_id_count


# How many unique concepts are there?
wikidata_id_count%>%nrow()
# 4,047,344 different concepts 
summary(wikidata_id_count$count)
sd(wikidata_id_count$count)

#What is the Top3 and the percentage?
wikidata_id_count%>%
          mutate(percentage = count/sum(count)*100)%>%arrange(desc(percentage))
  
  
#==== Wiki category
tweets_urls_df%>%
  select(created_at,tweet_id,wiki_category)%>%
      collect()->wiki_category


wiki_category%>%
  separate_rows(wiki_category,sep = ';')%>%
    group_by(wiki_category)%>%
        summarise(count=n())%>%
          arrange(desc(count))->wiki_category_count

# How many concpets don't have a category
wiki_category_count%>%filter(is.na(wiki_category))%>%pull(count)

wiki_category_count%>%
          mutate(percentage = count/sum(count)*100)%>%arrange(desc(percentage))
            

# ==== Wiki category score

tweets_urls_df%>%
  select(wiki_category_scores)%>%
    collect()-> wiki_category_score

# Turn the character and ; departed values into separate rows 
# Turn the character into double
wiki_category_score%>%
        separate_rows(wiki_category_scores)%>%
        mutate(wiki_category_scores = as.numeric(wiki_category_scores))%>%
        summarise(mean_score = mean(wiki_category_scores,na.rm=TRUE),
               sd_score = sd(wiki_category_scores,na.rm=TRUE))->score_mean_final



# ====== 
# 1. Plot CDFs for Authors, Tweet lang, Attention Index, Wikidata
# ===================================================

# needs the dfs from top to create the cumulative distribution functions
# unless not produced up in the script will be created here


user_cdf<-ggplot(n_tweets_per_author, 
         aes(log(n_tweets_per_author))) + stat_ecdf(size=1.1)+
            theme_minimal(base_size = 16)+
              labs(y='CDF',x='(a) Tweets per User (log)')


language_cdf<-ggplot(tweet_lang_count, 
                 aes(log(count))) + stat_ecdf(size=1.1)+
                      theme_minimal(base_size = 16)+
                      scale_x_continuous(labels=scales::comma)+
                        labs(y='CDF',x='(b) Tweets per Language (log)')

tweets_urls_df%>%
  select(attention_index_scaled)%>%
            collect()->attention_index

ais_cdf<-ggplot(attention_index, 
       aes(log(as.numeric(attention_index_scaled)+0.001))) + stat_ecdf(size=1.1)+
              theme_minimal(base_size = 16)+
                  labs(y='CDF',x='(c) Attention Index Scaled (log+c)')

wikidata_cdf<-ggplot(wikidata_id_count,
             aes(log(count))) + stat_ecdf(size=1.1)+
              scale_x_continuous(labels=scales::comma)+
                theme_minimal(base_size = 16)+
                    labs(y='CDF',x='(d) Links per Wiki Concept (log)')


user_cdf+language_cdf+ais_cdf+wikidata_cdf+ plot_layout(ncol = 4)->cdf_plots


  
ggsave('cdf_plots.png',width = 52,height=10,units = 'cm')

  

# =======
# 2. Plot
#===================================================
# Timeseries of all - the top 6 most visited Wiki language editions and category other 


tweets_urls_df%>%
  collect()%>%
    mutate(year_month_day = ymd(as.Date(created_at)))%>%
      group_by(year_month_day,wiki_language_edition)%>%
        summarise(links_per_day_per_wiki = n())->links_per_day_per_wiki




#Picking the 7 with the highest share
top7_LE<- c('en','es','fr','ja','de','ru','nl')


#### Per month
links_per_day_per_wiki%>%
    mutate(year_month = floor_date(year_month_day,'month'))%>%
    #mutate(wiki_language_edition = ifelse(wiki_language_edition %in% top5_LE,
    #          wiki_language_edition,'Other LE'))%>%
      group_by(year_month,wiki_language_edition)%>%
        summarise(links_per_month_per_wiki = sum(links_per_day_per_wiki))->
          timeseries_data_complete_month


#What is the average links per month?
mean(timeseries_data_complete_month$links_per_month_per_wiki)


#What is the total i.e. taking all language editions over time
timeseries_data_complete_month%>%
              group_by(year_month)%>%
                summarise(count_total = sum(links_per_month_per_wiki))->monthly_total


#We do a combined time series and bar chart plot
# With top-7 and other category (Okabe-Ito has 8 colors)

timeseries_data_complete_month%>%
  mutate(wiki_language_edition = ifelse(wiki_language_edition %in% top7_LE,
           wiki_language_edition,'Other'))%>%
      group_by(wiki_language_edition)%>%
          summarise(total= sum(links_per_month_per_wiki))%>%
            mutate(percentage = round(total/sum(total),2))%>%
              arrange(desc(total))%>%
  mutate(
        wiki_language_edition = forcats::fct_rev(forcats::fct_inorder(wiki_language_edition)),
        wiki_language_edition = forcats::fct_relevel(wiki_language_edition, "Other", after = 0),
        percent_label = paste0(sprintf("%4.1f", percentage * 100), "%"))->wiki_language_edition_barchart


# Here comes the bar plot for share of language editions
wiki_language_edition_barchart%>%
                ggplot(aes(y=wiki_language_edition,
                    x=percentage,fill=wiki_language_edition))+geom_col()+
               geom_text(aes(label=percent_label),size=4,hjust = 1,
                                        nudge_x = 0.07,color='black')+
                scale_y_discrete(labels = palette_OkabeIto_black_labels)+
                scale_fill_manual(values = palette_OkabeIto_black)+
                scale_x_continuous(labels = label_percent())+theme_minimal(base_size = 16)+
                labs(y='',x='Percentage [%]',title='(a) Share of Links per Language Edition')+
                    theme(legend.position='none')->wiki_le_barchart
  
  
# This is to create the timeseries plot
timeseries_data_complete_month%>%
            filter(year_month<'2021-01-01')%>%
            mutate(wiki_language_edition = ifelse(wiki_language_edition %in% top7_LE,
                                             wiki_language_edition,'Other'))%>%
            group_by(wiki_language_edition,year_month)%>%
            summarise(links_per_month_per_wiki=sum(links_per_month_per_wiki))%>%
            mutate(wiki_language_edition = fct_reorder2(wiki_language_edition, 
                                        year_month,links_per_month_per_wiki)) %>%
            ggplot(mapping=aes(x=year_month,
                                  y=links_per_month_per_wiki,
                                      color=wiki_language_edition))+
              scale_color_manual(values = palette_OkabeIto_black)+
              scale_x_date(NULL,
                 breaks = scales::breaks_width("15 months"), 
                 labels = scales::label_date_short(),
                 expand = c(0,0)
                 ) + 
              scale_y_continuous(position = 'right',
                            breaks=c(0,25000,50000,
                             100000,150000,200000,250000,300000),
                                labels=scales::comma,
                            expand = expansion(mult = c(0, .1)))+
                geom_line(size=1.1)+
                  geom_smooth(aes(color = NULL),color='grey',alpha=0.25)+
                    theme_minimal(base_size = 16)+
                      labs(x='',y='Number of Links (Count)\n',color='',
                           title='(b) Monthly Link Count per Language Edition')+
                             theme(legend.position='none')+theme(panel.grid.minor=element_blank())->wiki_le_timeseries


# Interactive version to check some of the outliers manually
#ggplotly(wiki_le_timeseries)

layout<-'AABBBB'
  
wiki_le_barchart+wiki_le_timeseries+plot_layout(design = layout)->plot_1_final

plot_1_final

ggsave('wiki_le_timeseries.png',width = 52,height=16,units = 'cm')
#===============================================================================



# 2. Bar chart plot of meta category ts
# =====================================

# We need wiki_category from the descriptive stats on top

wiki_category%>%
  mutate(year_month_day = ymd(as.Date(created_at)))%>%
    mutate(year = floor_date(year_month_day,'year'))%>%
      #separate_rows(wiki_category,sep = ';')%>%
        mutate(wiki_category = str_remove(wiki_category,'\\..*$'))%>%
          group_by(year, wiki_category)%>%
            summarise(count=n())%>%
              arrange(desc(count))->wiki_category_year_count


# Rename History_and_Society
# Bring them in the right order from high to low
# Recode NA as Missing/NA and group the early years 2006,2007,2008,2009
wiki_category_year_count%>%
  mutate(
    wiki_category = ifelse(wiki_category=='History_and_Society','History & Society',wiki_category),
    wiki_category = ifelse(is.na(wiki_category),'Missing/NA',wiki_category),
    wiki_category = forcats::fct_rev(forcats::fct_inorder(wiki_category)),
    wiki_category = forcats::fct_relevel(wiki_category,'Missing/NA', after = 0))%>%
  #mutate(year = if_else(year<as.Date('2010-01-01'),as.Date('2009-01-01'),year))%>%
      group_by(year,wiki_category)%>%
        summarise(count = sum(count))%>%
          mutate(percentage = count/sum(count),
          percent_label = paste0(sprintf("%4.1f", percentage * 100), "%"))->wiki_category_year_count_ordered
      


# Create a stacked barchart 
wiki_category_year_count_ordered%>%
      filter(!year=='2021-01-01')%>%
      ggplot(aes(year,percentage,fill=wiki_category))+
      geom_col(color='white',alpha=0.8)+
            theme_minimal(base_size = 16)+
      scale_fill_paletteer_d('rtist::klimt')+
        labs(x='',y='Percentage of Links \n',color='',fill='Wiki Meta Category',title='(a)')+
        #theme(legend.position = c(.12, .8))+
        #theme(legend.background = element_rect(fill = "white",color='white'))+
      theme(legend.position='top', legend.justification='left',legend.direction='horizontal')+
            theme(legend.title = element_text(size=10), legend.text=element_text(size=10))+
              scale_x_date(
                breaks = date_breaks('1 year'),
                labels = scales::label_date_short(),
                expand = c(0, 0))+
              geom_text(aes(label=percent_label),
                    size = 3.5,color='white',position = position_stack(vjust = 0.5))+
              scale_y_continuous(labels=label_percent())->meta_category_ts


# =======================================
# Cumsum time series of top 5 concepts
wikidata_id_count%>%head(n=5)%>%pull(wikidata_id)->top5_concepts

top5_concepts_title <-tibble(wikidata_id=top5_concepts,title=c('Bangtan Sonyeondan (BTS)',
                                                'Consumption tax',
                                                  'Dunning-Kruger effect',
                                                    'Hexadecimal time',
                                                      'SoundBreak19 (SB19)'))

tweets_urls_df%>%
  collect()%>%
   filter(wikidata_id%in%top5_concepts)%>%
    mutate(year_month_day = ymd(as.Date(created_at)))%>%
      mutate(year_month = floor_date(year_month_day,'month'))%>%
        group_by(year_month,wikidata_id)%>%
          summarise(concept_per_month = n())->count_concepts_per_month_top5


# Here comes the cumsum
count_concepts_per_month_top5%>%
                  arrange(year_month)%>%
                    group_by(wikidata_id)%>%
                      mutate(cumsum_concept_month = cumsum(concept_per_month))%>%
                        select(year_month,wikidata_id,cumsum_concept_month)%>%
                          left_join(top5_concepts_title,by=c('wikidata_id'))%>%
                            unite('id_title',wikidata_id,title,sep=' - ')-> top5_timeseries
  

# Get the highest value for each concept for direct labeling i.e. no legend
top5_timeseries_last <- top5_timeseries%>%
                           group_by(id_title)%>%
                               filter(year_month == max(year_month))  
  
#Create the plot
top5_timeseries%>%
    ggplot(aes(x=year_month,y=cumsum_concept_month,group=id_title))+
                    geom_point(aes(shape=id_title))+
                          geom_line()+theme_minimal(base_size = 16)+
                          labs(x='',y='\n Number of Links (Cummulative Sum) \n',title = '(b)')+
                          scale_x_date(
                            breaks = date_breaks('15 month'),
                            labels = scales::label_date_short(),
                            limits = c(
                              ymd("2008-01-01"),
                              ymd("2021-02-01")
                            ),
                            expand = c(0, 0)
                          )+
                      scale_y_continuous(
                        labels=scales::comma,
                        sec.axis = dup_axis(
                          breaks = top5_timeseries_last$cumsum_concept_month,
                          labels = top5_timeseries_last$id_title,
                          name = NULL)) + guides(shape = "none",linetype='none')->top5_timeseries_plot

#####
# Here we combine the meta categories time series plot and the top 5 concepts 



# The layout for patchwork
layout<-'AAAAABBB'

# combine the plots according to the layout
meta_category_ts+top5_timeseries_plot+plot_layout(design = layout) -> plot_2_final
# draw the object
plot_2_final
# save the object
ggsave('meta_cat_top5_ts.png',width = 52,height=17,units = 'cm')
  
  


### ---- The End ---- ###
  
           