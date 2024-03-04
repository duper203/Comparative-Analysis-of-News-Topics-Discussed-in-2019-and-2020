##### Comparative Analysis of News Topics Discussed in 2019 and 2020 #####
##### Name: Hyesoo Kim, Jeongyun Kim
##### Matriculation number: EU-110076842, EU-110076843

install.packages("seededlda")
library(tidyverse)
library(quanteda)
library(htmltools)
library(seededlda)
library(quanteda.corpora)
library(stringr)
library(dplyr)
library(patchwork)
library(ggplot2)


##### Load Data #####
usenews.mediacloud.wm.2019 <- readRDS('usenews.mediacloud.wm.2019.rds')
usenews.mediacloud.wm.2020 <- readRDS('usenews.mediacloud.wm.2020.rds')
crowdtangle2019    <-    readRDS('usenews.crowdtangle.2019.rds')
crowdtangle2020    <-    readRDS('usenews.crowdtangle.2020.rds')

##### Create a list of only the news organisations we will use #####
##### We chose only news organisations that write in English #####
media_outlets = c("New York Times",
                  "BBC",
                  "CNN",
                  "Washington Post",
                  "msnbc.com 1",
                  "The Guardian UK",
                  "News.com.au",
                  "9 News",
                  "Yahoo News - Latest News & Headlines")

##### Make a empty list #####
datalist2019=list() 
datalist2020=list()

##### Create a loop that only pulls news from news outlets #####
##### in the media outlet list and puts them in the list #####
e=0 
for(c in c(1:76)){ 
  outlet.name = docvars(usenews.mediacloud.wm.2019[[c]], 'media_name')[1]
  if(outlet.name %in% media_outlets){ 
    e=e+1
    datalist2019[[e]] <- usenews.mediacloud.wm.2019[[c]] 
  }
}

e=0
for(c in c(1:81)){ 
  outlet.name = docvars(usenews.mediacloud.wm.2020[[c]], 'media_name')[1]
  if(outlet.name %in% media_outlets){ 
    e=e+1
    datalist2020[[e]] <- usenews.mediacloud.wm.2020[[c]] 
  }
}


##### Remove stop words, country and city names, unnecessary terms, and punctuation #####
datalist2019 <- lapply(datalist2019, function(x) {
  dfm(x, remove = c(stopwords("en"),"australia","australian","sydney",
                    "bbc","BBC","CNN","cnn","nsw",
                    "melbourne","queensland","america",
                    "american", "uk","united","washington","london","england",
                    "copyright", "media", "getty","9news","scotland","ireland","wales"
  ), remove_punct = TRUE)
})

datalist2020 <- lapply(datalist2020, function(x) {
  dfm(x, remove = c(stopwords("en"),"australia","australian","sydney",
                    "bbc","BBC","CNN","cnn","nsw",
                    "melbourne","queensland","america",
                    "american", "uk","united","washington","london","england",
                    "copyright", "media", "getty","9news","scotland","ireland","wales"
  ), remove_punct = TRUE)
})

##### Combine dfms in a datalist into a single dfm #####
combined_dfm19 <- rbind(datalist2019[[1]], datalist2019[[2]], datalist2019[[3]], 
                        datalist2019[[4]], datalist2019[[5]], datalist2019[[6]], 
                        datalist2019[[7]], datalist2019[[8]], datalist2019[[9]])
combined_dfm20 <- rbind(datalist2020[[1]], datalist2020[[2]], datalist2020[[3]], 
                        datalist2020[[4]], datalist2020[[5]], datalist2020[[6]], 
                        datalist2020[[7]], datalist2020[[8]], datalist2020[[9]])

##### LDA Topic Modelling #####
##### Leave frequently occurring words with dfm_trim() #####
dfm19 <- dfm_trim(combined_dfm19, min_termfreq = 0.8, termfreq_type = "quantile", max_docfreq = 0.4, docfreq_type = "prop")
dfm20 <- dfm_trim(combined_dfm20, min_termfreq = 0.8, termfreq_type = "quantile", max_docfreq = 0.4, docfreq_type = "prop")

set.seed(123)

##### Extract 10 topics from trimmed dfm #####
lda_19 <- textmodel_lda(dfm19, k=10, max_iter = 100)
lda_20 <- textmodel_lda(dfm20, k=10, max_iter = 100)

##### Extract 10 words that correspond to each topics #####
terms(lda_19,10)
terms(lda_20,10)

##### Add topics back to our dataframe as a document-level variable #####
df_19 <- docvars(combined_dfm19) %>% as.data.frame()
df_19$topic <- topics(lda_19)
df_20 <- docvars(combined_dfm20) %>% as.data.frame()
df_20$topic <- topics(lda_20)


##### Create a graph showing how many news stories were published by topics based on publication date #####
topicGraph19 <- 
  df_19 %>% 
  filter(!is.na(topic)) %>% 
  group_by(topic, publish_date) %>%
  summarise(freq = n()) %>% 
  ggplot(aes(x=publish_date, y= freq, colour=topic)) + 
  geom_line(na.rm = T) + ylim(c(0, 400)) +
  theme_minimal() +
  labs(title = "News publications by topic based on publication date - 2019", x = "Publication Date", y = "Publications")
topicGraph19
topicGraph20 <- 
  df_20 %>% 
  filter(!is.na(topic)) %>% 
  group_by(topic, publish_date) %>%
  summarise(freq = n()) %>% 
  ggplot(aes(x=publish_date, y= freq, colour=topic)) + 
  geom_line(na.rm = T) + ylim(c(0, 400)) +
  theme_minimal() +
  labs(title = "News publications by topic based on publication date - 2020", x = "Publication Date", y = "Publications")
topicGraph20
##### Create a graph for the COVID topic #####
df_20 %>% 
  filter(topic == "topic1") %>%
  mutate(year_month = format(as.Date(publish_date), "%Y-%m-%d")) %>% 
  group_by(year_month) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = as.Date(year_month), y =freq, group=1)) +
  geom_line(colour = "#F8766D") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%B") + 
  theme_minimal() +
  labs(title = "News publications of Topic 1(COVID) base on publish date", x = "Publication Date", y = "Publications")


##### Add Facebook Data #####
##### Count the number of angry on posts linking to news articles on Facebook #####
crowdtangle2019 = usenews.crowdtangle.2019 %>% group_by(link) %>% summarize(angry = sum(statistics.actual.angryCount, na.rm=T)) 
crowdtangle2020 = crowdtangle2020 %>% group_by(link) %>% summarize(angry = sum(statistics.actual.angryCount, na.rm=T)) 

##### Combine the overlapping columns, url and link, #####
##### with the dataframe we already created using left_join() #####
outlets_fb_19 = left_join(df_19, crowdtangle2019, by=c("url"="link"))
outlets_fb_20 = left_join(df_20, crowdtangle2020, by=c("url"="link"))

##### Create a graph counting the number of angry reactions to news by topic #####
fbGraph19 <- 
  outlets_fb_19 %>% 
  filter(!is.na(topic)) %>% 
  group_by(topic, angry) %>%
  summarise(freq = n()) %>% 
  ggplot(aes(x=topic, y= freq, fill=topic)) +
  geom_bar(stat='identity')+
  theme_minimal() +
  labs(title = "Number of Facebook angry reactions to news by topic - 2019", x = "Topic", y = "Sum of angry")
  
fbGraph20 <- 
  outlets_fb_20 %>% 
  filter(!is.na(topic)) %>% 
  group_by(topic, angry) %>%
  summarise(freq = n()) %>% 
  ggplot(aes(x=topic, y= freq, fill=topic)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title = "Number of Facebook angry reactions to news by topic - 2020", x = "Topic", y = "Sum of angry")



