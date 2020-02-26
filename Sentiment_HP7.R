installed.packages(c("sentimentr","tm","tidyverse","stringr","dplyr","tidytext","textdata","wordcloud","igraph","ggraph"))


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(sentimentr)
library(dplyr)
library(tm)
library(textstem)
library(tidyverse)    
library(stringr)  
library("readxl")
library(tidytext)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)


#Import and prepare the variables to use ahead
whole_script <-  read.csv("script.csv") 
summary(whole_script)





#Start with Text Mining (Perform tokenization,Frequency Count, Sentiment Analysis)


dialoges_tibble<-  tibble(Word=as.character(whole_script$TEXT))

str(dialoges_tibble)
head(dialoges_tibble)

## Tokenization of text (converting into one word per row and storing in another tibble)


freq_words<-dialoges_tibble%>% unnest_tokens(Word,Word)

## Removing the stop words from the tibble by using anti-join with stop_words data set present in tidyverse
freq_after_stopwords <- freq_words %>% 
  anti_join(stop_words,by = c("Word"="word" ))

## Performing basic Sentiment Analysis using NRC
freq_after_stopwords %>%
  right_join(get_sentiments("nrc"),by = c("Word"="word" )) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

## Performing basic Sentiment Analysis using BING
freq_after_stopwords %>%
  right_join(get_sentiments("bing"),by = c("Word"="word" )) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

## Getting thr word cloud of positive and negative terms
freq_after_stopwords %>%
  inner_join(get_sentiments("bing"),by = c("Word"="word" )) %>%
  count(Word, sentiment, sort = TRUE) %>%
  acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)


#Implementing social Network map



#Reaing all the charecters in the script

All_charecters<- read.csv("script.csv")

All_charecters <- All_charecters %>% select('character.A', 'character.B')


length(unique(All_charecters$`character.A`))
unique(All_charecters$`character.A`)

length(unique(All_charecters$`character.B`))
unique(All_charecters$`character.B`)


# Taking dialog by dialog, who spoke to whome

conversations <- All_charecters %>% group_by(`character.A`,`character.B`) %>% summarise(counts = n())

# taking the most talked charecters
conversations<-conversations[order(conversations$counts,decreasing = TRUE),]



set.seed(42) # setting a seed allows us to select the same sample every time

# Taking unique Nodes 
nodes <- c(as.character(conversations$character.A), as.character(conversations$character.B))
nodes <- unique(nodes)

# Plotting the graph
social_map_of_characters <- graph_from_data_frame(conversations, directed = TRUE)

plot(graph_map, vertex.size=4,
      vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
 

 
 ##Plotting most used words in the script
 text_script <- iconv(whole_script$TEXT)
 text_script <- Corpus(VectorSource(text_script))
 inspect(text_script[1:5])
 
 text_script<- tm_map(text_script,tolower)
 text_script<- tm_map(text_script,removePunctuation)
 text_script<- tm_map(text_script,removeNumbers)
 text_script<- tm_map(text_script,removeWords,stopwords('English'))
 text_script<- tm_map(text_script,removeWords,c('deathlyhallowspart','harrypotterforever','abcfamily','harrypotterweekend','potterheadweekend','starts','marathon','harrypotter','watching'
                                ,'potter','dont','harry'))
 text_script<- tm_map(text_script,stripWhitespace)
 
 tdm <- TermDocumentMatrix(text_script)
 
 tdm <- as.matrix(tdm)
 
 row_sum<-rowSums(tdm)
 row_sum<- subset(row_sum, row_sum>=25)
 
 barplot(row_sum,las=2, col=rainbow(50))
 

 
 
 
