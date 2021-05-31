#Part 1: Scraping data from tripadvisor 

#loading libaries required 

library(tibble) 

library(tidyverse) 

library(stringr) 

library(dplyr) 

library(rvest) 



# building sequence of urls for scraping 

#urls of first 3 zoo reviews: 

# https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or5 

# https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or10 

# https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or15 

# 

# number at end of url jumps by 5 for each page  



# create a sequence which counts up by 5 to generate multiple urls -- 5 to 2000 = 400 pages of reviews 

pagenumbers <- seq(5,2000, by=5) #create a sequence of numbers to reflect url changes for all review pages 



# replace the number at end first url with sequence generated above and create vector 

urls <- paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers)  





# scrape location, heading, bodycopy and date of review with html nodes found using selector gadget (these take about 20 minutes to generate) 

locations1 <- lapply(paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers), 
                     
                     function(url){ 
                       
                       url %>% read_html()%>% 
                         
                         html_nodes('._1EpRX7o3') %>% 
                         
                         html_text() %>%                       
                         
                         gsub('[\r\n\t]','',.) 
                       
                     }) 



headings1 <- lapply(paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers), 
                    
                    function(url){ 
                      
                      url %>% read_html()%>% 
                        
                        html_nodes('.glasR4aX') %>% 
                        
                        html_text() %>%                       
                        
                        gsub('[\r\n\t]','',.) 
                      
                    }) 



bodycopy1 <- lapply(paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers), 
                    
                    function(url){ 
                      
                      url %>% read_html()%>% 
                        
                        html_nodes('.cPQsENeY') %>% 
                        
                        html_text() %>%                       
                        
                        gsub('[\r\n\t]','',.) 
                      
                    }) 

dates1 <- lapply(paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers), 
                 
                 function(url){ 
                   
                   url %>% read_html()%>% 
                     
                     html_nodes('._34Xs-BQm') %>% 
                     
                     html_text() %>%                       
                     
                     gsub('[\r\n\t]','',.) 
                   
                 }) 



#scraping of more complex ratings values using the html_elements and html_children  

ratings1 <- lapply(paste0('https://www.tripadvisor.ie/Attraction_Review-g186605-d214885-Reviews-or',pagenumbers), 
                   
                   function(url){ 
                     
                     url %>% read_html()%>% 
                       
                       html_elements(".nf9vGX55")%>% 
                       
                       html_children() %>% 
                       
                       html_attr("class") %>%  
                       
                       str_remove_all("ui_bubble_rating bubble_") 
                     
                   }) 





#convert lists of scraped data to vectors 

head <- as_vector(headings1) 

body <- as_vector(bodycopy1) 

date <- as_vector(dates1) 

location <- as_vector(locations1) 

ratings <- as_vector(ratings1) 





#using gsub to clean up unneeded data from date and location fields 

date <- gsub("Date of experience: ", "", date) 



location <- gsub("contributions","", location) 

location <- gsub("helpful votes", "", location) 

location <- gsub("helpful vote", "", location) 

location <- gsub("contribution","", location) 

location <- gsub("[[:digit:]]+", "", location) 

location <- gsub("  ", "", location) 





# vectors have differing lengths so will take a set amount of each for binding and analysis 

head2 <- head[1:1400] 

date2 <- date[1:1400] 

location2 <- location3[1:1400] 

body2 <- body[1:1400] 

ratings2 <- ratings [1:1400] 

# binding scraped fields 

bind <- cbind(head2, body2, date2, location2, ratings2) 



# creating csv file for groupwork/analysis 

write.csv(bind, "zooreviews.csv")  



## Querying our corpus 



# 1 Naive bayes of words and associated ranks 



knitr::opts_chunk$set(echo = TRUE) 



#load corpus 

reviews_raw <- read.csv(file="zooreviews.csv", header= TRUE, sep=",") 



# Wordcloud (visualisation) 

install.packages("wordcloud") 

library(wordcloud) 

# load text mining package 

install.packages("tm")  

library(tm) 

# library for Naive Bayes 

library (e1071) 

# gmodel for displaying naive bayes results 

install.packages("gmodels") 

library(gmodels) 


# Check data

summary(reviews_raw) 

# Check mean (addional query) 

mean(reviews_raw$ratings) 



# From here we can see that date, variation, and verified_reviews are characters. Rating and feedback are integers 

# Naive Bayes needs factor values to be applied so I will change rating and variation to factors. 

# The rest can remain as is as I won't be applying analysis on them. 



reviews_raw$ratings = factor(reviews_raw$ratings) 

reviews_raw$location = factor(reviews_raw$location) 

# check 



# check data 

str(reviews_raw) 



# Now we will check how many of each ratings and how many different locations we have 

table(reviews_raw$ratings) 

table(reviews_raw$location)
      
      
      
      
      
# Create a corpus from the body text. 
      
### Create a corpus from the body text and clean 
      
reviews_corpus = Corpus(VectorSource(reviews_raw$body))
      
      
      
# Have a little look at section the corpus 
      
inspect(reviews_corpus[5:10])
      
      
      
# Change the text to lower case & Saves to variable corpus_clean 
      
corpus_clean <- tm_map(reviews_corpus, content_transformer(tolower))  
      
# Cleaning the data 
      
corpus_clean <- tm_map(reviews_corpus, content_transformer(tolower))  
      
      
      
corpus_clean = tm_map(corpus_clean, removeNumbers)              # removes numbers 
      
corpus_clean = tm_map(corpus_clean, removeWords, stopwords())   # remove stop words 
      
reviews_corpus_test = corpus_clean[741:1060] 
      
prop.table(table(reviews_raw_train$ratings)) 
      
prop.table(table(reviews_raw_test$ratings)) 
      
      
      
#creating test and train portions of the data 
      
prop.table(table(reviews_raw_train$location)) 
      
prop.table(table(reviews_raw_test$location)) 
      
# Here the locations distribution were checked out of matter of interest.  
      
# This section was left out because there wasn't enough training samples in the dataset to train the model 
      
#prop.table(table(reviews_raw_train$location)) 
      
#prop.table(table(reviews_raw_test$location)) 
      
      
      
# Both rating training sets have approximately 63% 5 stars, 25% 4 stars, 7.5% 3 stars, 2% 2 stars, 4% 1 stars 
      
# This means that we have evenly distributed the factors or star reviews 
      
      
      
# In terms of the location I was not able to evenly distribute it over the training and test data  
      
# so I decided to omit it - 16 factors proved to be too much 
      
      
      
      
      
# A word cloud can be used to visually depict the frequency at which words appear in text data 
      
# Here I've added colour to denote the most occurring words 
      
      
      
wordcloud(ratings_corpus_train, min.freq = 10, random.order = F, colors = c("chartreuse", "cornflowerblue", "darkorange")) 
      
wordcloud(reviews_corpus_train, min.freq = 10, random.order = F, colors = c("chartreuse", "cornflowerblue", "darkorange")) 
      
      
      
# Lets look at the word clouds separated based on if they are 1 or 5 bubbles 
      
      
      
reviews_train = apply(reviews_train, MARGIN = 2, convert_counts) 
      
reviews_test  = apply(reviews_test, MARGIN = 2, convert_counts) 
      
      
      
### Obtaining results 
      
# load library for Naive Bayes 
      
library (e1071) 
      
      
      
reviews_classifier = naiveBayes(reviews_train, reviews_raw_train$ratings) 
      
reviews_test_pred = predict(reviews_classifier, reviews_test) 
      
      
      
      
      
# results 
      
install.packages("gmodels") 
      
library(gmodels) 
      
CrossTable(reviews_test_pred, reviews_raw_test$ratings, prop.chisq = F, prop.t = F, dnn = c('predicted', 'actual')) 
      
      
      
### Below we apply the random forest to our dataset attempt 1 (Kelly, J, 2021) 
      
# Load random forest package 
      
install.packages("randomForest") 
      
library(randomForest) 
      
# load gclus library 
      
library(gclus) 
      
      
      
# reload the data 
      
reviews_train = DocumentTermMatrix(reviews_corpus_train, list(dictionary = reviews_dict)) 
      
reviews_train$ratings <-as.factor(reviews_train$ratings) 
      
      
      
# fit random forest algorithm 
      
fit.rf <- randomForest(ratings~.,data=reviews_raw) 
      
      
      
# Result inspection 
      
fit.rf 
      
      
      
# Variable importance how this is not applicable in this case as we only focused on  
      
      
      
varImpPlot(fit.rf) 
      
      
      
      
      
CrossTable(reviews_test_pred, reviews_raw_test$ratings, prop.chisq = F, prop.t = F, dnn = c('predicted', 'actual')) 
      
      
      
install.packages("e1071") 
      
install.packages("caTools") 
      
install.packages("caret") 
      
      
      
# Loading package 
      
library(e1071) 
      
library(caTools) 
      
library(caret) 
      
      
      
      
      
confusionMatrix(reviews_test_pred,reviews_raw_test$ratings) 
      
      
      
      
      
# query 2: analysing ratings based on nationality of reviewer 
      
      
      
#load file containing country data  
      
reviews<- read.csv(file = "final_zoo.csv", header = TRUE, sep = ",") 
      
gsub(".*,","",reviews$country) 
      
      
      
as.data.frame(reviews)  
      
      
      
# looking at the nationality of reviewers using the unique function on the country column and the frequency with which they appear using the unique and count functions 
      
unique(reviews$country) 
      
reviews_by_country<- aggregate(data.frame(count = reviews$country),     
                                     
                                  list(value = reviews$country), 
                                     
                                  length) 
      
print(reviews_by_country) 
      
# this returned 41 unique results, 40 countries along with those who didn't provide a location 
      
# output of most frquently occuring countries 
      
# Country		count 
      
# United Kingdom	308 
      
# Ireland		284 
      
# Not listed		214 
      
# USA			69 
      
# Northern Ireland	28 
      
# Canada		20 
      
      
      
#creating vectors which contain the data pertaining to each specific country above which we have decided is frequent enough to be included 
      
Ireland <- subset(reviews, country == "Ireland") 
      
nrow(Ireland) 
      
Irish<- mean(Ireland$ratings2) 
      
      
      
      
      
Britain <- subset(reviews, country == "United Kingdom") 
      
nrow(Britain) 
      
British <- mean(Britain$ratings2) 
      
      
      
      
      
NI<- subset(reviews, country =="Northern Ireland") 
      
N_Irish <- mean(NI$ratings2) 
      
unique(reviews$country) 
      
      
      
      
      
USA<- subset(reviews, country =="USA") 
      
American <- mean(USA$ratings2) 
      
      
      
      
      
      
      
Canada <- subset(reviews, country == "Canada") 
      
Canadian <- mean(Canada$ratings2) 
      
review_by_country<- aggregate(data.frame(count = reviews$country),     
                                    
                                  list(value = reviews$country), 
                                    
                                  length) 
      
      
      
# calculating the mean of all reviews for comparison 
      
overall_score <- mean(reviews$ratings2) 
      
      
      
#creating a table where country score and name are combined 
      
scores <- c(Irish, British, N_Irish, American, Canadian, overall_score) 
      
country1 <- c("Ireland", "Britain", "Northern_Ireland", "USA", "Canada", "All") 
      
      
      
names(scores) <- country1 
      
      
      
print(scores) 
      
      
      
# output for average rating 
      
#        Ireland          Britain    Northern_Ireland              USA           Canada 		All  
      
#      45.10563         44.18831            46.07143         45.94203         46.50000           44.28428  
      
      
      
      
      
#section 2: sentiment analysis 
      
      
      
#installing required packages 
      
knitr::opts_chunk$set(echo = TRUE) 
    
install.packages('textdata') 
      
install.packages("ggplot2") 
      
      
      
#load required libraries 
      
library(tidyverse)      # data manipulation & plotting 
      
library(stringr)        # text cleaning and regular expressions 
      
library(tidytext)       # provides additional text mining functions 
      
library(textdata)       # Includes sentiment lexicons for classification and analysis. 
      
library("ggplot2")      # useful for graph 
      
      
      
## ---------------STEP 2 TIDYING DATA ---------------------------------------------------------------------- 
      
      
      
SentimentAnalysis= read.csv(file="zooreviews.csv", head=TRUE, sep=",") #upload the file 
      
sentiments #quick view of the sentiment words group 
      
c=SentimentAnalysis$body #converting column into vector 
      
text_tb=tibble(c) #converting vector into data frame 
      
      
      
text_tb=tibble(ReviewNumber=seq_along(c), text = c) # creating 2 columns 
      
      
      
SplitReviews=text_tb %>% 
        
    unnest_tokens(word, text) # splitting each review by word 
      
    SplitReviews 
      
      
      
      
      
#--------------------------STEP 3 SENTIMENT ANALYSIS-------------------- 
      
----------------#grouping words per type of sentiment------------------- 
      
      
      
Sentiment=SplitReviews %>% 
        
    right_join(get_sentiments("nrc")) %>% 
        
    filter(!is.na(sentiment)) %>% #filter if not sentiment 
        
      count(sentiment, sort = TRUE) 
      
Sentiment 
      
      
      
      
      
#plotting  
      
plot1=ggplot(Sentiment, aes(x=reorder(sentiment, n), y=n)) + geom_bar(stat="identity", color="white", fill="grey") + labs(title="Frequency sentiment words", y="frequency", x="sentiment word") + coord_flip() # Y axis derived from counts of X item 
      
print(plot1) 
      
      
      
      
      
----------------#grouping words in negative and positive------------------------  
      
#counting the number of words and grouping per type of sentiment 
      
bing_word_counts <- SplitReviews %>% 
        
        inner_join(get_sentiments("bing")) %>% 
        
        count(word, sentiment, sort = TRUE) %>% 
        
        ungroup() 
      
      
      
bing_word_counts 
      
      
      
      
      
#graph of sentiment analysis 
      
plot2=bing_word_counts %>% 
        
        group_by(sentiment) %>% 
        
        top_n(10) %>% 
        
        ggplot(aes(reorder(word, n), n, fill = sentiment)) + 
        
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) + 
        
        facet_wrap(~sentiment, scales = "free_y") + 
        
        labs(y = "Contribution to sentiment", x = NULL) + 
        
        coord_flip() 

print(plot2) 
      
      
      
# outputting wordcloud shaped in the letters zoo --------------------- 
      
library(wordcloud2) 
      
      
      
bing_word_counts1 <- bing_word_counts[,-(2),drop=FALSE] 
letterCloud(bing_word_counts1, word = "ZOO", size=1) 
      
      