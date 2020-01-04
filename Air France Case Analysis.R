######################################################################################
"

Data Science: R

Business Case: Air France

Created by:

Jorge Betancourt,
Oliver Baseley,
Yi Yuan,
German Perdomo,
Salewa Oyagbola,
Neil Parekh,

Date:
11/29/2019

" 
#####################################################################################
#                                 Libraries                                         #  
#####################################################################################

# install.packages("plotly")
# install.packages('rJava')
# install.packages("openNLP")
# install.packages("qdap")
# install.packages("dendextend")
# install.packages("ggthemes")
# install.packages("RWeka")

# Libraries

library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(rJava)
library(qdap)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggthemes)
library(RWeka)

#####################################################################################
#                                 Data Massaging                                    #  
#####################################################################################

# import air france dataset from the working directory

AirFdf <- read_excel("Air France Case Spreadsheet Supplement.xls", 
                   sheet = "DoubleClick")
View(AirFdf)
nrow(AirFdf)
# View the structure of AFdf
glimpse(AirFdf)
str(AirFdf)

# Convert the blanks to "NA"

AirFdf[AirFdf==""] <- NA

# Check for missing values using the sapply() function 

sapply(AirFdf, function(x) sum(is.na(x)))

# creating a new column for ROA as KPI

# typeof(AFdf$Amount)
# typeof(AFdf$`Total Cost`)

Total_Cost <- unique(AirFdf$`Total Cost`)


AirFdf <- within(AirFdf, ROA <- Amount/`Total Cost`)


# 1. print channels
publisher_name <- unique(AirFdf$`Publisher Name`)

# tally sales by channel
sales <- c()
for (i in 1:length(publisher_name)) {
  sales <- c(sales,sum(AirFdf$Amount[which(AFdf[,2] == publisher_name[i])]))
  i <- i + 1
}
print(sales)

cbind(publisher_name,as.numeric(sales))
# find the most profitable channel
max_sales<-max(sales)
publisher_name[which(sales == max_sales)]


#####################################################################################
#                     Publisher Strategy | Most Effective Channel                   #  
#####################################################################################

############################
#  Descriptive Statistics  #
############################

# Create a new data frame for the analysis

AFdf <- AirFdf

Statistics <- c("Mean", "Median", "SD", "Min", "Max")
Amount <- round(c(mean(AFdf$Amount),median(AFdf$Amount), sd(AFdf$Amount),min(AFdf$Amount), max(AFdf$Amount)), 2)
Total_Cost <- round(c(mean(AFdf$`Total Cost`),median(AFdf$`Total Cost`),sd(AFdf$`Total Cost`),min(AFdf$`Total Cost`),max(AFdf$`Total Cost`)), 2)
Impressions <- round(c(mean(AFdf$Impressions),median(AFdf$Impressions),sd(AFdf$Impressions),min(AFdf$Impressions),max(AFdf$Impressions)), 2)
Clicks <- round(c(mean(AFdf$Clicks),median(AFdf$Clicks),sd(AFdf$Clicks),min(AFdf$Clicks),max(AFdf$Clicks)), 2)
Summary <- as.data.frame(cbind(Statistics, Amount, Total_Cost, Impressions, Clicks))

############################
#    Create New Columns    #
############################

AFdf <- AFdf[-c(338),]

# removing a Total Cost = 0 that generate an infinite value

AFdf$Revenue <- AFdf$Amount - AFdf$`Total Cost`
AFdf$ROA <- as.numeric(AFdf$Revenue / AFdf$`Total Cost`)
AFdf$Book_Prob <- ( AFdf$`Trans. Conv. %` * AFdf$`Engine Click Thru %`) /100
AFdf <- within(AFdf, Cost_Book <- `Total Cost`/ `Total Volume of Bookings`)
AFdf[AFdf==""] <- 0
AFdf[AFdf=="Inf"] <- 0
AFdf$Average_Revenue_Booking <- AFdf$Amount / AFdf$`Total Volume of Bookings`
summary(AFdf) 

############################
#   Checking Data Frame    #
############################

summary(AFdf)
typeof(AFdf$`Match Type`)
head(AFdf)

############################
# Create Pivot Table in R  #
# to compare ROA and Avg.  #
# Cost per Click           #
############################

# dplyr library is loaded 

AFdf_pivot <- AFdf %>% group_by(`Publisher Name`) %>% summarize(
  avg_ROA = mean(ROA),
  avg_cpc = mean(`Avg. Cost per Click`)
)
summary(AFdf_pivot)

############################
#  Create Pivot Table For  #
#      Bubble Chart to     #
#  compare different SEM   #
############################

AFdf_pivot2 <- AFdf %>% group_by(`Publisher Name`) %>% summarize(
  total_records = n(),
  total_amount = sum(`Total Cost`),
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_prob = mean(`Book_Prob`),
  avg_ROA = mean(ROA)
)

summary(AFdf_pivot2)

############################
#         Bar Plot         #
############################

# plotly Library is loaded

# Bar chart to compare differents SEM
x <- c('Kayak', 'MSN-Global', 'MSN - US','Yahoo-US', 'Google-Global', 'Overture-Global', 'Google-US', 'Overture-US')
y <- c(64.5, 11.6, 2.18, 11.3, 5.69, 5.48, 2.22, 2.22)
data <- data.frame(x, y)

#The alphabetic default order so we need to code the following:
data$x <- factor(data$x, levels = data[["x"]])

p <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Return on Advertising", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

# Bar chart to compare the Top Two SEM

w <- c('Kayak', 'MSN-Global')
z <- c(64.5, 11.6)
data2 <- data.frame(w, z)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["w"]])

p <- plot_ly(data2, x= ~w, y= ~z, type = "bar", mode = "lines+markers+text", name = "Return on Advertising", color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

# Bar chart to compare the Top Two SEM

e <- c('Kayak Website', 'Other Online Travel-Sites')
t <- c(8, 0.8)
data2 <- data.frame(e, t)

#The alphabetic default order so we need to code the following:
data2$w <- factor(data2$w, levels = data2[["e"]])

p <- plot_ly(data2, x= ~e, y= ~t, type = "bar", mode = "lines+markers+text", name = "Click-Through Rate", color = I("red"), alpha = 0.5, width = 0.1) %>%
  layout(title = " Click-Through Rate (%) ",
         xaxis = list(title = ""),
         yaxis = list(title = "")
  )
p


############################
#      Bubble Chart        #
############################

# tidyverse and Plotly Libaries are Loaded 

p <- plot_ly(AFdf_pivot2, x = ~avg_prob, y = ~avg_ROA,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc, 
             color = ~`Publisher Name`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Publisher Strategy',
         xaxis = list(title = "Probability of Booking", showgrid = TRUE),
         yaxis = list(title = "Average ROA", showgrid = TRUE),
         showlegend = TRUE)

p

summary(AFdf_pivot2)


#####################################################################################
#                              Best Match Type & Bid Strategy                       #  
#####################################################################################



# Create a new data frame for the analysis

# Mtdf stands for Match type Dataframe

Mtdf <- AirFdf

#Slice the data

Mtdf[Mtdf=="Inf"] <- NA

Mtdf <- Mtdf[which(Mtdf$ROA > 0),]
Mtdf <- Mtdf[which(Mtdf$ROA != 'NA'),]

bid_strategy <- unique(Mtdf$`Bid Strategy`)
match_type <- unique(Mtdf$`Match Type`)
bid_strategy1 <- bid_strategy

#For best Bid Strategy

ROA_bid_strategy <-c()
Yahoo_US_Bid <- c()
MSN_Global_Bid <- c()
Google_Global_Bid <- c()
Overture_Global_Bid <- c()
Google_US_Bid <- c()
Overture_US_Bid <- c()
MSN_US_Bid <- c()


for (i in 1:length(bid_strategy)){
  Yahoo_US_Bid <- c(Yahoo_US_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                        Mtdf$'Publisher Name' == publisher_name[1])]))
  MSN_Global_Bid <- c(MSN_Global_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                            Mtdf$'Publisher Name' == publisher_name[2])]))
  Google_Global_Bid <- c(Google_Global_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                  Mtdf$'Publisher Name' == publisher_name[3])]))
  Overture_Global_Bid <- c(Overture_Global_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                      Mtdf$'Publisher Name' == publisher_name[4])]))
  Google_US_Bid <- c(Google_US_Bid, mean(AFdf$ROA[which(AFdf$'Bid Strategy' == bid_strategy[i] & 
                                                          AFdf$'Publisher Name' == publisher_name[5])]))
  Overture_US_Bid <- c(Overture_US_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                              Mtdf$'Publisher Name' == publisher_name[6])]))
  MSN_US_Bid <- c(MSN_US_Bid, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                    Mtdf$'Publisher Name' == publisher_name[7])]))
  i <- i +1
}

ROA_bid_strategy <- cbind(bid_strategy, Yahoo_US_Bid, MSN_Global_Bid, Google_Global_Bid, Overture_Global_Bid, Google_US_Bid
                          , Overture_US_Bid, MSN_US_Bid)
View(ROA_bid_strategy)

#For Best Match Type
ROA_match <-c()
Yahoo_US_Match <- c()
MSN_Global_Match <- c()
Google_Global_Match <- c()
Overture_Global_Match <- c()
Google_US_Match <- c()
Overture_US_Match <- c()
MSN_US_Match <- c()


for (i in 1:length(match_type)){
  Yahoo_US_Match <- c(Yahoo_US_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                            Mtdf$'Publisher Name' == publisher_name[1])]))
  MSN_Global_Match <- c(MSN_Global_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                AFdf$'Publisher Name' == publisher_name[2])]))
  Google_Global_Match <- c(Google_Global_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                      Mtdf$'Publisher Name' == publisher_name[3])]))
  Overture_Global_Match <- c(Overture_Global_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                          Mtdf$'Publisher Name' == publisher_name[4])]))
  Google_US_Match <- c(Google_US_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                              Mtdf$'Publisher Name' == publisher_name[5])]))
  Overture_US_Match <- c(Overture_US_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                                  Mtdf$'Publisher Name' == publisher_name[6])]))
  MSN_US_Match <- c(MSN_US_Match, mean(Mtdf$ROA[which(Mtdf$'Bid Strategy' == bid_strategy[i] & 
                                                        Mtdf$'Publisher Name' == publisher_name[7])]))
  i <- i +1
}

ROA_match <- cbind(match_type, Yahoo_US_Match, MSN_Global_Match, Google_Global_Match, Overture_Global_Match, Google_US_Match
                   , Overture_US_Match, MSN_US_Match)
View(ROA_match)

#####################################################################################
#                               SEM Keywords Strategy                               #  
#####################################################################################

# Create a new data frame for the analysis

kwdf <- AirFdf

View(kwdf)

# Filter the Data with ROA higher than 0

kwdf <- filter(kwdf, ROA > 0)

nrow(kwdf)

# divide data to do an analysis  by channel

# Google_us
Gusdf <- kwdf[which(kwdf$`Publisher Name` == "Google - US"),]

# Yahoo_us
Yusdf<-kwdf[which(kwdf$`Publisher Name` == "Yahoo - US"),]

# MSN_us
msn_usdf<-kwdf[which(kwdf$`Publisher Name` == "MSN - US"),]

# Overture_us_df
Ousdf<-kwdf[which(kwdf$`Publisher Name` == "Overture - US"),]

# MSN_global
msn_gldf<-kwdf[which(kwdf$`Publisher Name` == "MSN - Global"),]

# google_global
ggldf<-kwdf[which(kwdf$`Publisher Name` == "Google - Global"),]

# overture_global_df
Ogldf<-kwdf[which(kwdf$`Publisher Name` == "Overture - Global"),]


############################
#     Keyword Analysis     #
############################

# Isolate text from kwdf: Glob_kw stands for Global keywords
Glob_kw <- kwdf$Keyword
View(Glob_kw)

# Create a Barchart to visualize the most common keywords

# Create frequency for the 4510 obs 
frequency1 <- freq_terms(AFdf$Keyword,
                         top = 10,
                         at.least = 3, 
                         stopwords = c(tm::stopwords("en"))
)


# Make a frequency barchart
plot(frequency1)

# Create frequency for the filtered data 
frequency2 <- freq_terms(kwdf$Keyword,
                        top = 10,
                        at.least = 3, 
                        stopwords = c(tm::stopwords("en"))
)


# Make a frequency barchart
plot(frequency2)


###############################################################################

"
Word clouds and more interesting visuals
"

"
Common text mining visuals
Visuals are  good at quickly processing visual information
Word clouds are the gobaly used in text mining, 
They are still a good way to get a glimpse of common terms.
"


"
Make the vector a VCorpus object
There are two kinds of the corpus data type, the permanent corpus, PCorpus, 
and the volatile corpus, VCorpus. In essence, the difference between the two 
has to do with how the collection of documents is stored in your computer.
In this course, we will use the volatile corpus, which is held in 
your computer’s RAM rather than saved to disk, just to be more memory efficient.
"

# the tm library is loaded

# Make a vector source: Gbalkw_source stands for Global keywords source
Gbalkw_source <- VectorSource(Glob_kw)

"
Make the vector a VCorpus object
The VCorpus object is a nested list, or list of lists.
At each index of the VCorpus object, there is a PlainTextDocument object, 
which is essentially a list that contains the actual text data (content), 
as well as some metadata.

"

# Make a volatile corpus: Gbalkw_corpus stands for Global keywords corpus
Gbalkw_corpus <- VCorpus(Gbalkw_source)

# Print out Gbalkw_corpus
Gbalkw_corpus

# Print data on the 14th keyword in Gbalkw_corpus
Gbalkw_corpus[[14]]

# Print the content of the 14th keyword in Gbalkw_corpus
Gbalkw_corpus[[14]]$content


"
Apply preprocessing steps to a corpus
the tm_map function is used to apply a processing function to a corpus
the tm package functions do not need content_transformer(), but base R and qdap 
functions do.
"

# Alter the function code to match the instructions
clean_Gbalkwcorpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

# Apply your customized function to the Gbalkw_corpus: clean_Gbalkwcorp

clean_Gbalkwcorp <- clean_Gbalkwcorpus(Gbalkw_corpus)

# Print out a cleaned up Global keyword

clean_Gbalkwcorp[[24]][1]

"
Make a term-document matrix
The TDM is often the matrix used for language analysis.
This is because you likely have more terms than authors 
or documents and life is generally easier when you have more rows than columns.
An easy way to start analyzing the information is to change the matrix into 
a simple matrix using as.matrix() on the TDM.
"

# Create a TDM from clean_Gbalkwcorp: Gbalkw_tdm
Gbalkw_tdm <- TermDocumentMatrix(clean_Gbalkwcorp)

# Print Gbalkw_tdm data
Gbalkw_tdm

# Convert Gbalkw_tdm to a matrix: Gbalkw_m
Gbalkw_m<- as.matrix(Gbalkw_tdm)

# Print the dimensions of the matrix
dim(Gbalkw_m)


# Review a portion of the matrix
Gbalkw_m[ 60:65, 10:20]


# Create a matrix: Gbalkw_m

# Calculate the rowSums: Gbalterm_frequency
Gbalterm_frequency <- rowSums(Gbalkw_m)

# Sort Gbalterm_frequency in descending order
Gbalterm_frequency <- sort(Gbalterm_frequency, decreasing = T)

# View the top 10 most common words
Gbalterm_frequency[1:10]

# View the head of  most common words
# head(term_frequency)


# word clouds


Gbalkw_word_freqs <- data.frame(
  term = names(Gbalterm_frequency),
  num = Gbalterm_frequency
)


head(Gbalkw_word_freqs)


# The wordcloud package is loaded

# Create a wordcloud for the values in word_freqs
wordcloud(Gbalkw_word_freqs$term, Gbalkw_word_freqs$num,
          max.words = 100, 
          colors = c("grey80","blue4", "red")
          )

# Create word network
word_associate(
  kwdf$Keyword,
  match.string = c("flight"),
  stopwords = c(Top200Words),
  network.plot = T,
  cloud.colors = c("gray85", "darkred")
)

# Add title
title(main = "Airfrance Flight word association")

# Teaser: simple word clustering

Gbalkw_tdm2 <- removeSparseTerms(Gbalkw_tdm, sparse = 0.98)

print(Gbalkw_tdm2)
dim(Gbalkw_tdm2)

hc <- hclust(d = dist(Gbalkw_tdm2, method = "euclidean"), method = "complete")

# Plot a dendrogram
plot(hc)

# dendextend library is loaded

# Create hcd
hcd <- as.dendrogram(hc)

# Print the labels in hcd
labels(hcd)

# Change the branch color to red for "marvin" and "gaye"
hcd <- branches_attr_by_labels(hcd, c("air", "france"), color = "green")

# Plot hcd
plot(hcd)

# Add cluster rectangles 
rect.dendrogram(hcd, k = 2, border = "grey50")


# Using word assiciation

# Create associations
associations <- findAssocs(Gbalkw_tdm, "paris", 0)

# View the venti associations
associations

# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
head(associations_df)

# Plot the associations_df values (don't change this)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  ggtitle("Word Associations to Paris") + 
  theme_gdocs()

