


## Check for packages

list.of.packages <- c("wordcloud","wordcloud2","lubridate", "syuzhet","shiny","dplyr","tm","SnowballC","tidyverse","htmltools","htmlwidgets","radarchart","ggplot2","ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(wordcloud)
library(tm)
library(SnowballC) 
library(ggplot2)
library(ggthemes)
library(tidytext)
library(tidyverse)
library(syuzhet)
library(lubridate)

billboard_lyrics_1964.2015<-read.csv("billboard_lyrics_1964-2015.csv",stringsAsFactors = FALSE)


songs<-data.frame(billboard_lyrics_1964.2015,stringsAsFactors = FALSE)
songs$Lyrics<- as.character(songs$Lyrics)
songs$Song<- as.character(songs$Song)
songs$Artist<- as.character(songs$Artist)
## Check for missing values

colSums(is.na(songs)) ## Handling Missing Values in Lyrics column

##  Drop all Missing value rows


songs<-songs[!(is.na(songs$Lyrics) | songs$Lyrics==" " | songs$Lyrics=="  "), ]

## ReCheck for missing values

colSums(is.na(songs))

### EDA for Question 4

# Convert to tibble
song_lyrics <- as_tibble(songs[c(2:5)])

# Tidy dataset - lyrics
tidy_lyrics <-  song_lyrics %>%
  # Group by the songs and years
  group_by(Year,Song) %>%
  # Define a new column linenumber
  mutate(linenumber=row_number()) %>%
  # Transform the non-tidy text data to tidy text data. Default "words" token.
  unnest_tokens(word, Lyrics) %>%
  ##Remove Stopwords
  anti_join(stop_words) %>% 
  count(word,sort=TRUE) %>%
  ungroup()

##Performing Sentiment analysis using Bing lexicon

lyric_sentiment_bing <-  tidy_lyrics %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing"))

lyric_tidy_sentiment <- lyric_sentiment_bing %>% 
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive -negative)


#########################  QUESTION 1, 2 , 3 ,4 ###############################################


##
ui<-fluidPage(
  # Title of the app
  #titlePanel("Tabsets"),
  tabsetPanel(
    
    tabPanel(
      titlePanel("Artist Hits Per Rank"),
      
      sidebarLayout(
        sidebarPanel(
          
          # Select song rank
          selectInput("Rank", "Choose a rank :",
                      choices=c(1:100),selected = 1),
          
          # ##Select a year
          # selectInput("time", "Choose a year :",
          #             choices=c(1965:2015),selected = 1965 ),
          
          actionButton("update","Create Bar Plot")
        ),
        mainPanel(
          plotOutput("Boxplot")
        )
      )
      
      
    ), 
    tabPanel(
      
      titlePanel("Frequently Used Words"),
      sidebarLayout(
        sidebarPanel(
          # Select input for year1
          selectInput("year1", "Choose a year :",
                      choices=c(1965:2015),selected = 1965 ),
          
          
          # Select input for second year
          # Select input for year
          selectInput("year2", "Choose a year :",
                      choices=c(1965:2015),selected = 1965 ),
          
          # Select input for frequency of words in title or lyrics
          selectInput("option", "Choose an option to see word frquency in title/lyrics:",
                      choices=c("Title","Lyrics"),selected ="Title"),
          
          
          # # Slider input for word frequency
          sliderInput("fre", "Frequency:",
                      min = 1, max = 50, value = 1),
          # 
          # # Slider input for number of words 
          sliderInput("maxw", "Max words:",
                      min = 10, max = 1000, value = 50),
          ## Order of words
          checkboxInput("order","Random Order"),
          
          ## Colour theme options
          selectInput("theme", "Select the wordcloud color theme",
                      choices=c("Accent","Dark2","Set1","Set2"),selected ="Dark2"),
          
          
          actionButton("frequent","Create Plot")
        ),
        ##show plot
        mainPanel(
          
          plotOutput("FrequencyWC")
        ))
      
    ),
    
    
    tabPanel(
      titlePanel(" Sentiment Word Cloud"),
      sidebarLayout(
        sidebarPanel(
          
          # Select input for year
          selectInput("decade", "Choose a Decade :",
                      choices=c("1970s","1980s","1990s","2000s","2010s"),selected = "1970s" ),
          
          # Select word sentiment
          selectInput("sentiment", "Choose a sentiment :",
                      choices=c("negative","positive"),selected = "positive" ),
          
          
          # # Slider input for word frequency
          sliderInput("freq", "Frequency:",
                      min = 1, max = 50, value = 1),
          # 
          # # Slider input for number of words 
          sliderInput("maxw", "Max words:",
                      min = 10, max = 1000, value = 50),
          ## Order of words
          checkboxInput("order","Random Order"),
          
          ## Colour theme options
          selectInput("theme", "Select the wordcloud color theme",
                      choices=c("Accent","Dark2","Set1","Set2"),selected ="Dark2"),
          
          
          actionButton("cloud","Create Sentiment Word Cloud")
        ) ,
        ##show word cloud
        mainPanel(
          
          plotOutput("wcsentimentplot")
        ))
    )
    
  )
)



##


server<-function(input, output) {
  
  # TAB 1
  bar_data<- reactive({
    
    
    if(songs$Rank==input$Rank)
    {
      bar_file<-songs%>% filter(count(Artist)>=1)
    }
    
    else{
      NULL
    }
  })
  
  ## TAB 2
  
  wc_data1 <- reactive({
    
    if (input$option=="Lyrics"){
      
      wc_file1<-subset(songs$Lyrics,songs$Year==input$year1)
      
    }
    else{
      
      ## worcloud for title
      wc_file1<-subset(songs$Song,songs$Year==input$year1)
      
      
      
    }
    
    wc_corpus1<-Corpus(VectorSource(wc_file1))
    wc_corpus1 <- tm_map(wc_corpus1, stripWhitespace)
    #wc_corpus1 <- tm_map(wc_corpus1, content_transformer(tolower))
    wc_corpus1 <- tm_map(wc_corpus1, removeWords, stopwords("english"))
    wc_corpus1 <- tm_map(wc_corpus1, removeNumbers)
    wc_corpus1 <- tm_map(wc_corpus1, removePunctuation)
    
    
    
    
  })
  
  
  wc_data2 <- reactive({
    
    if (input$option=="Lyrics"){
      
      wc_file2 <- subset(songs$Lyrics,songs$Year==input$year2)
      
      
      # 
    }
    else{
      
      wc_file2<- subset(songs$Song,songs$Year==input$year2)
      
    }
    
    wc_corpus2<-Corpus(VectorSource(wc_file2))
    wc_corpus2 <- tm_map(wc_corpus2, stripWhitespace)
    #wc_corpus2 <- tm_map(wc_corpus2, content_transformer(tolower))
    wc_corpus2 <- tm_map(wc_corpus2, removeWords, stopwords("english"))
    wc_corpus2 <- tm_map(wc_corpus2, removeNumbers)
    wc_corpus2 <- tm_map(wc_corpus2, removePunctuation)
    
    
    
  })
  
  wordcloud_rep<-repeatable(wordcloud)
  
  
  
  
  
  ## TAB 3
  
  wc_sentiment_data <- reactive({
    
    
    
    if (input$decade=="1970s"){
      
      wc_file11<-subset(lyric_sentiment_bing[,(3)], lyric_sentiment_bing$sentiment==input$sentiment& lyric_sentiment_bing$Year<1980)
    }
    else if  (input$decade=="1980s") {
      wc_file11<-subset(lyric_sentiment_bing[,3], lyric_sentiment_bing$sentiment==input$sentiment& lyric_sentiment_bing$Year>1980 & lyric_sentiment_bing$Year <1990)
      
    }
    else if  (input$decade=="1990s") {
      wc_file11<-subset(lyric_sentiment_bing[,3], lyric_sentiment_bing$sentiment==input$sentiment& lyric_sentiment_bing$Year>1990 & lyric_sentiment_bing$Year <2000)
      
    }
    else if  (input$decade=="2000s") {
      wc_file11<-subset(lyric_sentiment_bing[,3], lyric_sentiment_bing$sentiment==input$sentiment& lyric_sentiment_bing$Year>2000 & lyric_sentiment_bing$Year <2010)
    }                     
    else{
      wc_file11<-subset(lyric_sentiment_bing[,3], lyric_sentiment_bing$sentiment==input$sentiment& lyric_sentiment_bing$Year>2010)
    }
    
    wc_file11<- Corpus(VectorSource(wc_file11))
    #wc_file11 <- tm_map(wc_file11, content_transformer(tolower))
    wc_file11 <- tm_map(wc_file11, removeNumbers)
    wc_file11 <- tm_map(wc_file11, removePunctuation)
    
  })
  
  
  wordcloud_rep<-repeatable(wordcloud)
  
  ##### Data for output 
  
  
  
  ## TAB 1
  output$Boxplot<- renderPlot({
    if(input$update){
      isolate({
        withProgress({
          setProgress(message = "Processing  Box plot ...")
          
          total_hits<-  songs %>%
            group_by(Artist,Rank,Year) %>%
            summarise(n_times_in_100 = n())%>%
            ungroup()
          
          total_hits%>% filter(Rank==input$Rank)%>%ggplot(aes(x=reorder(Artist,-n_times_in_100,FUN = sum),y=  n_times_in_100 , fill=Artist , group=Rank))+
            geom_bar(stat="identity")+
            xlab("Artists Name ")+
            ylab(" Number of hits ")+
            
            theme(axis.text.x = element_text(angle = 90 , hjust = 1))+
            theme(legend.position="None")
          
        })
      })
    }
  })
  
  ### TAB 2 
  output$FrequencyWC<- renderPlot({
    
    if(input$frequent){
      isolate({
        
        withProgress({
          setProgress(message = "Processing Wordcloud ...")
          
          
          wc_corpus1<- wc_data1()
          wc_corpus2<-wc_data2()
          
          
          
          wc_color=brewer.pal(8,"Set2")
          
          if(input$theme=="Accent"){
            wc_color=brewer.pal(8,"Accent")
          }
          else if(input$theme=="Dark2"){
            wc_color=brewer.pal(8,"Dark2")
          }
          else if(input$theme=="Set1"){
            wc_color=brewer.pal(9,"Set1")
          }
          else{
            wc_color=brewer.pal(8,"Set2")
          }
          
          par(mfrow=c(1,2))
          wordcloud(wc_corpus1,min.freq = input$fre, max.words=input$maxw,colors=wc_color,random.order= input$order,rot.per = 0.25)
          
          wordcloud(wc_corpus2,min.freq = input$fre, max.words=input$maxw,colors=wc_color,random.order= input$order,rot.per = 0.25)
          
          
          
        })
      })
    } 
    
  }) 
  ## TAB 3
  output$wcsentimentplot<- renderPlot({
    
    if(input$cloud){
      isolate({
        
        withProgress({
          setProgress(message = "Processing  Corpus...")
          
          wc_file11<-wc_sentiment_data()
          
          wc_color=brewer.pal(8,"Set2")
          
          if(input$theme=="Accent"){
            wc_color=brewer.pal(8,"Accent")
          }
          else if(input$theme=="Dark2"){
            wc_color=brewer.pal(8,"Dark2")
          }
          else if(input$theme=="Set1"){
            wc_color=brewer.pal(9,"Set1")
          }
          else{
            wc_color=brewer.pal(8,"Set2")
          }
          wordcloud(wc_file11,min.freq = input$freq, max.words=input$maxw,colors=wc_color,random.order= input$order,rot.per = 0.25)
          #wordcloud2(wc_sentiment_data(),shuffle = TRUE,minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
          #rotateRatio = 1)
        })
      })
      
    }
  })
}

# # Create a Shiny app object
shinyApp(ui = ui, server = server)



