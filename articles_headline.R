
###################################################################################################################

###-------------------------------- USA_Today_WEB SCRAPER --------------------------------### 

USATODAY_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.gnt_ar_b'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.gnt_ar_b_p'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_USA_Today_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
  
}

###-------------------------------- Washington Examiner_WEB SCRAPER --------------------------------### 

Wash_Exam_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Examiner_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Washington_Times_WEB SCRAPER --------------------------------### 

Wash_Times_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Times_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- Wash_Post_WEB SCRAPER --------------------------------### 

Wash_Post_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Post_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- WSJ_WEB SCRAPER --------------------------------### 

WSJ_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_WSJ_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------RCP WEB SCRAPER --------------------------------### 

RCP_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'blockquote') %>% html_nodes('p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'blockquote'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_RCP_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- Raw_Story_WEB SCRAPER --------------------------------### 


Raw_Story_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Raw_Story_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------Politico_ WEB SCRAPER --------------------------------### 
Politico_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Politico_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Nyt_WEB SCRAPER --------------------------------### 
NYT_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.StoryBodyCompanionColumn') %>% html_nodes('p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], ' .css-4w7y5l'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.e2kc3sl0') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.clearfix') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'text'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.evys1bk0'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "")
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_NYT_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}
###-------------------------------- NewsWeek_WEB SCRAPER --------------------------------### 
Newsweek_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.js-subbuzz__title-text'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Newsweek_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###-------------------------------- Huffington_Post_WEB SCRAPER --------------------------------### 


HP_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_HP_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- The_Hill_WEB SCRAPER --------------------------------### 

Hill_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Hill_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------Fox WEB SCRAPER --------------------------------### 



Fox_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Fox_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- CNN_WEB SCRAPER --------------------------------### 


CNN_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.l-container') %>% html_nodes('.zn-body__paragraph'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.zn-body__paragraph'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '#storytext') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.clearfix') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'em') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'text'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.inline-placeholder'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'span'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_cnn_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###-------------------------------- Info_Wars_WEB SCRAPER --------------------------------### 
Infowars_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.css-1k8kgo7') %>% html_nodes('p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'strong') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'strong'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'text'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.zn-body__paragraph'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '#__next'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_IW_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- BBT_WEB SCRAPER --------------------------------### 


BBT_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_BBT_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Daily_Caller_WEB SCRAPER --------------------------------### 

DC_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_DailyCaller_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}




###--------------------------------Daily_Bease WEB SCRAPER --------------------------------### 


DB_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Daily_Beast_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}




###-------------------------------- Daily_Kos_WEB SCRAPER --------------------------------### 

DailyKos_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'blockquote') %>% html_nodes('p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'blockquote'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Daily_Kos_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###--------------------------------Gateway_Pundit_ WEB SCRAPER --------------------------------### 


Gateway_Pundit_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'blockquote') %>% html_nodes('p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h3'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Gateway_Pundit_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###--------------------------------MSNBC WEB SCRAPER --------------------------------### 

MSNBC_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.mb0'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.dekText___3cncc') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.clearfix') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.mol-para-with-font'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_MSNBC_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}





###--------------------------------SLATE_WEB SCRAPER --------------------------------### 

SLATE_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.slate-graf') )) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.article__content'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.ot-close-icon') %>% html_nodes('.slate-graf'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.slate-graf') %>% html_nodes('p'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.slate-paragraph') %>% html_nodes('p'))) %>%
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_slate_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}






###-------------------------------- NPR_WEB SCRAPER --------------------------------### 


NPR_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_NPR_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###--------------------------------VOX WEB SCRAPER --------------------------------### 

VOX_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.c-entry-content'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'text'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_VOX_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------RANDOM NEWS WEB SCRAPER --------------------------------### 

Random_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'text'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h3') %>% html_nodes('h2')  %>% html_nodes('h1'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------TOTAL WEB SCRAPER --------------------------------### 

Total_Scraper<- function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1, platform=FALSE)
  
{
  
  if(platform=="New York Times"){return(NYT_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Kos"){return(DailyKos_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Raw Story"){return(Raw_Story_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Vox"){return(VOX_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="HuffPost"){return(HP_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Slate"){return(SLATE_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Newsweek"){return(Newsweek_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Beast"){return(DB_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="NPR"){return(NPR_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="MSNBC"){return(MSNBC_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="ABC News"){return(Random_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Post"){return(Wash_Post_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="CNN"){return(CNN_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Politico"){return(Politico_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="USA Today"){return(USATODAY_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Hill"){return(Hill_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Wall Street Journal"){return(WSJ_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="RealClearPolitics"){return(RCP_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Examiner"){return(Wash_Exam_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Fox News"){return(Fox_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Caller"){return(DC_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="InfoWars"){return(Infowars_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Breitbart"){return(BBT_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="GatewayPundit"){return(Gateway_Pundit_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Times"){return(Wash_Times_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="FALSE"){return(Random_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  
  if (!(as.character(platform) %in% P3))
  {
    return(Random_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  
}


###--------------------------------HEADLINE SCRAPERS --------------------------------### 



##-------------------------------- USA_Today_HEADLINE SCRAPER --------------------------------### 

USATODAY_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.gnt_ar_hl'))) %>%
      paste(collapse = '')
    
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_nodes (hold[[i]], '.gnt_cw') %>% html_nodes('.gnt_ar_hl')) %>%
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')}
    as.character() 
    
  
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_USA_Today_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
  
}

###-------------------------------- Washington Examiner_HEADLINE SCRAPER --------------------------------### 

Wash_Exam_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.ArticlePage-headline'))) %>%
      paste(collapse = '')
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Examiner_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Washington_Times_HEADLINE SCRAPER --------------------------------### 

Wash_Times_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'p'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Times_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- Wash_Post_HEADLINE SCRAPER --------------------------------### 

Wash_Post_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'span'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Wash_Post_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- WSJ_HEADLINE SCRAPER --------------------------------### 

WSJ_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.WSJTheme-module--card__headline--label--JP7Q_1Ds'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.bigTop__hed'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.wsj-article-headline'))) %>% 
      paste(collapse = '')}
    as.character()   
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_WSJ_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------RCP HEADLINE SCRAPER --------------------------------### 

RCP_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'span'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.kgyLAI'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_RCP_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###-------------------------------- Raw_Story_HEADLINE SCRAPER --------------------------------### 


Raw_Story_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.custom-post-headline'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Raw_Story_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------Politico_ HEADLINE SCRAPER --------------------------------### 
Politico_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.headline'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Politico_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Nyt_HEADLINE SCRAPER --------------------------------### 
NYT_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '#link-7ff63a68'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.e1h9rw200'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "")
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_NYT_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}
###-------------------------------- NewsWeek_HEADLINE SCRAPER --------------------------------### 
Newsweek_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.title'))) %>%
      paste(collapse = '')
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Newsweek_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###-------------------------------- Huffington_Post_HEADLINE SCRAPER --------------------------------### 


HP_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.headline__title'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_HP_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- The_Hill_HEADLINE SCRAPER --------------------------------### 

Hill_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '#page-title'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Hill_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------Fox HEADLINE SCRAPER --------------------------------### 



Fox_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.headline'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Fox_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- CNN_HEADLINE SCRAPER --------------------------------### 


CNN_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.banner-text--natural') )) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.pg-headline'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '#js-0-headlineText-jumbotron-card-0'))) %>% 
      paste(collapse = '')}
   
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_cnn_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###-------------------------------- Info_Wars_HEADLINE SCRAPER --------------------------------### 
Infowars_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.css-1f72nq5'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.entry-title'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_IW_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- BBT_HEADLINE SCRAPER --------------------------------### 


BBT_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_BBT_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###-------------------------------- Daily_Caller_HEADLINE SCRAPER --------------------------------### 

DC_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_DailyCaller_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}




###--------------------------------Daily_Bease HEADLINE SCRAPER --------------------------------### 


DB_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Daily_Beast_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}




###-------------------------------- Daily_Kos_HEADLINE SCRAPER --------------------------------### 

DailyKos_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.designation-staff'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  
  ## organizes data frame ##
  # necessary for printing (see below)
  # Dates have been collected, but they are not properly being ported into the data-frame; THIS NEEDS TO BE CHECKED
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Daily_Kos_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}



###--------------------------------Gateway_Pundit_ HEADLINE SCRAPER --------------------------------### 


Gateway_Pundit_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.entry-title'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Gateway_Pundit_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###--------------------------------MSNBC HEADLINE SCRAPER --------------------------------### 

MSNBC_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'di'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.cardTitle___2kTZx'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.lh-none'))) %>% 
      paste(collapse = '')}
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.mol-para-with-font'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_MSNBC_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}





###--------------------------------SLATE_HEADLINE SCRAPER --------------------------------### 

SLATE_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], '.article__hed') )) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>% 
      paste(collapse = '')}

    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_slate_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}






###-------------------------------- NPR_HEADLINE SCRAPER --------------------------------### 


NPR_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_nodes (hold[[i]], '.contentheader--one') %>% html_nodes('.branding--seamus')  %>% html_nodes('.branding__outer')) %>%
      paste(collapse = '')}
    as.character() 
    
    str_replace_all(articleText[[i]], "[\n]", "") 
    
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_NPR_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}


###--------------------------------VOX HEADLINE SCRAPER --------------------------------### 

VOX_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_VOX_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------RANDOM NEWS HEADLINE SCRAPER --------------------------------### 

Random_Headline_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1)
{
  
  require(dplyr)
  require(stringr)
  require(RcppRoll)
  require(boilerpipeR)
  require(rvest)
  storyChar <- as.character(storyChar) #create a dataframe of story URLs as characters
  storyChar
  hold <- list(0)
  articleText <-list(0)
  timestamp <-list(0)
  timestampHold <- list(0)
  if(storyURL==FALSE){num=1}else {num <- length(storyURL[[1]])}
  num
  
  
  for (i in 1:num) {try(hold[[i]] <- read_html(storyChar[i]))} 
  for (i in 1:num) {
    articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h1'))) %>%
      paste(collapse = '')
    if (articleText[[i]] == ""){articleText[[i]] <- try(html_text(html_nodes (hold[[i]], 'h2'))) %>% 
      paste(collapse = '')}
    as.character() 
    str_replace_all(articleText[[i]], "[\n]", "") 
    print(paste("Processing article number: ", as.character(i), sep = ""))} #main loop
  
  
  articleText
  
  if(!(storyURL==FALSE)){ articleDF <- data.frame(storyID = as.character(storyURL[,StoryID_column]), headline = as.character(storyURL[,headline_column]), matrix(unlist(articleText), nrow = num), publish_time = as.character(storyURL[,date_column]))
  articleDF$stringAsFactors <- NULL
  names(articleDF)[3] <- 'text'
  names(articleDF)[4] <- 'time'
  tm<-as.Date(Sys.time())
  keyword<- paste(keyword,"_Headline_txt_", tm, ".csv", sep = "")
  write.csv(articleDF, file = keyword)
  return(articleDF)} else {return(articleText)}
}

###--------------------------------TOTAL HEADLINE SCRAPER --------------------------------### 

Total_Headline_Scraper<- function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID_column=1,headline_column=1,date_column=1, platform=FALSE)
  
{
  
  if(platform=="New York Times"){return(NYT_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Kos"){return(DailyKos_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Raw Story"){return(Raw_Story_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Vox"){return(VOX_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="HuffPost"){return(HP_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Slate"){return(SLATE_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Newsweek"){return(Newsweek_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Beast"){return(DB_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="NPR"){return(NPR_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="MSNBC"){return(MSNBC_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="ABC News"){return(Random_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Post"){return(Wash_Post_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="CNN"){return(CNN_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Politico"){return(Politico_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="USA Today"){return(USATODAY_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Hill"){return(Hill_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Wall Street Journal"){return(WSJ_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="RealClearPolitics"){return(RCP_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Examiner"){return(Wash_Exam_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Fox News"){return(Fox_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Daily Caller"){return(DC_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="InfoWars"){return(Infowars_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Breitbart"){return(BBT_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="GatewayPundit"){return(Gateway_Pundit_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="Washington Times"){return(Wash_Times_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  if(platform=="FALSE"){return(Random_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  
  if (!(as.character(platform) %in% P3))
  {
    return(Random_Headline_Scraper(storyURL,storyChar, keyword,StoryID_column,headline_column,date_column))}
  
}



######################################################################
 df<- storyURL


len<- length(df[,1])
P1<-c("New York Times","Daily Kos","Raw Story","Vox","HuffPost","Slate","Newsweek","NPR","MSNBC","ABC News","Washington Post","Politico","Hill","Wall Street Journal","RealClearPolitics","Washington Examiner","Fox News","Daily Caller","InfoWars","GatewayPundit","Washington Times")
P2<-c("Breitbart", "USA Today", "CNN", "Daily Beast") 
P3<- c("Breitbart", "USA Today", "CNN", "Daily Beast", "New York Times","Daily Kos","Raw Story","Vox","HuffPost","Slate","Newsweek","NPR","MSNBC","ABC News","Washington Post","Politico","Hill","Wall Street Journal","RealClearPolitics","Washington Examiner","Fox News","Daily Caller","InfoWars","GatewayPundit","Washington Times")

kw<- "FB_C"
dftoput<- FALSE #(FALSE FOR MULTIPLe PUBLICATION, DF FOR 1 PUBLICATION)

sID = 2
tme = 3
pf=11
hl=4
Article<-data.frame()
Articles<-data.frame()
for (i in 176463:len)
{print(paste("rvest loop in progress: article",as.character(i), "out of ", as.character(len), " articles"))
  if (dftoput==FALSE){
    articleText<- Total_Scraper(storyURL=dftoput,storyChar=as.character(df$Expanded.Link[i]), keyword=kw,StoryID_column=sID,headline_column=hl,date_column=tme, platform=df$Platform[i])
    headlineText<- Total_Headline_Scraper(storyURL=dftoput,storyChar=as.character(df$Expanded.Link[i]), keyword=kw,StoryID_column=sID,headline_column=hl,date_column=tme, platform=df$Platform[i])
    
    Article<-data.frame(storyID = as.character(df[i,sID]), matrix(unlist(headlineText)), matrix(unlist(articleText), nrow = length(articleText)), publish_time = as.character(df[i,tme]), platform=df[i,pf] )
    
    Article$stringAsFactors <- NULL
    names(Article)[2] <- 'headline'
    names(Article)[3] <- 'text'
    names(Article)[4] <- 'time'
    Articles<- rbind(Articles, Article)
  }
  else
  {
    Article<- Total_Scraper(storyURL=dftoput,storyChar=df$Expanded.Link, keyword=kw,StoryID_column=sID,headline_column=hl,date_column=tme, platform=df$Platform[i])
    
    Articles<- Article
    Articles$platform<-df$Platform[1]
    break
  }}
write.csv(Articles, "File_name_total.csv")

