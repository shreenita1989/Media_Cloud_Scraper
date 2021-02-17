
library(rvest)

Random_Image_Scraper<-function(storyURL=FALSE,storyChar=FALSE, keyword="keyword",StoryID="1", StoryID_column=1,headline_column=1,date_column=1)
{html<- as.character(storyChar)
  y=html  %>% # adjust to your path
    html_nodes("img") %>%
    html_attr("src")
  name_of_file<- paste(keyword, "_", StoryID, ".jpg")
  download.file(y, name_of_file, mode = 'wb')

}
df<- storyURL


len<- length(df[,1])
P1<-c("New York Times","Daily Kos","Raw Story","Vox","HuffPost","Slate","Newsweek","NPR","MSNBC","ABC News","Washington Post","Politico","Hill","Wall Street Journal","RealClearPolitics","Washington Examiner","Fox News","Daily Caller","InfoWars","GatewayPundit","Washington Times")
P2<-c("Breitbart", "USA Today", "CNN", "Daily Beast") 
P3<- c("Breitbart", "USA Today", "CNN", "Daily Beast", "New York Times","Daily Kos","Raw Story","Vox","HuffPost","Slate","Newsweek","NPR","MSNBC","ABC News","Washington Post","Politico","Hill","Wall Street Journal","RealClearPolitics","Washington Examiner","Fox News","Daily Caller","InfoWars","GatewayPundit","Washington Times")

kw<- "keyword"
dftoput<- FALSE #(FALSE FOR MULTIPLe PUBLICATION, DF FOR 1 PUBLICATION)

sID = 2
tme = 3
pf=11
hl=4
Article<-data.frame()
Articles<-data.frame()
for (i in 1:len)
{print(paste("rvest loop in progress: article",as.character(i), "out of ", as.character(len), " articles"))
  if (dftoput==FALSE){
    articleText<- Total_Scraper(storyURL=dftoput,storyChar=as.character(df$Expanded.Link[i]), keyword=kw,StoryID=as.character(df$storyID[i]),StoryID_column=sID,headline_column=hl,date_column=tme, platform=df$Platform[i])
    
}}