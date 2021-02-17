library("mediacloudr")

library("devtools")


# add API key to your R environment file.
# Open your .Renviron file. The file is usually located in your home directory. If the file does not exist, just create one and name it .Renviron.
# Add a new line and enter your API key in the following format: MEDIACLOUD_API_KEY=<YOUR_API_KEY>.
# Save the file and restart your current R session to start using mediacloudr.
#left

dftot<-data.frame()
years<-c("2020", "2016")#Year
mont<-c("01","02","03","04","05","06","07","08","09","10","11","12")#Month
mon<- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31 , 30, 31)#Days of the month


count=0

for(yr in  years)
{
  for (mo in 1:length(mon))
  {mt=mon[mo]-1
  m=mont[mo]
  for (i in 1:mt){
    
    istr=as.character(i)
    istr2=as.character(i+1)
    if (i<9){
      istr=paste("0",istr, sep="")
      istr2=paste("0",istr2, sep="")
      #print(istr)
    }
    mediaright<- "(media_id:6443 OR media_id:101 OR media_id:25444 OR media_id:1092 OR media_id:18775 OR media_id:18515 OR media_id:19334 OR media_id:1722)"
    medialeft<- "(media_id:115 OR media_id:1127 OR media_id:104828 OR media_id:27502 OR media_id:4425 OR media_id:56510 OR media_id:1707 OR media_id:1096 OR  media_id:1149)"
    mediacenter<- "(media_id:39000 OR media_id:1 OR media_id:2 OR media_id:1095 OR media_id:18268 OR media_id:4 OR media_id:18364 OR media_id:1150 OR media_id:1040)"
    mediatotal<- "(media_id:6443 OR media_id:101 OR media_id:25444 OR media_id:1092 OR media_id:18775 OR media_id:18515 OR media_id:19334 OR media_id:1722 OR media_id:115 OR media_id:1127 OR media_id:104828 OR media_id:27502 OR media_id:4425 OR media_id:56510 OR media_id:1707 OR media_id:1096 OR  media_id:1149 OR media_id:39000 OR media_id:1 OR media_id:2 OR media_id:1095 OR media_id:18268 OR media_id:4 OR media_id:18364 OR media_id:1150 OR media_id:1040)"
    
    
    media_tot<- mediaright#REPLACE
    keywords<-"(text:metoo OR text:'me too movement')" #REPLACE
    key<- paste(keywords," AND ",mediatot," AND publish_date:[", yr, "-", m,"-",istr,"T00:00:00.000Z TO ", yr, "-", m,"-",istr,"T06:00:00.000Z]", sep="")
    print(key)
    df <- get_story_list(rows = 1000,
                         fq = key,
                         api_key = "***********************")
    print(length(df$url))
    if (count==0)
    {dftot<-df
    count=1}
    else
    {dftot<- rbind(dftot, df)}
    
    key<- paste(keywords," AND ",mediatot," AND publish_date:[", yr, "-", m,"-",istr,"T06:00:00.000Z TO ", yr, "-", m,"-",istr,"T12:00:00.000Z]", sep="")
    print(key)
    df <- get_story_list(rows = 1000,
                         fq = key,
                         api_key = "***********************")
    print(length(df$url))
    if (count==0)
    {dftot<-df
    count=1}
    else
    {dftot<- rbind(dftot, df)} 
    
    key<- paste(keywords," AND ",mediatot," AND publish_date:[", yr, "-", m,"-",istr,"T12:00:00.000Z TO ", yr, "-", m,"-",istr,"T18:00:00.000Z]", sep="")
    print(key)
    df <- get_story_list(rows = 1000,
                         fq = key,
                         api_key = "***********************")
    print(length(df$url))
    
    if (count==0)
    {dftot<-df
    count=1}
    else
    {dftot<- rbind(dftot, df)}
    key<- paste(keywords," AND ",mediatot," AND publish_date:[", yr, "-", m,"-",istr,"T18:00:00.000Z TO ", yr, "-", m,"-",istr2,"T00:00:00.000Z]", sep="")
    print(key)
    df <- get_story_list(rows = 1000,
                         fq = key,
                         api_key = "***********************")
    print(length(df$url))
    if (count==0)
    {dftot<-df
    count=1}
    else
    {dftot<- rbind(dftot, df)}
  }
  }
}

dfnew_2020_fin<-dftot
