library(tidyverse)
library(stringr)
library(zoo)
#library(fuzzyjoin)
library(caret)
library(gbm)
submiddiontem = NULL
submission = NULL
data = read_csv("MNCAATourneyCompactResults.csv")
teamrankingfinal = read_csv("TeamRankingData.csv", guess_max=7000)

#Coerce to correct data frames
# teamrankingfinal$Ratingschedule.strength.by.other=as.double(teamrankingfinal$Ratingschedule.strength.by.other)
# teamrankingfinal$Ratingneutral.by.other=as.double(teamrankingfinal$Ratingneutral.by.other)
# teamrankingfinal$Ratinglast.10.games.by.other=as.double(teamrankingfinal$Ratinglast.10.games.by.other)
# teamrankingfinal$SAGRank=as.numeric(teamrankingfinal$SAGRank)
# teamrankingfinal$POMRank=as.numeric(teamrankingfinal$POMRank)
teamrankingfinal$clust=as.factor(teamrankingfinal$clust)

#Create two rows for each game (team A/B)
dat1 = data[,c(1:6)]
dat2 = data[,c(1:2,5:6,3:4)]

names(dat1)
names(dat2)

dat1 %>% names() %>% str_replace("W","A") %>% str_replace("L","B") -> datnames

names(dat1) = datnames
names(dat2) = datnames

  
datalong = rbind(dat1,dat2)

#Add win column
datalong= datalong%>%mutate(win=ifelse(AScore>BScore,1,0))
#datalong[c(1,82042),]

### Making data for using rating system

# sample = read_csv("SampleSubmissionStage2.csv")
sample = read_csv("MSampleSubmissionStage1_2020.csv")

#Prepare submission to receive team scores from Model
submissionteams = 
  sample %>%
  separate(ID,c("Season","ATeamID","BTeamID"),"_") %>%
  mutate(ATeamID = as.integer(ATeamID)) %>%
  mutate(BTeamID = as.integer(BTeamID)) %>%
  mutate(Season=as.integer(Season)) %>%
  mutate(DayNum=NA, AScore=NA,BScore=NA) %>%
  select(Season,DayNum,ATeamID,AScore,BTeamID,BScore)


datalong2=rbind(dat1,dat2)
# Filter out testset data for Stage 1 submission
datalong2 %>% filter(Season<2015) -> datalong2

#Add in the unknown elements
datalong2=rbind(datalong2,submissionteams)

datalong2=datalong2 %>% mutate(win=ifelse(AScore>BScore,1,0))
datalong2=left_join(datalong2,select(teamrankingfinal,Season,TeamID,Overalloffensive.efficiency, Overalldefensive.efficiency,Ratingschedule.strength.by.other,Ratingneutral.by.other,Ratinglast.10.games.by.other,SAGRank,POMRank,clust),by=c("ATeamID"="TeamID","Season"="Season"))
datalong2=left_join(datalong2,select(teamrankingfinal,Season,TeamID,Overalloffensive.efficiency, Overalldefensive.efficiency,Ratingschedule.strength.by.other,Ratingneutral.by.other,Ratinglast.10.games.by.other,SAGRank,POMRank,clust),by=c("BTeamID"="TeamID","Season"="Season"))



#Make datalong names same as 
test=NULL
test[1:7]=names(datalong2)[1:7]
for (i in 8:23){
  test[i]=names(datalong2)[i]=paste0("A",names(datalong2)[i])
}
names(datalong2)=test


#Add TeamA data
datalong=left_join(datalong,select(teamrankingfinal,-Team),by=c("ATeamID"="TeamID","Season"="Season")) 

#Add 'A' to names of new columns
test=NULL
test[1:7]=names(datalong)[1:7]
for (i in 8:23){
test[i]=names(datalong)[i]=paste0("A",names(datalong)[i])
}
names(datalong2)=test

#Add TeamB data
datalong=left_join(datalong,select(teamrankingfinal,-Team),by=c("BTeamID"="TeamID","Season"="Season"))

#Add 'B' to names of new columns
test[1:23]=names(datalong)[1:23]
for (i in 24:39){
  test[i]=names(datalong)[i]=paste0("B",names(datalong)[i])
}
names(datalong)=test
str(datalong)


###Create model1 using tourney games and non-ranking metrics

model1data=datalong[,c(1,7:17,21:32,36:37)] %>% 
  filter(Season >1997) %>% filter(Season <2014) %>%
  select(-Season)

model1 = glm(win~., data = model1data, family = binomial(link = "logit"))
summary(model1)

###Create model2 using tourney games and non-ranking metrics

model2data=model1data %>% 
  select(-AOverallassist..per..turnover.ratio,-BOverallassist..per..turnover.ratio,-ALast3offensive.efficiency,-ALast3defensive.efficiency,-ALast3possessions.per.game,-BLast3offensive.efficiency,-BLast3defensive.efficiency,-BLast3possessions.per.game)

model2 = glm(win~., data = model2data, family = binomial(link = "logit"))
summary(model2)

model2boost=gbm(win~., data = model2data, distribution = "bernoulli")
summary(model2boost)

###Model 3 with rating data

model3data=datalong2[,c(1,7:23)] %>% 
  # filter(Season >2003) %>% filter(Season < 2020) %>%select(-Season)
  filter(Season >2003) %>% filter(Season < 2015) %>%select(-Season) #for stage 1


model3 = glm(win~., data = model3data, family = binomial(link = "logit"))
summary(model3)

## Kaggle Submission Final  
  
  # sample = read_csv("SampleSubmissionStage2.csv")
  
  #summary(model1)
  
  # ateam = 
  #   teamrankingfinal %>%
  #   select(ATeamID,Arollpoints,ATeamRPI,Acluster,AGaveUp,ATeamPOM) %>%
  #   # select(ATeamID,Arollpoints,ATeamRPI,Acluster,AGaveUp,BTeamID.x,BTeamRPI,Bcluster) %>%
  #   group_by(ATeamID) %>%
  #   filter(row_number()==n()) 
  # 
  # bteamlastgames = 
  #   joined %>%
  #   # select(ATeamID,Arollpoints,ATeamRPI,Acluster,AGaveUp) %>%
  #   select(BTeamID.x,BTeamRPI,Bcluster,BTeamPOM) %>%
  #   group_by(BTeamID.x) %>%
  #   filter(row_number()==n()) %>%
  #   rename(BTeamID=BTeamID.x)
  
kagglejoindata=teamrankingfinal %>% select(-Team,-Ratingschedule.strength.by.other,-Ratingneutral.by.other,-Ratinglast.10.games.by.other)
kagglejoindata2=teamrankingfinal %>% select(-Team,-Ratingschedule.strength.by.other,-Ratingneutral.by.other,-Ratinglast.10.games.by.other,-Overallassist..per..turnover.ratio,-Last3offensive.efficiency,-Last3defensive.efficiency,-Last3possessions.per.game)
kagglejoindata3=teamrankingfinal[,c(2,3,5,13:19)]
kagglejoindatafinal=datalong2[,c(1,3,5,8:23)] %>% 
  filter(Season >=2015)   # Use this for test.
  # filter(Season ==2020) %>% select(-Season)  # Use this for actual submission
  
  #create data for submission of teams  
  subdata = 
    sample %>%
    separate(ID,c("Year","ATeamID","BTeamID"),"_") %>%
    mutate(ATeamID = as.integer(ATeamID)) %>%
    mutate(BTeamID = as.integer(BTeamID)) %>%
    mutate(Year=as.integer(Year)) %>%
    #left_join(kagglejoindatafinal,by=c("ATeamID"="TeamID","Year"="Season")) %>%
    #left_join(kagglejoindatafinal,by=c("BTeamID"="TeamID","Year"="Season"),suffix=c("A","B"))
    # left_join(kagglejoindatafinal,by=c("ATeamID"="ATeamID","BTeamID"="BTeamID")) # Use this for final
    left_join(kagglejoindatafinal,by=c("ATeamID"="ATeamID","BTeamID"="BTeamID", "Year"="Season")) # Use this for Stage 1
    
    #left_join(kagglejoindatafinal,by=c("BTeamID"="BTeamID"))
  names(subdata)=c("Year","ATeamID","BTeamID","Pred",names(model3data)[2:17])  
  
  
  subdata %>% filter(clust.x==8) %>% distinct(ATeamID)
  # submission2014 =
  #   subdata %>%
  #   mutate(Pred = predict.glm(model, subdata, type = "response")) %>%
  #   unite(ID,Year,ATeamID,BTeamID) %>%
  #   select(ID,Pred) %>%
  #   filter(str_sub(ID,1,4)==year)
  
  
  # submission2015 = 
  #   subdata %>%
  #   mutate(Pred = predict.glm(model, subdata, type = "response")) %>%
  #   unite(ID,Year,ATeamID,BTeamID) %>%
  #   select(ID,Pred) %>%
  #   filter(str_sub(ID,1,4)==year)
  
  # submission2016 =
  #   subdata %>%
  #   mutate(Pred = predict.glm(model, subdata, type = "response")) %>%
  #   unite(ID,Year,ATeamID,BTeamID) %>%
  #   select(ID,Pred) %>%
  #   filter(str_sub(ID,1,4)==year)
  
  # submission2017 =
  #   subdata %>%
  #   mutate(Pred = predict.glm(model, subdata, type = "response")) %>%
  #   unite(ID,Year,ATeamID,BTeamID) %>%
  #   select(ID,Pred) %>%
  #   filter(str_sub(ID,1,4)==year)
  
  # submission2018 =
  #   subdata %>%
  #   mutate(Pred = predict.glm(model, subdata, type = "response")) %>%
  #   unite(ID,Year,ATeamID,BTeamID) %>%
  #   select(ID,Pred) %>%
  #   filter(str_sub(ID,1,4)==year)
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
  }
  
subdata=subdata %>% mutate_cond(clust.x=="9",clust.x="6")
subdata=subdata %>% mutate_cond(clust.x=="8",clust.x="6")
subdata=subdata %>% mutate_cond(clust.y=="9" ,clust.y="6")
subdata=subdata %>% mutate_cond(clust.y=="8",clust.y="6")
subdata=subdata %>% mutate_cond(is.na(Ratingneutral.by.other.x),Ratingneutral.by.other.x=0)
subdata=subdata %>% mutate_cond(is.na(Ratingneutral.by.other.y),Ratingneutral.by.other.y=0)

  names(subdata)
  
  submiddiontem =
    subdata %>%
    mutate(Pred = predict(model3, subdata[,5:20],n.trees=10000, type = "response")) %>%
    unite(ID,Year,ATeamID,BTeamID) %>%
    select(ID,Pred)# %>% 
   # filter(str_sub(ID,1,4)==Year)
  
  submission = bind_rows(submission,submiddiontem)
  print(j)

# submission =
#   submission2014 %>%
#   bind_rows(submission2015) %>%
#   bind_rows(submission2016) %>%
#   bind_rows(submission2017) %>%
#   bind_rows(submission2018) 


write.csv(submiddiontem,"PleussKaggleFinal.csv", row.names = FALSE)


  
#left_join(kagglejoindatafinal,by=c("BTeamID"="BTeamID"))


