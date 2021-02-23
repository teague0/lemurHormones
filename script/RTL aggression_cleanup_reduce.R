#Aggression and submission data summarized by day. 

aggSub <- read.csv("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Aggression Submission/Aggression Submission Cleaned.csv")

library(tidyverse)

aggSub <- rename(aggSub, submit = q, aggress = Q, submit.received = q.receiv, aggress.received = Q.receiv) #rename variables so they are understandable
aggSub <- aggSub %>% select(an.id, group, sex, age, date, season, week.no:N)
#Total time observed is in milliseconds. Divide by 60000 to get minutes. Observation sessions were 12 minutes long.

#Marking summaries by day
mark <- read.csv("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Marking/Marking_age.csv")
markDay <- mark %>% group_by(an.id = Animal.ID, sex = Sex, date = Date, age.mo = Age.Mo., age.wk = Age.Week.,) %>% 
    summarize(anogen.mark = sum(Anogen.Mark),
              wrist.mark = sum(Wrist.Mark),
              tail.anoint = sum(Tail.annoint),
              tail.waive = sum(Tail.waive), 
              minutes.observed = length(cd.res.file)*12)
markDay <- markDay %>% filter(!is.na(anogen.mark))

aggMark <- aggSub %>% left_join(markDay)


#Write this to a google sheet to be pulled in later
library(googlesheets4)
library(googledrive)

ss1 <- gs4_create("AggressionSubmissionMarking_RTL", sheets = aggMark)
drive_mv("AggressionSubmissionMarking_RTL", path = "~/GBIO 450 - Blue/Data/")

