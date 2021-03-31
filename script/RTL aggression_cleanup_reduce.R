#Aggression and submission data summarized by day. 

aggSub_orig <- read.csv("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Aggression Submission/Aggression Submission Cleaned.csv")

library(tidyverse)


#Fix duplicated animal / dates. This was from dumb 2011 Teague who used Excel to fill in ages of animals and looks like he used 2 different fills that gave either age in months or an age category cutoff of adult. Rename variables so they are understandable

aggSub <- aggSub_orig %>% 
  group_by(an.id, group, sex, date, season, week.no, total.time.observed) %>% 
  summarize(submit = sum(q),
            aggress = sum(Q), 
            submit.received = sum(q.receiv), 
            aggress.received = sum(Q.receiv), 
            N = sum(N))
  
  
#Total time observed is in milliseconds. Divide by 60000 to get minutes. Observation sessions were 12 minutes long.

aggSub <- aggSub %>% arrange(an.id, date)


#Marking summaries by day
mark <- read.csv("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Marking/Marking_age.csv")
markDay <- mark %>% group_by(an.id = Animal.ID, sex = Sex, date = Date, age.mo = Age.Mo., age.wk = Age.Week.,) %>% 
    summarize(anogen.mark = sum(Anogen.Mark),
              wrist.mark = sum(Wrist.Mark),
              tail.anoint = sum(Tail.annoint),
              tail.waive = sum(Tail.waive), 
              minutes.observed = length(cd.res.file)*12)
markDay <- markDay %>% filter(!is.na(anogen.mark))
markDay <- markDay %>% select(an.id, date, anogen.mark:minutes.observed)

aggMark <- aggSub %>% left_join(markDay)


#Write this to a google sheet to be pulled in later
library(googlesheets4)
library(googledrive)

ss1 <- gs4_create("AggressionSubmissionMarking_RTL", sheets = aggMark)
drive_mv("AggressionSubmissionMarking_RTL", path = "~/GBIO 450 - Blue/Data/")



