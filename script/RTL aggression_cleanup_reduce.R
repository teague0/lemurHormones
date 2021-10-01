#Aggression and submission data summarized by day. This includes the new aggression submission data generated 2021-09-28
library(tidyverse)


#load aggsub & mark from DataAccess

#Marking data is by file, need to create daily sums
markDay <- mark %>% group_by(an.id = `Animal ID`, sex = Sex, date = as.Date(Date)) %>% 
  summarize(age.mo = mean(`Age(Mo)`),
            age.wk = mean(`Age(Week)`),
            anogen.mark = sum(`Anogen Mark`),
            wrist.mark = sum(`Wrist Mark`),
            tail.anoint = sum(`Tail annoint`),
            tail.waive = sum(`Tail waive`), 
            nFiles = n())
adults <- which(is.na(markDay$age.mo))
markDay$age.mo[adults] <- "adult"
markDay$age.wk[adults] <- "adult"

aggMark <- aggsub %>% left_join(markDay, by = c("an.id", "date"))


#Write this to a google sheet to be pulled in later
library(googlesheets4)
library(googledrive)

ss1 <- gs4_create("AggressionSubmissionMarking_RTL", sheets = aggMark)
drive_mv("AggressionSubmissionMarking_RTL", path = "~/Integrative Movement Lab/Project Data/Adriyan Blue/GBIO 450 - Blue/Data/")



