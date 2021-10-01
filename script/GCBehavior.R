#GC + behavior
library(tidyverse)
library(googlesheets4)
library(lubridate)


#Add in some demographic data. I want to calculate ages based on known / estimate birth dates
demos <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "ID & Demo")

#That file is a mess, so reduce & clean up some basic info
lemurs <- demos %>% select(an.id = "Animal ID...1", 
                           group = "Group", 
                           sex = "Sex", 
                           birthDate = "Birth Date",
                           gestate = "Gestate?",
                           lactate = "Lactate (more than 4 wks)?",
                           babyBirthDay = "Baby Born...21",
                           babyMissingDay = "Date Baby Missing",
                           babySex = "Baby Sex...37")
lemurs <- as.data.frame(lemurs)
lemurs$an.id <- str_replace(lemurs$an.id,"[(]","")
lemurs$an.id <- str_replace(lemurs$an.id,"[)]","")
lemurs$an.id <- str_to_upper(lemurs$an.id)
lemurs$birthDate <- date(lemurs$birthDate)
lemurs$babyBirthDay <- date(lemurs$babyBirthDay)

#Clean IDs
#AggMark
aggMark <- read_sheet("https://docs.google.com/spreadsheets/d/1PqbPrsczjsCdvDUyyvYnmQtUjznnhA9XdFXHk-Wlj7w/edit?usp=sharing")
aggMark$an.id <- str_replace(aggMark$an.id,"[( )]","")
aggMark$an.id <- str_to_upper(aggMark$an.id)
aggMark$an.id <- str_replace(aggMark$an.id,"[( )]","")
unique(aggMark$an.id)
aggMark <- aggMark %>% left_join(lemurs, by = "an.id")
aggMark$date <- date(aggMark$date)
aggMark$age.wk <- as.numeric(round((aggMark$date - aggMark$birthDate)/7))
aggMark$week.yr <- week(aggMark$date)

gc <- read_sheet("https://docs.google.com/spreadsheets/d/12j8bzWNEdZ3SdTUqrbkXAa5Sb-JFNn25EkFakMO-G2E/edit?usp=sharing")
gc$an.id <- str_to_upper(gc$an.id)
gc$date <- as.Date(gc$collectionDate)
gc$week.yr <- week(gc$date)
gc <- gc %>% left_join(lemurs, by = "an.id")
gc$age.wk <- as.numeric(round((gc$date - gc$birthDate)/7))
gc$age.2wk <- 2*ceiling(gc$age.wk/2)


#I'm going to split these into animals we know their age in weeks to those we don't (adults > 4)
gc.2wkavg <- gc %>% group_by(an.id, age.2wk) %>%
  summarize(minDate = min(date),
            min.wkyr = min(week.yr),
            mean.ngg = mean(ng.g, na.rm = T),
            sd.ngg = sd(ng.g), na.rm = T,
            nobs = n())

#Aggression / submission measures by 2 week intervals
aggMark$age.wk <- as.numeric(aggMark$age.wk)
aggMark$age.2wk <- 2*ceiling(aggMark$age.wk/2)


#Create rates based on age aggregates of 2 weeks. The adults (> 208 weeks) are all lumped together and will have to be treated separately.
aggMrk.2wk <- aggMark %>%
  group_by(an.id, age.2wk) %>% 
  summarize(minDate = min(date),
            min.wkyr = min(week.yr),
            min.agewk = min(age.wk),
            obsTime.h = sum(obsTime)/3600000,
            wkAggress.rt = sum(aggress, na.rm =T)/obsTime.h,
            wkSubmit.rt = sum(submit, na.rm =T)/obsTime.h,
            wkAggRcv.rt = sum(aggress.rec, na.rm =T)/obsTime.h,
            wkSubRcv.rt = sum(submit.rec, na.rm =T)/obsTime.h)

#Match GC to behavior
aggGC_full <- aggMrk.2wk %>% full_join(gc.2wkavg, by = c("an.id", "age.2wk"))
aggGC_full$min.wkyr <- NA
  for(i in 1:length(aggGC_full$min.wkyr.x)){
    aggGC_full$min.wkyr[i] <- min(aggGC_full$min.wkyr.x[i], aggGC_full$min.wkyr.y[i], na.rm = T)}

#Some additional group info
groupComps <- data.frame(group = c("Blue", "Green", "Red", "Orange", "Yellow", "Teal", "Purple"),
                         nFemales = c(10, 8, 6, 15, 7, 7, 9),
                         nMales = c(10,10,7,10,5,4,11))
ageClasses <- data.frame(ageClass = c("1-Inf1", "2-Inf2", "3-Juv1", "4-Juv2", "5-SubAd", "6-Adult"),
                         minAge = c(0, 13, 25, 53, 105, 157),
                         maxAge = c(12, 24, 52, 104, 156, 2000))
reproSeasons <- data.frame(season = c("3-Lactation","4-Weaning", "5-Recovery","1-Mating", "2-Gestation","3-Lactation"),
                           weekStart = c(1, 5, 9, 18, 22, 37),
                           weekEnd =  c(4, 8, 17, 21, 36, 52))

#push the group & demographic data onto the 2wk averages
aggGC_full <- aggGC_full %>% left_join(lemurs, by = "an.id")
aggGC_full <- aggGC_full %>% left_join(groupComps, by = "group")
aggGC_full$min.wkyr.x <- NULL
aggGC_full$min.wkyr.y <- NULL
names(aggGC_full$minDate.x) <- "minDate.behav"
names(aggGC_full$minDate.y) <- "minDate.GC"

aggGC_full$ageClass <- NA
for (i in 1:length(aggGC_full$ageClass)){
  aggGC_full$ageClass[i] <- ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[1] & 
                                     aggGC_full$age.2wk[i] <= ageClasses$maxAge[1], 
                                   ageClasses$ageClass[1],
                                   ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[2] & 
                                            aggGC_full$age.2wk[i] <= ageClasses$maxAge[2], 
                                          ageClasses$ageClass[2],
                                          ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[3] & 
                                                   aggGC_full$age.2wk[i] <= ageClasses$maxAge[3], 
                                                 ageClasses$ageClass[3],
                                                 ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[4] & 
                                                          aggGC_full$age.2wk[i] <= ageClasses$maxAge[4],
ageClasses$ageClass[4],
ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[5] & 
         aggGC_full$age.2wk[i] <= ageClasses$maxAge[5], 
       ageClasses$ageClass[5],
       ifelse(aggGC_full$age.2wk[i] >= ageClasses$minAge[6] & 
                aggGC_full$age.2wk[i] <= ageClasses$maxAge[6], 
              ageClasses$ageClass[6], NA))))))
}

aggGC_full$season <- NA
for (i in 1:length(aggGC_full$season)){
  aggGC_full$season[i] <- ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[1] & 
                                     aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[1], 
                                 reproSeasons$season[1],
                                   ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[2] & 
                                            aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[2], 
                                          reproSeasons$season[2],
                                          ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[3] & 
                                                   aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[3], 
                                                 reproSeasons$season[3],
                                                 ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[4] & 
                                                          aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[4],
                                                        reproSeasons$season[4],
                                                        ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[5] & 
                                                                 aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[5], 
                                                               reproSeasons$season[5],
                                                               ifelse(aggGC_full$min.wkyr[i] >= reproSeasons$weekStart[6] & 
                                                                        aggGC_full$min.wkyr[i] <= reproSeasons$weekEnd[6], 
                                                                      reproSeasons$season[6], NA))))))
}

library(googledrive)
ss1 <- gs4_create("GC_Behavior_RTL 2021 10 01", sheets = aggGC_full)
drive_mv("GC_Behavior_RTL 2021 10 01", path = "~/Integrative Movement Lab/Project Data/Adriyan Blue/GBIO 450 - Blue/Data/")



