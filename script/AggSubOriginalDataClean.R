library(tidyverse)
library(readxl)
library(lubridate)

#I'm suspicious of the aggressions / submission summariers that were used in the past, so I'd like to go back to raw data and re-calculate them.

dat <- read_excel("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/SAS output/final_data.xlsx") #all of the behavioral data
nobs <- read_excel("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/SAS output/Number of Observation Files.xlsx", col_types = "text") #summaries of how much each animal was observed.
nobsX <- read_excel("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/SAS output/Number of Observation Files.xlsx") #summaries of how much each animal was observed.
filenameDate <- read_excel("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/SAS output/Feed by part filename ID date.xlsx", sheet = "Part & beh by file", col_types = "text") #behavior data are classed by filename -- no other IDS

filenameDate <- read.csv("~/ownCloud/Dissertation/Dissertation Data/JWatch_data/SAS output/Feed by part filename ID date.csv")
filenameDate <- filenameDate %>% select(an.id = AnID, date = Date, filename = Filename)
filenameDate$date <- mdy(filenameDate$date)


#The definitions of the behavior codes are here: "~/ownCloud/Dissertation/Dissertation Data/Teague behaviors modifiers.xls"

#Create a summary of the aggression & submission rates for each individual per observation day.
#Aggression: Q (1 stare, 2 move/lunge/chase, 3 contact win) 
#Submission: q (1 look away, 2 move/jump/flee, 3 contact lose)
# directed at focal: >

#For this, the COUNT information is the most useful. The total duration of time observed can be taken from the nobs summaries or summed up separately. It's just hard to do that & filter in 1 step

nameDateinfo <- filenameDate %>% select(an.id, date, filename)

aggsub <- dat %>% filter(str_detect(behavior, "Q") | str_detect(behavior, "q"))
aggress <- which(str_detect(aggsub$behavior, "Q"))
aggressRec <- which(str_detect(aggsub$behavior, "Q>"))
submit <- which(str_detect(aggsub$behavior, "q"))
submitRec <- which(str_detect(aggsub$behavior, "q>"))
aggsub$aggSub <- NA
aggsub$aggSub[aggress] <- "aggress"
aggsub$aggSub[aggressRec] <- "aggress.rec"
aggsub$aggSub[submit] <- "submit"
aggsub$aggSub[submitRec] <- "submit.rec"

aggsub <- aggsub %>% left_join(nameDateinfo)

#Fix some IDs
idFix <- as.data.frame(which(is.na(aggsub$an.id)))
idFix$filename <- aggsub$filename[idFix$`which(is.na(aggsub$an.id))`]
names(idFix) <- c("index", "filename")
idFix$filenameOG <- idFix$filename
idFix$filename <- str_remove(idFix$filename, ".dat")
idFix <- idFix %>% separate(filename, c("group", "obs", "date", "an.id"), "-",extra = "drop")

#Still need to join these back to the main dataa
idFix$groupAnID <- paste0(idFix$group, "_", idFix$an.id)

#Some IDs from idFix need to be clarified:
idFix$an.id[which(idFix$groupAnID=="b_jf2")] <- "JF(217)"
idFix$an.id[which(idFix$groupAnID=="g_167b")] <- "167"
idFix$an.id[which(idFix$groupAnID=="g_sam")] <- "335"
idFix$an.id[which(idFix$groupAnID=="o_jf1")] <- "JF(154)"
idFix$an.id[which(idFix$groupAnID=="o_jf2")] <- "JF(300)"
idFix$an.id[which(idFix$groupAnID=="o_saf1")] <- "326"
idFix$an.id[which(idFix$groupAnID=="o_saf2")] <- "329"
idFix$an.id[which(idFix$groupAnID=="o_sam")] <- "331"
idFix$an.id[which(idFix$groupAnID=="p_sam1")] <- "JF(328)"
idFix$an.id[which(idFix$groupAnID=="p_umm1")] <- "UMM1-P"
idFix$an.id[which(idFix$groupAnID=="p_UMM1")] <- "UMM1-P"
idFix$an.id[which(idFix$groupAnID=="p_UMM2")] <- "UMM2-P"
idFix$an.id[which(idFix$groupAnID=="r_jf")] <- "JF(44)"
idFix$an.id[which(idFix$groupAnID=="t_2")] <- "312"
idFix$an.id[which(idFix$groupAnID=="t_jf")] <- "JF(202)"
idFix$an.id[which(idFix$groupAnID=="t_JF1")] <- "JF(202)"
idFix$an.id[which(idFix$groupAnID=="t_umm1")] <- "UMM1-T"
idFix$an.id[which(idFix$groupAnID=="t_UMM1")] <- "UMM1-T"
idFix$an.id[which(idFix$groupAnID=="y_3")] <- "JM(155)"
idFix$an.id[which(idFix$groupAnID=="y_jm")] <- "JM(155)"
idFix$an.id[which(idFix$groupAnID=="y_JM")] <- "JM(155)"
idFix$an.id[which(idFix$groupAnID=="y_JM1")] <- "JM(155)"

#Fix the date column
#change sept to sep

idFix$date <- str_replace(idFix$date, "sept", "sep")

#adding (?i) ignores case
jan <- which(str_detect(idFix$date, "(?i)jan") == TRUE)
feb<-  which(str_detect(idFix$date, "(?i)feb")== TRUE)
mar<-  which(str_detect(idFix$date, "(?i)mar")== TRUE)
apr<-  which(str_detect(idFix$date, "(?i)apr")== TRUE)
may<-  which(str_detect(idFix$date, "(?i)may")== TRUE)
jun<-  which(str_detect(idFix$date, "(?i)jun")== TRUE)
jul<-  which(str_detect(idFix$date, "(?i)jul")== TRUE)
aug<-  which(str_detect(idFix$date, "(?i)aug")== TRUE)
sep<-  which(str_detect(idFix$date, "(?i)sep")== TRUE)
oct<-  which(str_detect(idFix$date, "(?i)oct")== TRUE)
nov<-  which(str_detect(idFix$date, "(?i)nov")== TRUE)
dec<-  which(str_detect(idFix$date, "(?i)dec")== TRUE)

y2009 <- c(jun, jul, aug, sep, oct, nov, dec)
y2010 <- c(jan, feb, mar, apr, may)
idFix$date[y2009] <- paste0(idFix$date[y2009], "2009")
idFix$date[y2010] <- paste0(idFix$date[y2010], "2010")
idFix$date <- as.Date(idFix$date, "%d%b%Y")
#There are some NAs. pull them & fix
nas <- which(is.na(idFix$date)) #The date is in the obs column
idFix$date[nas] <- as.Date(paste0(idFix$obs[nas], "2009"), "%d%b%Y")

filesfixed <- which(aggsub$filename %in% idFix$filenameOG)
aggsub$an.id[filesfixed] <- idFix$an.id
aggsub$date[filesfixed] <- idFix$date
b303 <- which(aggsub$an.id == "303") #blue 303 becomes 332
aggsub$an.id[b303] <- "332"
umm2 <-  which(aggsub$an.id == "umm2")
aggsub$an.id[umm2] <- "UMM2-P"

#aggsub is now complete with the date, animal ID & counts of behaviors per observation file. This can be summarized by day now to calculate a daily rate


aggsubDay <- aggsub %>% group_by(an.id, date, aggSub) %>% 
  summarize(nOccurence = sum(COUNT)) %>% 
  pivot_wider(names_from = aggSub, values_from = nOccurence)

nobs$date <- as.Date(nobsX$Date)


dailyObsTime <- nobs %>% select(an.id = `An ID`, date, obs.wk, season = Season, obsTime = `Total Jwatch Time Observed`)

aggsubDay <- aggsubDay %>% left_join(dailyObsTime, by = c("an.id", "date"))

#Convert agg/sub counts to rates per hour. Time observed is in milliseconds. Divide by 60000 to get minutes, 3600000 for hours

aggsubDay$obsTime <- as.numeric(aggsubDay$obsTime)

aggsubDay <- aggsubDay %>% 
  mutate(
  aggressRt.h = aggress / (obsTime / 3600000),
  submitRt.h = submit / (obsTime / 3600000),
  aggressRecRt.h = aggress.rec / (obsTime / 3600000),
  submitRecRt.h = submit.rec / (obsTime / 3600000),
)

#save this to google drive
library(googledrive)
ss1 <- gs4_create("AggressionSubmission_RTL", sheets = aggsubDay)
drive_mv("AggressionSubmission_RTL", path = "~/Integrative Movement Lab/Project Data/Adriyan Blue/GBIO 450 - Blue/Data/")

