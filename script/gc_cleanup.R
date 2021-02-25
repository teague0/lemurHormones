#Glucocorticoid assay clean & fill with relevant sample & demographic ID
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(lubridate)


gc <- read_sheet("https://docs.google.com/spreadsheets/d/1Ny9b_lcgBNHw3TqyL0x3nTbRbjvQKQladaGttdoMclk/edit?usp=sharing")
gc <- gc %>% filter(!is.na(`Sample ID`))
gc <- gc %>% select(`Sample ID`, mean.pg = Mean, stdev.pg = `Std Dev`, CV = `CV (%)`, year.assayed)
gc$mean.pg <- as.numeric(unlist(gc$mean.pg))
gc$stdev.pg <- as.numeric(unlist(gc$stdev.pg))
gc$CV <- as.numeric(unlist(gc$CV))
gc <- gc %>% filter(!is.na(mean.pg))


sampleInfo <- read.csv("~/ownCloud/Ring-tailed lemur/Catta Hormones/LCatta sample list update2.csv")
ageClasses <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "AgeWeeksClass")
seasons <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "seasons")

#get a good date-time for the sample collection
sampleInfo$collectionDate <- mdy_hm(paste(sampleInfo$dat.us, sampleInfo$time, sep=" "))
sampDat <- sampleInfo %>% select(no, an.id, collectionDate, mass.g, sex, age.mo, age.wks)


#Clear out the redundant info. There are 2 rows for each sample, but a mean, SD & CV are given in the first row.



#Need to add in sample details "sample volume used (ul)", "sample.mass", "dilution.factor", date collected, assay date, season
#to calculate: "pg.ml", "pg", "pg.g"  

#Then add in animal details: ID, age in weeks/months, age category, sex

gc <- gc %>% left_join(sampDat, by =c(`Sample ID` = "no"))
gc$pg.ml <- gc$mean.pg/0.5 #pg / 50 uL/1000 (50 uL were used in the assay. Convert to ml)
gc$pg <- gc$pg.ml * 5 #pg/ml x the dilution factor (5) 
gc$pg.g <- gc$pg/gc$mass.g #pg / sample mass
gc$ng.g <- gc$pg.g/1000
gc <- gc %>% left_join(ageClasses, by = c("age.wks" = "age.weeks"))
gc$age.class <- factor(gc$age.class, 
                          levels=c("Infant-1", "Infant-2", "Juvenile-1", "Juvenile-2", "Subadult", "Adult")) #create an ordered factor so it will plot by correct age on X axis
gc$month <- month(gc$collectionDate)
gc <- gc %>% left_join(seasons, by = c("month" = "month.no"))
gc$season <- factor(gc$season,
                    levels = c("Gestation", "Lactation", "Weaning", "Recovery"))

ss1 <- gs4_create("glucocortsCleaned_RTL", sheets = gc)
drive_mv("glucocortsCleaned_RTL", path = "~/GBIO 450 - Blue/Data/")


ggplot(gc, aes(x = age.mo, y = log(pg.g), color= sex))+
  geom_point()

ggplot(gc)+
  geom_boxplot(aes(x = age.class, y = log(pg.g), fill=sex))+
  theme_classic()+
  labs(x = "Age Class", 
       y = expression(paste("Fecal glucocorticoids [ln(pg ", g^-1, ")]", sep= "")))

ggplot(gc)+
  geom_boxplot(aes(x = season, y = log(pg.g), fill = sex))+
  facet_wrap(~age.class)+
  theme_classic()


