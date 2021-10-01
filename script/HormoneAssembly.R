#Hormone assembly so that I only have to do this once. ***Nope. Needs more fixing***

#Read andro, e2, gc from DataAccess
#gc was cleaned up & is ready to go.
#e2 was cleaned but I'm not sure what the duplicate values are from
#andro. duplicate assay in rows, but the overall mean wasn't calculated. need to talk to S about this.

sampleInfo <- read.csv("~/ownCloud/Ring-tailed lemur/Catta Hormones/LCatta sample list update2.csv")
ageClasses <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "AgeWeeksClass")
seasons <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "seasons")

sampleInfo$collectionDate <- mdy_hm(paste(sampleInfo$dat.us, sampleInfo$time, sep=" "))
sampDat <- sampleInfo %>% select(no, an.id, collectionDate, mass.g, sex, age.mo, age.wks)


#Clear out the redundant info. There are 2 rows for each sample, but a mean, SD & CV are given in the first row.

e2




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