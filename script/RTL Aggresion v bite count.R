#crappy rates of aggression on bite count by food type


setwd("/Users/teague/Documents/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Bite Count")

btag<-read.csv("/Users/teague/Documents/Dissertation/Dissertation Data/JWatch_data/summarized analyses/Bite Count/bite_agg.csv")
str(btag)
fdfg.fd<-fdfg[fdfg$fd_fg=="fd",]
fdfg.fg<-fdfg[fdfg$fd_fg=="fg",]

library(lme4)
library(multcomp)
#GLM
bt.gl<-lmer(bite~hr.aggrt+Foodtype+(Repro.Season|An.ID), data=btag)
bt.gl0<-lmer(bite~hr.aggrt+(Repro.Season|An.ID), data=btag)
bt.gl1<-lmer(bite~Foodtype+(Repro.Season|An.ID), data=btag)
bt.gl2<-lmer(bite~hr.aggrt*Foodtype+(Repro.Season|An.ID), data=btag)
bt.gl3<-lmer(bite~1+(Repro.Season|An.ID), data=btag)

plot(bite~hr.aggrt, data=btag)

anova(bt.gl2,bt.gl3)
anova(bt.gl, bt.gl0)
anova(bt.gl0,bt.gl3)

