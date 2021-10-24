---
title:
output:
  html_document:
    keep_md: TRUE
---



## Ring-tailed lemur fecal glucocorticoids

Read in processed data


```r
library(tidyverse)
library(googlesheets4)

aggGC_full <- read_sheet("https://docs.google.com/spreadsheets/d/12EB2WXBiqfwHPIQ_2tqOvkV0i8Yzjoc5yhMlkAOtUv0/edit?usp=sharing")
```

## Targeted plots & analysis 
1. Seasonality of GC in adults:
    Do GC's change predictably with reproductive state in females (gestation, lactation, weaning, etc)
    

```r
library(gghalves)
library(lme4)
library(car)

aggGC_full %>% filter(!is.na(season), age.2wk > 150) %>% 
ggplot()+
  geom_half_boxplot(aes(x = season, y = log(mean.ngg), color = sex), side = "r")+
  geom_half_point(aes(x = season, y = log(mean.ngg), color = sex), side = "l", alpha = 0.6)+
  ggtitle("Fecal GC in RTL older than 150 weeks")+
  theme_bw()
```

![](PlotDrafts_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
aggGC_full.noNA <- aggGC_full %>% filter(!is.na(season))

adultSeason.lm <- lmer(log(mean.ngg)~season*sex + (1|an.id), data = aggGC_full.noNA[aggGC_full.noNA$age.2wk > 150, ])
Anova(adultSeason.lm)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: log(mean.ngg)
##             Chisq Df Pr(>Chisq)
## season     0.3891  1     0.5327
## sex        0.3345  1     0.5630
## season:sex 0.2216  1     0.6378
```

```r
summary(adultSeason.lm)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mean.ngg) ~ season * sex + (1 | an.id)
##    Data: aggGC_full.noNA[aggGC_full.noNA$age.2wk > 150, ]
## 
## REML criterion at convergence: 174.5
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.86860 -0.56475  0.02261  0.46662  2.43278 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  an.id    (Intercept) 0.000    0.000   
##  Residual             1.226    1.107   
## Number of obs: 58, groups:  an.id, 13
## 
## Fixed effects:
##                           Estimate Std. Error t value
## (Intercept)                1.33689    0.39154   3.414
## season3-Lactation          0.36310    0.47057   0.772
## sexMale                    0.05169    0.55372   0.093
## season3-Lactation:sexMale -0.30717    0.65256  -0.471
## 
## Correlation of Fixed Effects:
##             (Intr) ssn3-L sexMal
## sesn3-Lcttn -0.832              
## sexMale     -0.707  0.588       
## ssn3-Lctt:M  0.600 -0.721 -0.849
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

A mixed effects linear model shows only a signficant effect of sex on fecal GC's. Not of season, and there is no interaction between season & sex.

2. Aggression rates in adults across season & sex


```r
aggRc <- aggGC_full %>% filter(!is.na(season), age.2wk > 150) %>% 
ggplot()+
  geom_half_boxplot(aes(x = season, y = log(wkAggRcv.rt), color = sex), side = "r")+
  geom_half_point(aes(x = season, y = log(wkAggRcv.rt), color = sex), side = "l", alpha = 0.6)+
  theme_bw()+
  theme(legend.position = c(0.1, 0.85))

aggDt <- aggGC_full %>% filter(!is.na(season), age.2wk > 150) %>% 
ggplot()+
  geom_half_boxplot(aes(x = season, y = log(wkAggress.rt), color = sex), side = "r")+
  geom_half_point(aes(x = season, y = log(wkAggress.rt), color = sex), side = "l", alpha = 0.6)+
  theme_bw()+
    theme(legend.position = "none")

library(cowplot)
plot_grid(aggRc, aggDt, nrow = 2)
```

![](PlotDrafts_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
adultSeasonAggRc.lm <- lmer(wkAggRcv.rt~season*sex + (1|an.id), data = aggGC_full)
Anova(adultSeasonAggRc.lm)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: wkAggRcv.rt
##             Chisq Df Pr(>Chisq)  
## season     7.2956  3    0.06305 .
## sex        0.8951  1    0.34409  
## season:sex 0.4403  3    0.93179  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(adultSeasonAggRc.lm)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: wkAggRcv.rt ~ season * sex + (1 | an.id)
##    Data: aggGC_full
## 
## REML criterion at convergence: 3089.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3112 -0.4767 -0.2469  0.1941 17.8750 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  an.id    (Intercept) 0.07894  0.281   
##  Residual             1.72023  1.312   
## Number of obs: 902, groups:  an.id, 86
## 
## Fixed effects:
##                           Estimate Std. Error t value
## (Intercept)                0.51790    0.11792   4.392
## season3-Lactation          0.21550    0.13610   1.583
## season4-Weaning           -0.02726    0.21231  -0.128
## season5-Recovery           0.37957    0.20122   1.886
## sexMale                    0.18751    0.18223   1.029
## season3-Lactation:sexMale -0.13828    0.21222  -0.652
## season4-Weaning:sexMale   -0.06296    0.32740  -0.192
## season5-Recovery:sexMale  -0.06214    0.31234  -0.199
## 
## Correlation of Fixed Effects:
##             (Intr) ssn3-L ssn4-W ssn5-R sexMal s3-L:M s4-W:M
## sesn3-Lcttn -0.747                                          
## seasn4-Wnng -0.483  0.416                                   
## sesn5-Rcvry -0.514  0.443  0.286                            
## sexMale     -0.647  0.484  0.312  0.333                     
## ssn3-Lctt:M  0.479 -0.641 -0.267 -0.284 -0.748              
## ssn4-Wnng:M  0.313 -0.270 -0.648 -0.185 -0.491  0.419       
## ssn5-Rcvr:M  0.331 -0.285 -0.184 -0.644 -0.515  0.442  0.289
```

3. Fecal GC ~ Age in weeks * sex


```r
agePlot <- aggGC_full %>% dplyr::filter(age.2wk <= 150) %>%
  ggplot()+
  geom_point(aes(x = age.2wk, y = log(mean.ngg), color = sex), alpha = 0.6)+
  geom_smooth(aes(x = age.2wk, y = log(mean.ngg), color = sex))+
  theme_bw()


bplot <- aggGC_full %>% dplyr::filter(age.2wk <= 150) %>%
  ggplot()+
  geom_half_boxplot(aes(x = ageClass, y = log(mean.ngg), color = sex), side = "r")+
  geom_half_point(aes(x = ageClass, y = log(mean.ngg), color = sex), side = "l", alpha = 0.6)+
  theme_bw()

plot_grid(agePlot, bplot, nrow = 2)
```

![](PlotDrafts_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


4. Aggression Received & Dealt ~ Age in weeks * sex


```r
aggPlot <- aggGC_full %>% dplyr::filter(age.2wk <= 150) %>%
  ggplot()+
  geom_point(aes(x = age.2wk, y = log(wkAggress.rt), color = sex), alpha = 0.6)+
  geom_smooth(aes(x = age.2wk, y = log(wkAggress.rt), color = sex))+
  theme_bw()

aggRcPlot <- aggGC_full %>% dplyr::filter(age.2wk <= 150) %>%
  ggplot()+
  geom_point(aes(x = age.2wk, y = log(wkAggRcv.rt), color = sex), alpha = 0.6)+
  geom_smooth(aes(x = age.2wk, y = log(wkAggRcv.rt), color = sex))+
  theme_bw()

bplot <- aggGC_full %>% dplyr::filter(age.2wk <= 150) %>%
  ggplot()+
  geom_half_boxplot(aes(x = ageClass, y = log(mean.ngg), color = sex), side = "r")+
  geom_half_point(aes(x = ageClass, y = log(mean.ngg), color = sex), side = "l", alpha = 0.6)+
  theme_bw()

plot_grid(aggPlot, aggRcPlot, nrow = 2)
```

![](PlotDrafts_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

