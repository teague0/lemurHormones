library(cowplot)


aggout <- ggplot(aggGC_full)+
  geom_point(aes(x = age.2wk, y = log(wkAggress.rt), color = sex))+
  geom_smooth(aes(x = age.2wk, y = log(wkAggress.rt), color = sex))+
  geom_vline(xintercept = c(52, 104, 156))+
  theme_cowplot()
aggout  

aggrec <- ggplot(aggGC_full)+
  geom_point(aes(x = age.2wk, y = log(wkAggRcv.rt), color = sex))+
  geom_smooth(aes(x = age.2wk, y = log(wkAggRcv.rt), color = sex))+
  geom_vline(xintercept = c(52, 104, 156))+
  theme_cowplot()
aggrec  

gc <- ggplot(aggGC_full)+
  geom_point(aes(x = age.2wk, y = log(mean.ngg), color = sex))+
  geom_smooth(aes(x = age.2wk, y = log(mean.ngg), color = sex))+
  geom_vline(xintercept = c(52, 104, 156))+
  theme_cowplot()
gc  

gc <- ggplot(aggGC_full)+
  geom_point(aes(x = log(wkAggRcv.rt), y = log(mean.ngg), color = sex))+
  geom_smooth(aes(x = log(wkAggRcv.rt), y = log(mean.ngg), color = sex))+
  #geom_vline(xintercept = c(52, 104, 156))+
  theme_cowplot()

