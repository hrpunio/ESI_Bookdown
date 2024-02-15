library("ggplot2");
library("dplyr")
library("ggpubr")

d <- read.csv("fertility_rate_2003_2018.csv", sep = ';',  header=T, na.string="NA");

p1 <- ggplot(d, aes(x=as.factor(yr), y=frate, fill=as.factor(yr))) + 
  geom_violin() + 
  geom_boxplot(width=0.1) +
  ylab("frate") + xlab("years");

p2 <- ggplot(d, aes(x=as.factor(yr), y=frate, fill=as.factor(yr))) + 
  geom_boxplot() + 
  ylab("frate") + xlab("years");

p12 <- ggarrange(p1, p2, ncol=2, nrow=1)
ggsave(plot=p12, file="PPViolinCCY.png", width=11 )

d <- read.csv("fertility_rate_2003_2018cc.csv", sep = ';',  header=T, na.string="NA");
d <- d %>% filter(yr == 2018 )  %>% as.data.frame

p3 <- ggplot(d, aes(x=contname, y=frate, fill=contname)) + 
  geom_violin() + 
  geom_boxplot(width=0.1) +
  ylab("frate") + xlab("years");

p4 <- ggplot(d, aes(x=contname, y=frate, fill=contname)) + 
  geom_boxplot() + 
  ylab("frate") + xlab("years");

p34 <- ggarrange(p3, p4, ncol=2, nrow=1)
ggsave(plot=p34, file="PPViolinCC.png", width=11 )

#ggsave(file="fertRatio.png")
