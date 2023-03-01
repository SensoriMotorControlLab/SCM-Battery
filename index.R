library(tidyverse)
library(ggplot2)
# download gonogo data from OSF

getOSFdata(2020, "fall", "gonogo")

# download questionnaire files and quality files
getOSFdata(2020, "fall", "questionnaires")
getOSFdata(2020, "fall", "quality")

# try to get out a transformed df

df <- getGroupPerformance(2020, "fall", "gonogo")

#### create data based on Assel's ####

source('R/CleaningQ1.R')

# calculate use frequency
q1$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  q1$cannabis_freqnum[which(q1$cannabis_use_frequency == freqlist[idx])] <- idx
}

#### merging with tasks ####

gng <- df[which(df$passedscreening == TRUE),]
gng   <- merge(q1, gng,   by.x='id', by.y='participant', all=FALSE)

# write that into data for now
write.csv(gng, 'data/2020/mila_gonogo.csv', row.names=FALSE)

#### calculate mean by groups ####

gng %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(dprime), list(name = mean))

df %>%
  summarise_at(vars(dprime), list(name = mean))

# Box plot by group
ggplot(gng, aes(x = cannabis_group, y = dprime, fill= cannabis_group)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "NoGo", x = "")+ 
  scale_x_discrete(labels=c("Frequent User\n (n=47)", "Infrequent User\n (n=94)", "Non-User\n (n=174)"))+ 
  theme(legend.position = "none")   
