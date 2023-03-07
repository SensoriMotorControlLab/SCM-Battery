library(tidyverse)
library(ggplot2)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

# download gonogo data from OSF: RUN ONCE

getOSFdata(2020, "fall", "gonogo")
getOSFdata(2020, "fall", "visualsearch")

# download questionnaire files and quality files: RUN ONCE
getOSFdata(2020, "fall", "questionnaires")
getOSFdata(2020, "fall", "quality")

# try to get out a transformed df

gng_df <- getGroupPerformance(2020, "fall", "gonogo")
vs_df <- getGroupPerformance(2020, "fall", "visualsearch")

#### create data based on Assel's ####

source('R/MilaR/CleaningQ1.R')

# calculate use frequency
# 7: daily
# 6: a few times a week
# 5: once a week
# 4: a few times a month
# 3: around once a month
# 2: once or twice within these past 3 months
# 1: not in the past 3 months
# 0: [empty / never]

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

# calculate cannabis groups

q1 <- q1 %>% mutate(cannabis_group = cannabis_freqnum,
                    cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                            cannabis_freqnum > 5 ~ "Frequent users",
                                            TRUE ~ "Infrequent users"))

#### merging with tasks ####

gng_df <- gng_df[which(gng_df$passedscreening == TRUE),]
vs_df <- vs_df[which(vs_df$passedscreening == TRUE),]
df   <- merge(q1, gng_df,   by.x='id', by.y='participant', all=FALSE)
df   <- merge(df, vs_df,   by.x='id', by.y='participant', all=FALSE)

# write that into data for now
write.csv(df, 'data/2020/mila_full_df.csv', row.names=FALSE)

#### calculate mean by groups ####

df %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(dprime), list(name = mean))

df %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))


df %>%
  summarise_at(vars(dprime), list(name = mean))

# Box plot by group
ggplot(df, aes(x = cannabis_group, y = dprime, fill= cannabis_group)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "NoGo", x = "")+ 
  scale_x_discrete(labels=c("Frequent User\n (n=47)", "Infrequent User\n (n=94)", "Non-User\n (n=174)"))+ 
  theme(legend.position = "none")  

# line segment graph

