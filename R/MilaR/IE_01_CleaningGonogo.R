library(dplyr)
library(tidyr)
library(lubridate)

#### create data ####

new_gonogo <- read.csv('data/gonogo_Q.csv', stringsAsFactors=F)

#### subset each ####

# replace NA in column in informed consent with values from column informed_consent2
new_gonogo$informed_consent <- ifelse(new_gonogo$Consent == "", new_gonogo$Consent2, new_gonogo$Consent)
#new_gonogo$informed_consent <- ifelse(is.na(new_gonogo$informed_consent), new_gonogo$informed_consent2, new_gonogo$informed_consent)

# keep those who agreed to participate
new_gonogo <- new_gonogo[which(new_gonogo$informed_consent == 'I agree to participate in this study'),]

#new_gonogo <- new_gonogo %>% 
#  select(-informed_consent2)

# remove those that didn't passed screening
new_gonogo <- new_gonogo[which(new_gonogo$passedscreening == "TRUE"),]

# remove those who need to wear corrective devices to see screen and not wearing them now (33)
new_gonogo <- new_gonogo[which(new_gonogo$GlassesWearing != 'No'),]

# keep those who have finished the session
#new_gonogo <- new_gonogo[which(new_gonogo$finished == 'TRUE'),]

# fill NA with other value in the same column informed consent and same id
new_gonogo$Neurological <- ifelse(new_gonogo$Neurological == "", NA, new_gonogo$Neurological)

new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Neurological, .direction = "downup")

# remove people with neurological conditions, except migraine
new_gonogo <- subset(new_gonogo, (Neurological == "No" | 
                                  WhatNeurological == "headache and migraine " | 
                                  WhatNeurological == "Migraines" |
                                  WhatNeurological == "Chronic Migraines" |
                                  WhatNeurological == "Chronic Migraines " |
                                  WhatNeurological == "no" |
                                  WhatNeurological == "No" |
                                  WhatNeurological == "none"))

#new_gonogo <- new_gonogo %>% 
#  select(-neurological_condition_choice)

# remove opiate users
new_gonogo <- new_gonogo[which(new_gonogo$Opiates == "No"),]

## fill in sex and age
new_gonogo$Sex <- ifelse(new_gonogo$Sex == "", NA, new_gonogo$Sex)
new_gonogo$Age <- ifelse(new_gonogo$Age == "", NA, new_gonogo$Age)

new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Sex, .direction = "downup")


# group by id and replace Age with latest year
new_gonogo <- new_gonogo %>%
  group_by(id) %>%
  mutate(Age = if(all(is.na(Age))) NA else max(Age, na.rm = TRUE)) %>%
  ungroup()

# check if there are discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(Age, na.rm = TRUE) > 1)

# check if there are discrepancies by sex
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(Sex, na.rm = TRUE) > 1)

# fill the handedness
new_gonogo$Handedness <- ifelse(new_gonogo$Handedness == "", NA, new_gonogo$Handedness)

new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Handedness, .direction = "downup")

new_gonogo$PhysicallyActive <- ifelse(new_gonogo$PhysicallyActive == "", NA, new_gonogo$PhysicallyActive)

# fill in the physical activity by max
new_gonogo <- new_gonogo %>%
  group_by(id) %>%
  mutate(PhysicallyActive = ifelse(all(is.na(PhysicallyActive)), NA_real_, mean(PhysicallyActive, na.rm = TRUE))) %>%
  ungroup()

# check if there are discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(PhysicallyActive, na.rm = TRUE) > 1)

# fill in the stress by down
new_gonogo$Stressed <- ifelse(new_gonogo$Stressed == "", NA, new_gonogo$Stressed)

new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Stressed, .direction = "downup")

# change stressed to average reported
new_gonogo <- new_gonogo %>%
  group_by(id) %>%
  mutate(Stressed = ifelse(all(is.na(Stressed)), NA_real_, mean(Stressed, na.rm = TRUE))) %>%
  ungroup()

# fill the videogames by down
new_gonogo$VideoGames <- ifelse(new_gonogo$VideoGames == "", NA, new_gonogo$VideoGames)

new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(VideoGames, .direction = "downup")

# check if there are discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(VideoGames, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  mutate(VideoGames = ifelse("Yes" %in% VideoGames, "Yes", VideoGames)) %>%
  ungroup()

# change sleep time to average reported
new_gonogo <- new_gonogo %>%
  group_by(id) %>%
  mutate(Sleep = ifelse(all(is.na(Sleep)), NA_real_, mean(Sleep, na.rm = TRUE))) %>%
  ungroup()

# fill the concussions by down
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Concussion, .direction = "downup")

# check discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(Concussion, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  mutate(Concussion = ifelse("Yes" %in% Concussion, "Yes", Concussion)) %>%
  ungroup()

# fill music instrument by down
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(Music, .direction = "downup")

# check discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(Music, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  mutate(Music = ifelse("Yes" %in% Music, "Yes", Music)) %>%
  ungroup()

# fill ancestry by down
#new_gonogo <- new_gonogo %>% 
#  group_by(id) %>% 
#  fill(ancestry, .direction = "down")

# check discrepancies
#discrepancies <- new_gonogo %>%
#  group_by(id) %>%
#  filter(n_distinct(ancestry, na.rm = TRUE) > 1)

# too few observations (228) -- drop
#new_gonogo <- new_gonogo %>% 
#  select(-ancestry, -ancestry_other)

# fill education by down
#new_gonogo <- new_gonogo %>% 
#  group_by(id) %>% 
#  fill(education, .direction = "down")

## too few observations (301) -- drop
#new_gonogo <- new_gonogo %>% 
#  select(-education)

# fill used by down
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(CannabisUse, .direction = "downup")

# check discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(CannabisUse, na.rm = TRUE) > 1)

discrepancies <- discrepancies %>%
  select(id, CannabisUse, everything())

# change to yes's if at least once responded as yes
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  mutate(CannabisUse = ifelse("Yes" %in% CannabisUse, "Yes", CannabisUse)) %>%
  ungroup()

new_gonogo$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  new_gonogo$cannabis_freqnum[which(new_gonogo$CannabisUseHowOften == freqlist[idx])] <- idx
}

new_gonogo$CannabisUseHowOften <- ifelse(new_gonogo$CannabisUseHowOften == "", NA, new_gonogo$CannabisUseHowOften)

new_gonogo <- new_gonogo %>%
  mutate(cannabis_freqnum = if_else(is.na(CannabisUseHowOften), NA_real_, cannabis_freqnum))

new_gonogo <- new_gonogo %>%
  select(id, CannabisUse, CannabisUseHowOften, cannabis_freqnum, everything())

# fill used by down
new_gonogo <- new_gonogo %>% 
  group_by(id) %>% 
  fill(cannabis_freqnum, .direction = "downup")

# check discrepancies
discrepancies <- new_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
new_gonogo <- new_gonogo %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

new_gonogo <- new_gonogo %>% mutate(cannabis_group = cannabis_freqnum,
                                      cannabis_group = case_when(CannabisUse =="No" ~ "Non-users",
                                                                 cannabis_freqnum > 5 ~ "Frequent users",
                                                                 cannabis_freqnum > 0 & cannabis_freqnum < 6  ~ "Infrequent users"))
new_gonogo$cannabis_group <- factor(new_gonogo$cannabis_group, ordered = FALSE)
new_gonogo$cannabis_group <- relevel(new_gonogo$cannabis_group, ref = "Non-users")

new_gonogo$group <- new_gonogo$sample

gonogo <- new_gonogo


# Perform linear regression
model <- lm(dprime ~ sample, data = new_gonogo)

options(scipen=999)

# View summary of regression model
summary(model)



