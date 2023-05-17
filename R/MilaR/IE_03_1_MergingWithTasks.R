library(data.table)
library(stringr)

#### merge questionnaire with tasks ####

#### gonogo ####

## duplicates done on the same day (-70)
df_list[[1]] <- df_list[[1]][!duplicated(df_list[[1]][c("id", "sdate")]), ]

#df_list[[1]] <- df_list[[1]] %>% 
#  select(-startdate, -enddate)

df_gonogo <- merge(df_combined, df_list[[1]], by = c("id"))
#2636

df_gonogo <- df_gonogo[df_gonogo$date_1 >= df_gonogo$startdate & df_gonogo$date_1 <= df_gonogo$enddate, ]
#984

df_gonogo <- df_gonogo[!duplicated(df_gonogo[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#984

df_gonogo <- df_gonogo[!duplicated(df_gonogo[c("id", "date_1")], fromLast = TRUE), ]
#956


#### wrangling ####

# replace NA in column in informed consent with values from column informed_consent2
df_gonogo$informed_consent <- ifelse(df_gonogo$informed_consent == "", NA, df_gonogo$informed_consent)
df_gonogo$informed_consent <- ifelse(is.na(df_gonogo$informed_consent), df_gonogo$informed_consent2, df_gonogo$informed_consent)

# keep those who agreed to participate
df_gonogo <- df_gonogo[which(df_gonogo$informed_consent == 'I agree to participate in this study'),]
#956

df_gonogo <- df_gonogo %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_gonogo[columns_to_process] <- lapply(df_gonogo[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_gonogo <- df_gonogo %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
      handedness, glasses_contacts, wearing_glasses_now, 
      physically_activity, opiates, video_games, used, use_frequency,
      concussion, music, .direction = "downup")

# keep only finished sessions
df_gonogo <- df_gonogo[which(df_gonogo$finished == 'TRUE'),]
#929
df_gonogo <- df_gonogo[which(df_gonogo$passedscreening == 'TRUE'),]
#898

# remove those who need to wear corrective devices to see screen and not wearing them now
df_gonogo <- df_gonogo[which(df_gonogo$wearing_glasses_now != "No" | is.na(df_gonogo$wearing_glasses_now)), ]
#884

df_gonogo <- df_gonogo %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
df_gonogo <- subset(df_gonogo, (neurological_conditions == "No" | 
                                      neurological_condition_description == "headache and migraine " | 
                                      neurological_condition_description == "Migraines" |
                                      neurological_condition_description == "Chronic Migraines" |
                                      neurological_condition_description == "Chronic Migraines " |
                                      neurological_condition_description == "no" |
                                      neurological_condition_description == "No" |
                                      neurological_condition_description == "none"))
#841

df_gonogo <- df_gonogo %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_gonogo <- df_gonogo[which(df_gonogo$opiates == "No" | is.na(df_gonogo$opiates)),]
#787

#### set up users ####

# change to yes's if at least once responded as yes
df_gonogo <- df_gonogo %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_gonogo$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_gonogo$cannabis_freqnum[which(df_gonogo$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_gonogo$cannabis_freqnum <- ifelse(is.na(df_gonogo$used), NA, df_gonogo$cannabis_freqnum)

df_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_gonogo <- df_gonogo %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_gonogo <- df_gonogo %>% mutate(cannabis_group = cannabis_freqnum,
                                  cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                             cannabis_freqnum > 5 ~ "Frequent users",
                                                             TRUE ~ "Infrequent users"))
df_gonogo$cannabis_group <- factor(df_gonogo$cannabis_group, ordered = FALSE)
df_gonogo$cannabis_group <- relevel(df_gonogo$cannabis_group, ref = "Non-users")


df_gonogo$group <- df_gonogo$sample
gonogo <- df_gonogo

## testing the demogr

#merged_df_gonogo  <- merge(df_combined %>%
#  select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#         video_games, music, year_of_birth), merged_df_gonogo, by = c("id"))

#### visual search ####

df_high <- getGroupPerformance("2023", "fall", "visualsearch")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_vs <- merge(df, df_high, by = c("id"))

merged_df_vs <- merged_df_vs %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_vs  <- merged_df_vs  %>%
  filter(date_diff < 1)

## merging with demographic

#merged_df_vs  <- merge(df_combined %>%
#       select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#              video_games, music, year_of_birth), merged_df_vs, by = c("id"))

#### taskswitching ####

df_high <- getGroupPerformance("2023", "fall", "taskswitching")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_taskswitching <- merge(df, df_high, by = c("id"))

merged_df_taskswitching <- merged_df_taskswitching %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_taskswitching  <- merged_df_taskswitching  %>%
  filter(date_diff < 1)

## merge with demographic

#merged_df_taskswitching  <- merge(df_combined %>%
#        select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#               video_games, music, year_of_birth), merged_df_taskswitching, by = c("id"))

#### tunneling ####

df_high <- getGroupPerformance("2023", "fall", "tunneling")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_tunneling <- merge(df, df_high, by = c("id"))

merged_df_tunneling <- merged_df_tunneling %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_tunneling  <- merged_df_tunneling  %>%
  filter(date_diff < 1)

## merge with demographic

#merged_df_tunneling  <- merge(df_combined %>%
#        select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#               video_games, music, year_of_birth), merged_df_tunneling, by = c("id"))

#### trailMaking ####

df_high <- getGroupPerformance("2023", "fall", "trailMaking")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_trailmaking <- merge(df, df_high, by = c("id"))

merged_df_trailmaking <- merged_df_trailmaking %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_trailmaking  <- merged_df_trailmaking  %>%
  filter(date_diff < 1)

## merging with demographic

#merged_df_trailmaking  <- merge(df_combined %>%
#        select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#               video_games, music, year_of_birth), merged_df_trailmaking, by = c("id"))

#### nBack ####

df_high <- getGroupPerformance("2023", "fall", "nBack")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_nback <- merge(df, df_high, by = c("id"))

merged_df_nback <- merged_df_nback %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_nback  <- merged_df_nback  %>%
  filter(date_diff < 1)

## merging with demographic

#merged_df_nback  <- merge(df_combined %>%
#                                  select(id, cannabis_freqnum, cannabis_group, sex, physically_activity,
#                                         video_games, music, year_of_birth), merged_df_nback, by = c("id"))

## not yet

# Perform linear regression
model <- lm(dprime ~ how_high, data = merged_df_gonogo)

# View summary of regression model
summary(model)


