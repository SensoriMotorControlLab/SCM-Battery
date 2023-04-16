library(data.table)
library(stringr)

#### merge questionnaire with tasks ####

# part 3 and 4

df <- bind_rows(q_ie_part3_fall, q_ie_part3_winter)
df <- bind_rows(df, q_ie_part4_fall)
df <- bind_rows(df, q_ie_part4_winter)

#### subset each ####

# replace NA in column in informed consent with values from column informed_consent2
df$informed_consent <- ifelse(df$informed_consent == "", NA, df$informed_consent)
df$informed_consent <- ifelse(is.na(df$informed_consent), df$informed_consent2, df$informed_consent)

# keep those who agreed to participate
df <- df[which(df$informed_consent == 'I agree to participate in this study'),]

df <- df %>% 
  select(-informed_consent2)

# remove those who need to wear corrective devices to see screen and not wearing them now (33)
df <- df[which(df$wearing_glasses_now != 'No'),]

df <- df %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# keep those who have finished the session
df <- df[which(df$finished == 'TRUE'),]

df <- df %>% 
  select(-finished)

# fill NA with other value in the same column informed consent and same id
df$neurological_conditions <- ifelse(df$neurological_conditions == "", NA, df$neurological_conditions)

df <- df %>% 
  group_by(id) %>% 
  fill(neurological_conditions, .direction = "down")

# remove people with neurological conditions, except migraine
df <- subset(df, (neurological_conditions == "No" | 
                                      neurological_condition_description == "headache and migraine " | 
                                      neurological_condition_description == "Migraines" |
                                      neurological_condition_description == "Chronic Migraines" |
                                      neurological_condition_description == "Chronic Migraines " |
                                      neurological_condition_description == "no" |
                                      neurological_condition_description == "No" |
                                      neurological_condition_description == "none"))

df <- df %>% 
  select(-neurological_conditions, -neurological_condition_description)

# remove opiate users
df <- df[which(df$opiates == "No"),]

df <- df %>% 
  select(-opiates)

# remove those that didn't passed screening
#df <- df[which(df$passedscreening == "TRUE"),]

# fill in the stress by down
df <- df %>% 
  group_by(id) %>% 
  fill(stressed, .direction = "down")

# change stressed to average reported
df <- df %>%
  group_by(id) %>%
  mutate(stressed = ifelse(all(is.na(stressed)), NA_real_, mean(stressed, na.rm = TRUE))) %>%
  ungroup()

# change sleep time to average reported
df <- dfd %>%
  group_by(id) %>%
  mutate(sleep_last = ifelse(all(is.na(sleep_last)), NA_real_, mean(sleep_last, na.rm = TRUE))) %>%
  ungroup()

# fill the concussions by down
df <- df %>% 
  group_by(id) %>% 
  fill(concussion, .direction = "down")

# check discrepancies
discrepancies <- df %>%
  group_by(id) %>%
  filter(n_distinct(concussion, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
df <- df %>% 
  group_by(id) %>% 
  mutate(concussion = ifelse("Yes" %in% concussion, "Yes", concussion)) %>%
  ungroup()

## too few observations (301) -- drop
df <- df %>% 
  select(-education)

## create how_high
df$how_high <- rowMeans(df[c("how_high1", "how_high2", "how_high3", "how_high4")], na.rm = TRUE)


# remove duplicated 
double_info <- df[duplicated(df[c("id", "date_date")], fromLast = TRUE), ]

#### gonogo ####

df_high <- getGroupPerformance("2023", "fall", "gonogo")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_gonogo <- merge(df, df_high, by = c("id"))

merged_df_gonogo <- merged_df_gonogo %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_gonogo  <- merged_df_gonogo  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_gonogo  <- merged_df_gonogo %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### visual search ####

df_high <- getGroupPerformance("2023", "fall", "visualsearch")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_vs <- merge(df, df_high, by = c("id"))

merged_df_vs <- merged_df_vs %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_vs  <- merged_df_vs  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_vs  <- merged_df_vs %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### taskswitching ####

df_high <- getGroupPerformance("2023", "fall", "taskswitching")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_taskswitching <- merge(df, df_high, by = c("id"))

merged_df_taskswitching <- merged_df_taskswitching %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_taskswitching  <- merged_df_taskswitching  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_taskswitching  <- merged_df_taskswitching %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### tunneling ####

df_high <- getGroupPerformance("2023", "fall", "tunneling")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_tunneling <- merge(df, df_high, by = c("id"))

merged_df_tunneling <- merged_df_tunneling %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_tunneling  <- merged_df_tunneling  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_tunneling  <- merged_df_tunneling %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### trailMaking ####

df_high <- getGroupPerformance("2023", "fall", "trailMaking")

df_high$date_1 <- as.POSIXct(df_high$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
df_high$date_date <- as.Date(df_high$date_1)
df_high$id <- df_high$participant
merged_df_trailmaking <- merge(df, df_high, by = c("id"))

merged_df_trailmaking <- merged_df_trailmaking %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_trailmaking  <- merged_df_trailmaking  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_trailmaking  <- merged_df_trailmaking %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### nBack ####


# Perform linear regression
model <- lm(dprime ~ how_high, data = merged_df_gonogo)

# View summary of regression model
summary(model)


