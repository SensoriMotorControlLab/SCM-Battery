library(data.table)
library(stringr)

#### merge questionnaire with tasks ####

#### gonogo ####

merged_df_gonogo_control <- merge(df_combined, df_list[[1]], by = c("id"))

merged_df_gonogo_control <- merged_df_gonogo_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_gonogo_control  <- merged_df_gonogo_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_gonogo_control  <- merged_df_gonogo_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### visual search ####

merged_df_vs_control <- merge(df_combined, df_list[[2]], by = c("id"))

merged_df_vs_control <- merged_df_vs_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_vs_control  <- merged_df_vs_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_vs_control  <- merged_df_vs_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### taskswitching ####

merged_df_taskswitching_control <- merge(df_combined, df_list[[3]], by = c("id"))

merged_df_taskswitching_control <- merged_df_taskswitching_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_taskswitching_control  <- merged_df_taskswitching_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_taskswitching_control  <- merged_df_taskswitching_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### tunneling ####

merged_df_tunneling_control <- merge(df_combined, df_list[[4]], by = c("id"))

merged_df_tunneling_control <- merged_df_tunneling_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_tunneling_control  <- merged_df_tunneling_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_tunneling_control  <- merged_df_tunneling_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### trailMaking ####

merged_df_trailmaking_control <- merge(df_combined, df_list[[5]], by = c("id"))

merged_df_trailmaking_control <- merged_df_trailmaking_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_trailmaking_control  <- merged_df_trailmaking_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_trailmaking_control  <- merged_df_trailmaking_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))

#### nBack ####

merged_df_nback_control <- merge(df_combined, df_list[[6]], by = c("id"))

merged_df_nback_control <- merged_df_nback_control %>%
  mutate(date_diff = abs(date_date.x - date_date.y))

merged_df_nback_control  <- merged_df_nback_control  %>%
  filter(abs(date_date.x - date_date.y) <= 1)

merged_df_nback_control  <- merged_df_nback_control %>%
  group_by(id) %>%
  slice(which.min(date_diff))


# Perform linear regression
model <- lm(dprime ~ cannabis_group, data = merged_df_gonogo_control)

# View summary of regression model
summary(model)