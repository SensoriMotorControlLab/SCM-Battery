#### combine gonogo ####

# add group identifier column
merged_df_gonogo_control <- mutate(merged_df_gonogo_control, group = "control")
merged_df_gonogo <- mutate(merged_df_gonogo, group = "experimental")

# combine data frames
gonogo <- rbind(merged_df_gonogo_control, merged_df_gonogo)

#### combine visualsearch ####

# add group identifier column
merged_df_vs_control <- mutate(merged_df_vs_control, group = "control")
merged_df_vs <- mutate(merged_df_vs, group = "experimental")

# combine data frames
visualsearch <- rbind(merged_df_vs_control, merged_df_vs)

#### combine taskswitching ####

# add group identifier column
merged_df_taskswitching_control <- mutate(merged_df_taskswitching_control, group = "control")
merged_df_taskswitching <- mutate(merged_df_taskswitching, group = "experimental")

# combine data frames
taskswitching <- rbind(merged_df_taskswitching_control, merged_df_taskswitching)

#### combine tunneling ####

# add group identifier column
merged_df_tunneling_control <- mutate(merged_df_tunneling_control, group = "control")
merged_df_tunneling <- mutate(merged_df_tunneling, group = "experimental")

# combine data frames
tunneling <- rbind(merged_df_tunneling_control, merged_df_tunneling)

#### combine trailmaking ####

# add group identifier column
merged_df_trailmaking_control <- mutate(merged_df_trailmaking_control, group = "control")
merged_df_trailmaking <- mutate(merged_df_trailmaking, group = "experimental")

# combine data frames
trailmaking <- rbind(merged_df_trailmaking_control, merged_df_trailmaking)

#### combine nback ####

# add group identifier column
merged_df_nback_control <- mutate(merged_df_nback_control, group = "control")
merged_df_nback <- mutate(merged_df_nback, group = "experimental")

# combine data frames
nback <- rbind(merged_df_nback_control, merged_df_nback)