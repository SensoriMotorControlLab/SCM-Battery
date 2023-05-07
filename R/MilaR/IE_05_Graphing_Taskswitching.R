library(ggplot2)
library(survey)
library(flextable)
library(dplyr)

library(rlang)

library(srvyr)

library(gtsummary)

library(stargazer)
library(tibble)

library(jtools)
library(broom.mixed)

library(ggpubr)
library(ggstatsplot)

library(BayesFactor)

library(ggrepel)

library(reshape2)

library(performance)
library(tidyr)

#### taskswitching ####

## filter those that didn't pass the screening (n = 107)
taskswitching <- taskswitching %>%
  filter(passedscreening == TRUE)

# add new category for high users
taskswitching <- taskswitching %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           is.na(cannabis_freqnum) ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users"
         ))

# fill used by down
taskswitching <- taskswitching %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       singleblock_1_RT, singleblock_2_RT, switch_RT,
       nonswitch_RT, congruent_RT, nonCongruent_RT, totaltime,
       passedscreening,  cannabis_freqnum, .direction = "downup")

taskswitching  %>%
  group_by(users) %>%
  summarise_at(vars(singleblock_1_RT, singleblock_2_RT, switch_RT,
                    nonswitch_RT, congruent_RT, nonCongruent_RT), list(name = mean), na.rm = TRUE)

# Reorder the 'users' factor variable according to the vector above
taskswitching$users <- factor(taskswitching$users, levels = user_order)

#### BF calculations ####

bf_df_all <- data.frame(group = character(),
                    group1 = character(),
                    group2 = character(),
                    p.adj = numeric(),
                    stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("singleblock_1_RT", "switch_RT", "congruent_RT")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(taskswitching$users), levels(taskswitching$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(taskswitching, users == levels(taskswitching$users)[i])[[k]],
                        y = subset(taskswitching, users == levels(taskswitching$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
        bf_matrix[i,j] <- round(exp(test), 2)
      }
    }
  }
  
  # Transform bf_matrix to long format
  bf_df <- melt(bf_matrix)
  
  # Rename columns
  names(bf_df) <- c("group1", "group2", "p.adj")
  
  # Remove rows with "-" values
  bf_df <- bf_df[bf_df$p.adj != "-",]
  
  # add identifier
  bf_df$group <- k
  
  # keep only data for high users
  bf_df <- bf_df[(bf_df$group1 == "High users") | (bf_df$group2 == "High users"), ]
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = group,
                                  multiplier = case_when(group == "singleblock_1_RT" ~ 1,
                                                         group == "congruent_RT" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = group,
                                  group_2 = case_when(group == "singleblock_1_RT" ~ 1.33,
                                                      group == "congruent_RT" ~ 2.33,
                                                      TRUE ~ 3.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "Non-users" ~ multiplier - 0.34,
                                                      group1 == "Infrequent users" ~ multiplier - 0.11,
                                                      group1 == "Frequent users" ~ multiplier + 0.1))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "Non-users" ~ 3,
                                                group1 == "Infrequent users" ~ 3.5,
                                                group1 == "Frequent users" ~ 4))

bf_df_all$users <- bf_df_all$group1


#### calculate ANOVA bf ####

ts <- taskswitching %>%
  select(id, singleblock_1_RT, switch_RT, congruent_RT, users)  %>%
  pivot_longer(singleblock_1_RT:congruent_RT, names_to = "Group", values_to = "RT") %>%
  mutate(tasks = factor(case_when(
    Group == "singleblock_1_RT" ~ "single",
    Group == "switch_RT" ~ "switch",
    Group == "congruent_RT" ~ "congruent"
  ), levels = c("single", "congruent", "switch")))


ts$id <- as.factor(ts$id)

options(scipen = 999)

#model_ts <- anovaBF(formula = RT ~ users*tasks + id, data = ts, rscaleFixed = "wide", whichRandom = "id")
model_ts <- anovaBF(formula = RT ~ users*tasks, data = ts, rscaleFixed = "wide")

extractBF(model_ts)

r2_bayes(model_ts)

#### plot ####

ggplot(ts, aes(x = tasks, y = RT, fill = users)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Task Switching", x = "Tasks", y = "Mean") +
  scale_fill_manual(values = c("Non-users" = "#f8766d", "Infrequent users" = "#7caeff", "Frequent users" = "#00ba38", "High users" = "#c77cff")) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )






#### prepare descriptive table ####

tbl_svysummary <-
  survey::svydesign(~1, data = taskswitching) %>%
  tbl_svysummary(by = users,
                 include = c(singleblock_1_RT, switch_RT, congruent_RT, totaltime),
                 type = list(singleblock_1_RT ~ 'continuous',
                             congruent_RT ~ 'continuous', 
                             switch_RT ~ 'continuous', 
                             totaltime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_svysummary

#### saving output in a Word document ####

save_as_docx(tbl_svysummary, path = "data/output/Table_ts_1.docx")

#### ts model ####

# Perform linear regression
model_1 <- lm(singleblock_1_RT ~ users, data = taskswitching)
model_2 <- lm(congruent_RT ~ users, data = taskswitching)
model_3 <- lm(switch_RT ~ users, data = taskswitching)

stargazer(model_1, model_2, model_3, title = "Table 2. Regression Model", type="text", out = "data/output/Table_ts_2.doc")


####