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

library(stringr)


#### visualsearch ####

## looks like all passed screening?
visualsearch <- visualsearch %>%
  filter(passedscreening == TRUE)

# add new category for high users
visualsearch <- visualsearch %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           is.na(cannabis_freqnum) ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users"
         ))

# fill used by down
visualsearch <- visualsearch %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       cannabis_freqnum, .direction = "downup")

# Reorder the 'users' factor variable according to the vector above
visualsearch$users <- factor(visualsearch$users, levels = user_order)

visualsearch %>%
  group_by(users) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))

visualsearch %>%
  group_by(users) %>%
  summarise_at(vars(propcorrect_6_absent, propcorrect_12_absent, propcorrect_18_absent,
                    propcorrect_6_present, propcorrect_12_present, propcorrect_18_present), 
               list(name = mean))

#### calculate BFs ####

# BF for RT
bf_df_all <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("RT_6_absent", "RT_12_absent", "RT_18_absent",
            "RT_6_present", "RT_12_present", "RT_18_present")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(visualsearch$users), levels(visualsearch$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(visualsearch, users == levels(visualsearch$users)[i])[[k]],
                        y = subset(visualsearch, users == levels(visualsearch$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
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

# BF for propcorrect
bf_df_all_pc <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("propcorrect_6_absent", "propcorrect_12_absent", "propcorrect_18_absent",
            "propcorrect_6_present", "propcorrect_12_present", "propcorrect_18_present")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(visualsearch$users), levels(visualsearch$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(visualsearch, users == levels(visualsearch$users)[i])[[k]],
                        y = subset(visualsearch, users == levels(visualsearch$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
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
  bf_df_all_pc <- rbind(bf_df_all_pc, bf_df)
}

# calculate y positions and add users

vs <-visualsearch %>%
  select(propcorrect_6_absent:propcorrect_18_present, users)  %>%
  pivot_longer(propcorrect_6_absent:propcorrect_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "propcorrect_6_absent" ~ 6,
                              Group == "propcorrect_12_absent" ~ 12,
                              Group == "propcorrect_18_absent" ~ 18,
                              Group == "propcorrect_6_present" ~ 6,
                              Group == "propcorrect_12_present" ~ 12,
                              Group == "propcorrect_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "propcorrect_6_absent" ~ "absent",
                             Group == "propcorrect_12_absent" ~ "absent",
                             Group == "propcorrect_18_absent" ~ "absent",
                             Group == "propcorrect_6_present" ~ "present",
                             Group == "propcorrect_12_present" ~ "present",
                             Group == "propcorrect_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))

bf_df_all_pc <- bf_df_all_pc %>% mutate(set_size = str_extract(group, "\\d+"))
bf_df_all_pc <- bf_df_all_pc %>% mutate(Present = str_extract(group, "(present|absent)"))

bf_df_all_pc <- bf_df_all_pc %>% mutate(y = group1,
                                        y = case_when(group1 == "Non-users" ~ as.numeric(set_size) + 0.8,
                                                      group1 == "Infrequent users" ~ as.numeric(set_size) + 1.6,
                                                      group1 == "Frequent users" ~ as.numeric(set_size) + 2.4))

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_1 = vs$mean[match(paste(group1, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_2 = vs$mean[match(paste(group2, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all_pc$users <- bf_df_all_pc$group1

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(
    group_min = ifelse(group_1 > group_2, group_2, group_1),
    group_max = ifelse(group_1 > group_2, group_1, group_2)
  )

bf_df_all_pc$p.adj <- as.numeric(bf_df_all_pc$p.adj)
bf_df_all_pc$set_size <- as.double(bf_df_all_pc$set_size)

bf_df_all_pc <- bf_df_all_pc %>% 
  select(p.adj, set_size, users, y, Present, group_min, group_max)

bf_df_all_pc$group1 <- bf_df_all_pc$group_min
bf_df_all_pc$group2 <- bf_df_all_pc$group_max

#### anova ####

vs <-visualsearch %>%
  select(propcorrect_6_absent:propcorrect_18_present, users)  %>%
  pivot_longer(propcorrect_6_absent:propcorrect_18_present, names_to = "Group", values_to = "PC") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "propcorrect_6_absent" ~ 6,
                              Group == "propcorrect_12_absent" ~ 12,
                              Group == "propcorrect_18_absent" ~ 18,
                              Group == "propcorrect_6_present" ~ 6,
                              Group == "propcorrect_12_present" ~ 12,
                              Group == "propcorrect_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "propcorrect_6_absent" ~ "absent",
                             Group == "propcorrect_12_absent" ~ "absent",
                             Group == "propcorrect_18_absent" ~ "absent",
                             Group == "propcorrect_6_present" ~ "present",
                             Group == "propcorrect_12_present" ~ "present",
                             Group == "propcorrect_18_present" ~ "present"))  

vs$set_size_factor <- as.factor(vs$set_size)
vs$Present_factor <- as.factor(vs$Present)

model_vs <- anovaBF(formula = PC ~ users*set_size_factor*Present_factor, data = vs, rscaleFixed = "wide")

extractBF(model_vs)

r2_bayes(model_vs)


#### plotting ####

vs <-visualsearch %>%
  select(propcorrect_6_absent:propcorrect_18_present, users)  %>%
  pivot_longer(propcorrect_6_absent:propcorrect_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "propcorrect_6_absent" ~ 6,
                              Group == "propcorrect_12_absent" ~ 12,
                              Group == "propcorrect_18_absent" ~ 18,
                              Group == "propcorrect_6_present" ~ 6,
                              Group == "propcorrect_12_present" ~ 12,
                              Group == "propcorrect_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "propcorrect_6_absent" ~ "absent",
                             Group == "propcorrect_12_absent" ~ "absent",
                             Group == "propcorrect_18_absent" ~ "absent",
                             Group == "propcorrect_6_present" ~ "present",
                             Group == "propcorrect_12_present" ~ "present",
                             Group == "propcorrect_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))
# plot for RT
visualsearch %>%
  select(RT_6_absent:RT_18_present, users)  %>%
  pivot_longer(RT_6_absent:RT_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "RT_6_absent" ~ 6,
                              Group == "RT_12_absent" ~ 12,
                              Group == "RT_18_absent" ~ 18,
                              Group == "RT_6_present" ~ 6,
                              Group == "RT_12_present" ~ 12,
                              Group == "RT_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "RT_6_absent" ~ "absent",
                             Group == "RT_12_absent" ~ "absent",
                             Group == "RT_18_absent" ~ "absent",
                             Group == "RT_6_present" ~ "present",
                             Group == "RT_12_present" ~ "present",
                             Group == "RT_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present)  %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))%>%
  ggplot(aes(x=factor(set_size), y=mean, 
             group=interaction(Present, users),
             shape = Present,
             color=users)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_line(aes(linetype = Present)) +
  geom_point(size=3) +
  theme_bw()+
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  labs(title = "Visual Search", x = "")+ 
  scale_y_continuous(limits = c(1, 5), name = "Reaction Time (s)") +
  xlab("Set Size")

## by correctness
# line segment graph
vs %>%
  ggplot(aes(y=set_size, x=mean, 
             group=interaction(Present, users),
             shape = Present,
             color=users)) + 
  geom_errorbar(aes(xmin=mean-se, xmax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_path(aes(linetype = Present)) +
  geom_point(size=3) +
  stat_pvalue_manual(
    data = bf_df_all_pc, label = "BF = {p.adj}",
    xmin = "group_min", xmax = "group_max",
    y.position = "y"
  )+ 
  scale_y_continuous(breaks = c(6,12,18))+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  labs(title = "Visual Search", x = "Proportion correct", y = "Set size")+ 
  scale_color_discrete(name = "Users", 
                       labels=c(paste0("Non-User\n (n=", table(visualsearch$users)["Non-users"][[1]], ")"), 
                                paste0("Infrequent User\n (n=", table(visualsearch$users)["Infrequent users"][[1]], ")"), 
                                paste0("Frequent Users\n (n=", table(visualsearch$users)["Frequent users"][[1]], ")"), 
                                paste0("High users\n (n=", table(visualsearch$users)["High users"][[1]], ")")))+
  scale_shape_discrete(name = "Condition") +
  scale_linetype_discrete(name = "Condition")



#### prepare descriptive table ####

tbl_svysummary <-
  survey::svydesign(~1, data = visualsearch) %>%
  tbl_svysummary(by = users,
                 include = c(RT_6_absent, RT_12_absent, RT_18_absent,
                             RT_6_present, RT_12_present, RT_18_present, totaltime),
                 type = list(RT_6_absent ~ 'continuous',
                             RT_12_absent ~ 'continuous', 
                             RT_18_absent ~ 'continuous', 
                             RT_6_present ~ 'continuous',
                             RT_12_present ~ 'continuous', 
                             RT_18_present ~ 'continuous', 
                             totaltime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_svysummary

#### saving output in a Word document ####

save_as_docx(tbl_svysummary, path = "data/output/Table_vs_1.docx")

#### vs model ####

# Perform linear regression
model <- lm(dprime ~ users, data = visualsearch)

# View summary of regression model
model_1 <- lm(RT_6_absent ~ users, data = visualsearch)
model_2 <- lm(RT_12_absent ~ users, data = visualsearch)
model_3 <- lm(RT_18_absent ~ users, data = visualsearch)
model_4 <- lm(RT_6_present ~ users, data = visualsearch)
model_5 <- lm(RT_12_present ~ users, data = visualsearch)
model_6 <- lm(RT_18_present ~ users, data = visualsearch)

stargazer(model_1, model_2, model_3, 
          model_4, model_5, model_6, title = "Table 2. Regression Model", type="text", out = "data/output/Table_vs_2.doc")

## anova

model_1 <- aov(RT_6_absent ~ users, data = visualsearch)
model_2 <- aov(RT_12_absent ~ users, data = visualsearch)
model_3 <- aov(RT_18_absent ~ users, data = visualsearch)
model_4 <- aov(RT_6_present ~ users, data = visualsearch)
model_5 <- aov(RT_12_present ~ users, data = visualsearch)
model_6 <- aov(RT_18_present ~ users, data = visualsearch)

stargazer(model_1, model_2, model_3, model_4, model_5, model_6, 
          out = "data/output/Table_vs_3.doc")

# Perform Tukey's HSD test
my_tukey_1 <- TukeyHSD(model_1)
my_tukey_2 <- TukeyHSD(model_2)
my_tukey_3 <- TukeyHSD(model_3)
my_tukey_4 <- TukeyHSD(model_4)
my_tukey_5 <- TukeyHSD(model_5)
my_tukey_6 <- TukeyHSD(model_6)

# Combine the Tukey HSD results into a single data frame
tukey_tbl <- bind_rows(
  as.data.frame(my_tukey_1[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_2[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_3[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_4[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_5[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_6[[1]][, c(1, 4)])
)

# Add index as column
tukey_tbl <- tukey_tbl %>% rownames_to_column(var = "index")

# create a flextable object
flex_tbl <- flextable(tukey_tbl)

# create a Word document
save_as_docx(flex_tbl, path = "data/output/flex_tbl_2.docx")


