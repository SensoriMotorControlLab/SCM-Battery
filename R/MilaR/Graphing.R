library(ggplot2)

#### calculate mean by groups ####

# add new category for high users
gonogo <- gonogo %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           is.na(cannabis_freqnum) ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users"
         ))



gonogo  %>%
  group_by(users) %>%
  summarise_at(vars(dprime), list(name = mean), na.rm = TRUE)

# Create a vector of user groups in the desired order
user_order <- c("Non-users", "Infrequent users", "Frequent users", "High users")

# Reorder the 'users' factor variable according to the vector above
gonogo$users <- factor(gonogo$users, levels = user_order)


# Box plot by group
ggplot(gonogo, aes(x = users, y = dprime, fill= users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "NoGo", x = "")+ 
  scale_x_discrete(labels=c("Non-User\n (n=360)", "Infrequent User\n (n=174)", "Frequent Users\n (n=97)", "High users\n (n=45)"))+ 
  theme(legend.position = "none") 

#### gonogo model ####

# Perform linear regression
model <- lm(dprime ~ users, data = gonogo)

# View summary of regression model
summary(model)

#### visualsearch ####

df %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))

# line segment graph
df %>%
  select(RT_6_absent:RT_18_present, cannabis_group)  %>%
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
  group_by(cannabis_group, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(cannabis_group, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count)) %>%
  ggplot(aes(x=factor(set_size), y=mean, 
             group=interaction(Present, cannabis_group),
             shape = Present,
             color=cannabis_group)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_line(aes(linetype = Present)) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 6), name = "Reaction Time (s)") +
  xlab("Set Size")

