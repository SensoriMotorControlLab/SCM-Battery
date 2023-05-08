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


#### calculate mean by groups ####

#### gonogo ####

# remove observations that didn't pass the screening (25)
gonogo <- gonogo %>%
  filter(passedscreening == TRUE)

# add new category for high users
gonogo <- gonogo %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users",
         ))

# fill used by down
gonogo <- gonogo %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       cannabis_freqnum, .direction = "downup")

gonogo  %>%
  group_by(users) %>%
  summarise_at(vars(dprime), list(name = mean), na.rm = TRUE)

#### Bivariate Analysis ####

# Create a vector of user groups in the desired order
user_order <- c("Non-users", "Infrequent users", "Frequent users", "High users")

# Reorder the 'users' factor variable according to the vector above
gonogo$users <- factor(gonogo$users, levels = user_order)

# drop missing on sex and gonogo (n = 23) / not necessary for IE paper
#gonogo <- gonogo[complete.cases(gonogo[, c("sex", "nogo_RT")]), ]

# Box plot by group
ggplot(gonogo, aes(x = users, y = dprime, fill= users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "Go/No-Go", x = "")+ 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                            paste0("High users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+ 
  theme(legend.position = "none") 



diff_means <- gonogo %>% 
  group_by(users) %>% 
  summarize(mean_dprime = mean(dprime)) %>% 
  mutate(diff = mean_dprime - mean_dprime[1]) %>% 
  ungroup()

# perform pairwise t-tests between groups
pairwise.t.test(gonogo$dprime, gonogo$users, p.adjust.method = "bonferroni")

# prepare list for means comparisons
my_comparisons <- list(c("Frequent users", "High users"), c("Non-users", "High users"))

#### calculate BFs ####
ttestBF(x = subset(gonogo, users == "Frequent users")$dprime,
        y = subset(gonogo, users == "High users")$dprime)

ttestBF(x = subset(gonogo, users == "Non-users")$dprime,
        y = subset(gonogo, users == "High users")$dprime)

bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(gonogo$users), levels(gonogo$users)))

for (i in 1:4) {
  for (j in 1:4) {
    if (i == j | i > j) {
      bf_matrix[i,j] <- "-"
    } else {
      test <- ttestBF(x = subset(gonogo, users == levels(gonogo$users)[i])$dprime,
                         y = subset(gonogo, users == levels(gonogo$users)[j])$dprime)@numerator$`Alt., r=0.707`@analysis$bf
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

# keep only data for high users
bf_df <- bf_df[(bf_df$group1 == "High users") | (bf_df$group2 == "High users"), ]




#set.seed(123)

#ggbetweenstats(
#  data  = gonogo,
#  x     = users,
#  y     = dprime,
#  title = "Go/No-Go", 
#  type = "bayes",
#  bf.prior = "medium",
#  centrality.type = "parametric"
#)

#### anova BF ####

gonogo$id <- as.factor(gonogo$id)

model_gonogo <- anovaBF(formula = dprime ~ users, data = gonogo, rscaleFixed = "wide")

extractBF(model_gonogo)

r2_bayes(model_gonogo)


# add significance symbols to the plot
ggplot(gonogo, aes(x = users, y = dprime)) + 
  geom_violin(width = 0.5) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Go/No-Go", x = "",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
       ) + 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                            paste0("High Users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+
  theme_bw() + 
  theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank()) +
  stat_summary(
    fun = mean, 
    geom = "label_repel", 
    aes(label = sprintf("μ = %.2f", after_stat(y))), 
    position = position_nudge_repel(x = 0.4, y = 0.2)
  ) +
  #stat_compare_means(method = "anova", label.y = 6.5)+ # Add global p-value
  #stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),
  #                   method = "t.test", ref.group = "High users", label.y = 5.6)
  ## manually
  #geom_segment(aes(x = 3, xend = 4, y = 5.8, yend = 5.8), linewidth = 0.3) +
  #geom_segment(aes(x = 3, xend = 3, y = 5.7, yend = 5.8), linewidth = 0.3) +
  #geom_segment(aes(x = 4, xend = 4, y = 5.7, yend = 5.8), linewidth = 0.3) +
  #geom_text(x = 3.5, y = 5.9, label = expression("BF"[10]*" = 59.74"), size = 3) +
  #geom_segment(aes(x = 2, xend = 4, y = 6, yend = 6), linewidth = 0.3) +
  #geom_segment(aes(x = 2, xend = 2, y = 5.9, yend = 6), linewidth = 0.3) +
  #geom_segment(aes(x = 4, xend = 4, y = 5.9, yend = 6), linewidth = 0.3) +
  #geom_text(x = 3, y = 6.1, label = expression("BF"[10]*" = 0.73"), size = 3) +
  #geom_segment(aes(x = 1, xend = 4, y = 6.2, yend = 6.2), linewidth = 0.3) +
  #geom_segment(aes(x = 1, xend = 1, y = 6.1, yend = 6.2), linewidth = 0.3) +
  #geom_segment(aes(x = 4, xend = 4, y = 6.1, yend = 6.2), linewidth = 0.3) +
  #geom_text(x = 2.5, y = 6.3, label = expression("BF"[10]*" = 0.41"), size = 3)
  ## custom
  stat_pvalue_manual(
    data = bf_df, label = "BF = {p.adj}",
    xmin = "group1", xmax = "group2",
    y.position = c(5.8, 6.2, 6.6)
  )
  

