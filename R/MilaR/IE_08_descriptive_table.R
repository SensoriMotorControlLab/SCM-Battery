#### prepare descriptive table ####

tbl_1 <-
  tbl_summary(data = gonogo, by = users, percent = "column",
                 include = c(dprime),
                 type = list(dprime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_summary(data = gonogo, by = users, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  add_p(dprime ~ users) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_1_2 <-
  tbl_summary(data = gonogo_subset, by = group, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_1_3 <-
  tbl_summary(data = matched_gonogo, by = group, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 



tbl_2 <-
  tbl_summary(data = visualsearch, by = users, percent = "column",
              include = c(propcorrect_6_absent, propcorrect_12_absent, propcorrect_18_absent,
                          propcorrect_6_present, propcorrect_12_present, propcorrect_18_present),
              type = list(propcorrect_6_absent ~ 'continuous', 
                          propcorrect_12_absent ~ 'continuous', 
                          propcorrect_18_absent ~ 'continuous',
                          propcorrect_6_present ~ 'continuous', 
                          propcorrect_12_present ~ 'continuous', 
                          propcorrect_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_2_2 <-
  tbl_summary(data = visualsearch_subset, by = group, percent = "column",
              include = c(propcorrect_6_absent, propcorrect_12_absent, propcorrect_18_absent,
                          propcorrect_6_present, propcorrect_12_present, propcorrect_18_present),
              type = list(propcorrect_6_absent ~ 'continuous', 
                          propcorrect_12_absent ~ 'continuous', 
                          propcorrect_18_absent ~ 'continuous',
                          propcorrect_6_present ~ 'continuous', 
                          propcorrect_12_present ~ 'continuous', 
                          propcorrect_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_2_3 <-
  tbl_summary(data = matched_visualsearch, by = group, percent = "column",
              include = c(propcorrect_6_absent, propcorrect_12_absent, propcorrect_18_absent,
                          propcorrect_6_present, propcorrect_12_present, propcorrect_18_present),
              type = list(propcorrect_6_absent ~ 'continuous', 
                          propcorrect_12_absent ~ 'continuous', 
                          propcorrect_18_absent ~ 'continuous',
                          propcorrect_6_present ~ 'continuous', 
                          propcorrect_12_present ~ 'continuous', 
                          propcorrect_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_3 <-
  tbl_summary(data = taskswitching, by = users, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_3_2 <-
  tbl_summary(data = taskswitching_subset, by = group, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_3_3 <-
  tbl_summary(data = matched_taskswitching, by = group, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4 <-
  tbl_summary(data = trailmaking, by = users, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4_2 <-
  tbl_summary(data = trailmaking_subset, by = group, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4_3 <-
  tbl_summary(data = matched_trailmaking, by = group, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5 <-
  tbl_summary(data = nback, by = users, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5_2 <-
  tbl_summary(data = nback_subset, by = group, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5_3 <-
  tbl_summary(data = matched_nback, by = group, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_combined <- tbl_merge(
    tbls = list(tbl_1, tbl_1_2, tbl_1_3,
                tbl_2, tbl_2_2, tbl_2_3,
                tbl_3, tbl_3_2, tbl_3_3,
                tbl_4, tbl_4_2, tbl_4_3,
                tbl_5, tbl_5_2, tbl_5_3)
  ) %>%
  as_flex_table()

tbl_combined

#### saving output in a Word document ####

save_as_docx(tbl_combined, path = "data/output/Table_1.docx")

