library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
install.packages("lme4", dependencies=TRUE)
library(lme4)
install.packages("lmerTest", dependencies=TRUE)
library(lmerTest)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')

#### identifying part 3 and 4 ####

file_names <- c('data/Immediate+Cannabis+Effects-+Part+3_November+2,+2022_19.43.csv', 
                'data/Immediate+Cannabis+Effects-+Winter22-Part+3_November+2,+2022_19.53.csv',
                'data/Immediate+Cannabis+Effects-+Part+4_November+2,+2022_19.46.csv',
                'data/Immediate+Cannabis+Effects-+Winter22-Part+4_November+2,+2022_19.56.csv')

# columns that will be used
columns <- c("StartDate",
             "EndDate",
             "Finished",
             "group",
             "id")

Part34_list <- list()
for (file_name in file_names) {
  Part34 <- read.csv(file_name, stringsAsFactors = FALSE)
  Part34 <- Part34[columns]
  Part34 <- Part34[which(Part34$Finished == 'True'),]
  Part34_list[[file_name]] <- Part34
}

#### copying files to proper high folders ####

tasks <- c("gonogo", "visualsearch", "taskswitching", "trailmaking", "tunneling")
folder <- file.path("data", "pavlovia", "SensoriMotorBattery_anonymized")

tasks_lists <- list(
  gonogo_high = list(),
  visualsearch_high = list(),
  taskswitching_high = list(),
  trailmaking_high = list(),
  tunneling_high = list(),
  nback_high = list(),
  gonogo_control = list(),
  visualsearch_control = list(),
  taskswitching_control = list(),
  trailmaking_control = list(),
  tunneling_control = list(),
  nback_control = list()
)


for (task in tasks) {
  task_folder <- file.path(folder, task)
  dest_task_folder <- file.path(folder, paste0(task, "_high"))
  
  for (part in Part34_list) {
    output_files <- getOutputFiles(task_folder, part)
    tasks_lists[[paste0(task, "_high")]] <- c(tasks_lists[[paste0(task, "_high")]], output_files)
    moveFiles(output_files, task_folder, dest_task_folder)
  }
}

for (task in tasks) {
  task_folder <- file.path(folder, task)
  dest_task_folder <- file.path(folder, paste0(task, "_control"))
  
  for (part in Part34_list) {
    output_files <- getOutputFiles(task_folder, part, t = "outside")
    tasks_lists[[paste0(task, "_control")]] <- c(tasks_lists[[paste0(task, "_control")]], output_files)
    moveFiles(output_files, task_folder, dest_task_folder)
  }
}

tasks <- c("nback")
folder <- file.path("data", "pavlovia", "SensoriMotorBattery_raw")

for (task in tasks) {
  task_folder <- file.path(folder, task)
  dest_task_folder <- file.path(folder, paste0(task, "_high"))
  
  for (part in Part34_list) {
    output_files <- getOutputFiles(task_folder, part)
    tasks_lists[[paste0(task, "_high")]] <- c(tasks_lists[[paste0(task, "_high")]], output_files)
    moveFiles(output_files, task_folder, dest_task_folder)
  }
}

for (task in tasks) {
  task_folder <- file.path(folder, task)
  dest_task_folder <- file.path(folder, paste0(task, "_control"))
  
  for (part in Part34_list) {
    output_files <- getOutputFiles(task_folder, part, t = "outside")
    tasks_lists[[paste0(task, "_control")]] <- c(tasks_lists[[paste0(task, "_control")]], output_files)
    moveFiles(output_files, task_folder, dest_task_folder)
  }
}

#### get tasks ####
tasks <- c("gonogo", "visualsearch", "taskswitching", "tunneling", "trailmaking")
suffixes <- c("_high", "_control")

for (task in tasks) {
  for (suffix in suffixes) {
    file_list <- tasks_lists[[paste0(task, suffix)]]
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", task, file_list = file_list)
    assign(paste0(task, suffix, "_df"), df)
  }
}

tasks <- c("nback")
suffixes <- c("_high", "_control")

for (task in tasks) {
  for (suffix in suffixes) {
    file_list <- tasks_lists[[paste0(task, suffix)]]
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_raw", task, file_list = file_list)
    assign(paste0(task, suffix, "_df"), df)
  }
}

df_imm <- merge(Part3_1, gng_df,   by.x='id', by.y='participant', all=FALSE)

#### combining data ####

gonogo_control_df$treatment <- "control"
gonogo_high_df$treatment <- "experiment"

gonogo_combined_df <- rbind(gonogo_control_df, gonogo_high_df)

taskswitching_control_df$treatment <- "control"
taskswitching_high_df$treatment <- "experiment"

taskswitching_combined_df <- rbind(taskswitching_control_df, taskswitching_high_df)


nback_control_df$treatment <- "control"
nback_high_df$treatment <- "experiment"

nback_combined_df <- rbind(nback_control_df, nback_high_df)
nback_combined_df$dprime <- nback_combined_df$N1_dprime + 
  nback_combined_df$N2_dprime + nback_combined_df$N3_dprime


trailmaking_control_df$treatment <- "control"
trailmaking_high_df$treatment <- "experiment"

trailmaking_combined_df <- rbind(trailmaking_control_df, trailmaking_high_df)

tunneling_control_df$treatment <- "control"
tunneling_high_df$treatment <- "experiment"

tunneling_combined_df <- rbind(tunneling_control_df, tunneling_high_df)

visualsearch_control_df$treatment <- "control"
visualsearch_high_df$treatment <- "experiment"

visualsearch_combined_df <- rbind(visualsearch_control_df, visualsearch_high_df)

# Set up model formula with random intercepts for each participant
model <- lmer(dprime ~ treatment + (1|participant), data = gonogo_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

# Set up model formula with random intercepts for each participant
model <- lmer(totaltime ~ treatment + (1|participant), data = taskswitching_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

# Set up model formula with random intercepts for each participant
model <- lmer(dprime ~ treatment + (1|participant), data = nback_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

# Set up model formula with random intercepts for each participant
model <- lmer(totaltime ~ treatment + (1|participant), data = trailmaking_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

# Set up model formula with random intercepts for each participant
model <- lmer(totaltime ~ treatment + (1|participant), data = tunneling_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

# Set up model formula with random intercepts for each participant
model <- lmer(totaltime ~ treatment + (1|participant), data = visualsearch_combined_df)

# Fit the model
fit <- summary(model)

# View the model results
print(fit)

