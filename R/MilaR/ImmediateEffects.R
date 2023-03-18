library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')

Part3_1 <- read.csv('data/Immediate+Cannabis+Effects-+Part+3_November+2,+2022_19.43.csv', stringsAsFactors=F)
Part3_2 <- read.csv('data/Immediate+Cannabis+Effects-+Winter22-Part+3_November+2,+2022_19.53.csv', stringsAsFactors=F)



# keeping only the columns that will be used
Part3_1 <- Part3_1[columns]
Part3_2 <- Part3_2[columns]

# keep those who have finished the session
Part3_1 <- Part3_1[which(Part3_1$Finished == 'True'),]
Part3_2 <- Part3_2[which(Part3_2$Finished == 'True'),]

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
gng_df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", "gonogo", file_list = tasks_lists[["gonogo_high"]])
gng_df_control <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", "gonogo", file_list = tasks_lists[["gonogo_control"]])

vs_df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", "visualsearch", file_list = tasks_lists[["visualsearch_high"]])
vs_df_control <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", "visualsearch", file_list = tasks_lists[["visualsearch_control"]])

tasks <- c("gonogo", "visualsearch", "tunneling")
suffixes <- c("_high", "_control")

for (task in tasks) {
  for (suffix in suffixes) {
    file_list <- tasks_lists[[paste0(task, suffix)]]
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", task, file_list = file_list)
    assign(paste0(task, suffix, "_df"), df)
  }
}

df_imm <- merge(Part3_1, gng_df,   by.x='id', by.y='participant', all=FALSE)

#### testing why not working reading files ####

year <- "pavlovia"
semester <- "SensoriMotorBattery_anonymized" 
task <- "taskswitching" 
file_list = tasks_lists[["taskswitching_high"]]
  
if (is.integer(year)) {year <- sprintf('%d',year)}

# load function and settings from task source file
source(paste0('R/',task,'.R'))

# "settings" are variables declared in the sourced file:
# nlines: a vector of acceptable number fo lines in each csv file
# usefirst: boolean saying if we use the first of multiple files for one participants
# (usually best to keep this TRUE)

# get list of file names
folder <- file.path('data',year,semester,task)
if(length(file_list) == 0){
  files <- list.files(folder,pattern='*.csv')
} else {
  files <- file_list
}

# use readLines and weed out those with too few lines
filelines <- unlist(lapply(sprintf('%s%s%s',folder,.Platform$file.sep,files), function(x){length(readLines(x))}))
files <- files[which(filelines %in% nlines)]

# extract participant IDs and timestamps
participants <- as.data.frame(do.call("rbind", lapply(files, getIDtimestamp_KK, task)), stringsAsFactors=F)
participants <- participants[order(participants$timestamp),]
row.names(participants) <- NULL

# remove duplicates:
participants <- participants[!duplicated(participants$participant, fromLast=!usefirst),]

# get relative filenames:
participants$filename <- sprintf('data/%s/%s/%s/%s_%s_%s',year,semester,task,participants$participant,participants$task,participants$timestamp)

# magic: this assigns a function to f, by finding a function
# that has the name specified in the character variable task
# which is why the function in the sourced file
# (that has the same name as the task)
# needs to have the same name as the task
f <- match.fun(task)

# and use lapply to run stuff on all participants
functionoutput <- as.data.frame(do.call("rbind", lapply(participants$filename, f)))

# this will be a complicated format, so we simplify it a little here
colnames <- names(functionoutput)
for (colname in colnames) {
  functionoutput[colname] <- unlist(functionoutput[colname])
}
  

for (f in tasks_lists[["taskswitching_high"]]){
  print(f)
  taskswitching(f)
}


