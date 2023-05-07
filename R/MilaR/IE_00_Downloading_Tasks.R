library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(lubridate)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')

#### Cleaning the tasks data, Loading to dataframes ####

tasks <- c("gonogo", "visualsearch", "taskswitching", "tunneling", "trailMaking", "nBack")

for (task in tasks) {
  start_time <- system.time({
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", task)
    assign(paste0(task, "_df"), df)
  })
  
  # print the elapsed time
  print(paste0("Elapsed time for task ", task, " was: ",  as.numeric(start_time["elapsed"]), " seconds"))
}

# May 4, 2023: updating taskswitching

df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", "taskswitching")
assign(paste0("taskswitching", "_df"), df)
df_list[[3]] <- taskswitching_df


# list of data frames
df_list <- list(gonogo_df, visualsearch_df, taskswitching_df, 
                tunneling_df, trailMaking_df, nBack_df)

# iterate over data frames and modify date column
df_list <- lapply(df_list, function(df) {
  df$date_1 <- as.POSIXct(df$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
  return(df)
})

##

df_list <- lapply(df_list, function(df) {
  df$date_1 <- as.POSIXct(df$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
  df$date_date <- as.Date(df$date)
  df$id <- df$participant
  return(df)
})

## May 4, 2023: fixing the taskswitching dataframe
df_list[[3]]$date_date <- as.Date(df_list[[3]]$date)
df_list[[3]]$id <- df_list[[3]]$participant

for (i in seq_along(df_list)) {
  write.csv(df_list[[i]], file.path("data", paste0("df_", tasks[i], ".csv")), row.names = FALSE)
}