
# we want to run functions named as a string over a bunch of participants

getGroupPerformance <- function(year, semester, task) {
  
  if (is.integer(year)) {year <- sprintf('%d',year)}
  
  # load function and settings from task source file
  source(paste0('R/',task,'.R'))
  
  # "settings" are variables declared in the sourced file:
  # nlines: a vector of acceptable number fo lines in each csv file
  # usefirst: boolean saying if we use the first of multiple files for one participants
  # (usually best to keep this TRUE)
  
  # get list of file names
  folder <- sprintf('data/%s/%s/%s/',year,semester,task)
  files <- list.files(folder,pattern='*.csv')
  
  # use readLines and weed out those with too few lines
  filelines <- unlist(lapply(sprintf('data/%s/%s/%s/%s',year,semester,task,files), function(x){length(readLines(x))}))
  files <- files[which(filelines %in% nlines)]
  
  # extract participant IDs and timestamps
  participants <- as.data.frame(do.call("rbind", lapply(files, getIDtimestamp, task)), stringsAsFactors=F)
  participants <- participants[order(participants$timestamp),]
  row.names(participants) <- NULL
  
  # remove duplicates:
  participants <- participants[!duplicated(participants$participant, fromLast=!usefirst),]
  
  # get relative filenames:
  participants$filename <- sprintf('data/%s/%s/%s/%s_%s_%s.csv',year,semester,task,participants$participant,task,participants$timestamp)
  
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
  
  # return a data frame
  return(functionoutput)
  
}

getIDtimestamp <- function(filename, task) {
  
  pattern <- sprintf('_%s_', task)
  
  pos <- gregexpr(pattern=pattern, filename)[[1]][1]
  pp <- substr(filename, 1, pos-1)
  ts <- substr(filename, pos+nchar(pattern), nchar(filename)-4)
  
  return(c('participant'=pp, 'timestamp'=ts))
  
}

combineCSVfiles <- function(path, nrows=NULL, ncols=NULL, skiponwarning=TRUE, files=NULL) {
  
  # combine all csv files in the folder on the path into one data frame
  
  if (is.null(files)) {
    files <- list.files(path = path, 
                        full.names = TRUE,
                        pattern = '\\.csv$')
  }
  
  datalist <- list()
  
  for (file in files) {
    
    csv_df <- tryCatch(
      {
        # we try to read the file:
        csv_df <- read.csv(file = file,
                           stringsAsFactors = FALSE) 
        
        # check the columns and rows:
        if(!is.null(nrows)) {
          if(dim(csv_df)[1] != nrows) {
            next # do not include files with incorrect number of rows
          }
        }
        if(!is.null(ncols)) {
          if(dim(csv_df)[2] != ncols) {
            next # do not include files with incorrect number of columns
          }
        }
        
        # at this point the file seems likely OK, so we add the data frame to our list:
        datalist[[length(datalist)+1]] <- csv_df
        
      },
      error=function(cond) {
        message(paste("File can not be read:", file))
        message(cond)
        return(NULL)
      },
      warning=function(cond) {
        message(paste("File caused a warning:", file))
        message(cond)
        return(NULL) # could be a different value from errors, like NA
      },
      finally={
        # nothing here?
      }
    )
    
  }
  
  # combine all data frames and return:
  return(do.call(rbind, datalist))
  
}

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}
