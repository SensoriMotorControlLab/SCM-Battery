
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(165, 111)

# super-handy operator:
`%notin%` <- Negate(`%in%`)

visualsearch <- function(filename) {
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  # set up a vector for output, must be a named vector:
  output <- c()
  use = TRUE
  
  # remove / rename weird variable names:
  if ('responsecorrect' %notin% names(df) & 'trialResp.corr' %in% names(df)) {
    df$responsecorrect <- df$trialResp.corr
  }
  if ('RT' %notin% names(df) & 'trialResp.rt' %in% names(df)) {
    df$RT <- df$trialResp.rt
  }
  
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$responsecorrect)),]
  
  # remove lines with very low RTs:
  df <- df[which(df$RT > 0.1),]
  # remove lines with ridiculously high RTs:
  df <- df[which(df$RT < 40),]
  
  # print(str(df))
  
  # get target presence as a variable:
  if ('targetpresence' %notin% names(df) & 'trialResp.keys' %in% names(df) & 'trialResp.corr' %in% names(df)) {
    df$targetpresent <- NA
    df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 1)] <- 0 # 'absent'
    df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 1)] <- 1 # 'present'
    df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 0)] <- 1 # 'present'
    df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 0)] <- 0 # 'absent'
    df$targetpresence <- df$targetpresent
  }
  
  df$counttrials <- 1 # these will be summed to get a count of trials per conditions
  # check if there are enough good trials left
  counttrials <- aggregate(counttrials ~ arraysize + targetpresence, data=df, FUN=sum)
  if (any(counttrials$counttrials < 12)) {
    #print(filename)
    use = FALSE
    #cat('removed because of reaction time strangeness (too high or too low)\n')
  }
  
  if ('responsecorrect' %notin% names(df) & 'trialResp.corr' %in% names(df)) {
    df$responsecorrect <- as.numeric(as.logical(df$trialResp.corr))
  }
  df$responsecorrect <- as.numeric(df$responsecorrect)
  # print(as.numeric(df$responsecorrect[c(1:5)]))
  
  df$targetpresence <- as.vector(c('0'='absent', '1'='present')[sprintf('%d',df$targetpresence)])
  
  
  # get proportion correct scores to data:
  correct <- aggregate(responsecorrect ~ arraysize + targetpresence, data=df, FUN=mean)
  
  # print(correct)
  
  # remove people who are less then 66.7% correct in any condition
  # if (any(correct$trialResp.corr < (2/3))) {
  #   use = FALSE
  #   cat('what is going on?\n')
  # }
  
  correctOutput <- as.vector(unlist(correct$responsecorrect))
  names(correctOutput) <- sprintf('propcorrect_%d_%s',correct$arraysize,correct$targetpresence)
  
  # we remove the trials without a correct response:
  df[which(df$responsecorrect == 1),]
  
  # print(df$targetpresence)
  
  
  # now get the average RTs:
  RTs <- aggregate(RT ~ arraysize + targetpresence, data=df, FUN=mean)
  
  # actually, we're going to go with the median, which is less sensitive to outliers:
  #RTs <- aggregate(trialResp.rt ~ arraysize + targetpresent, data=df, FUN=median)
  
  RToutput <- as.vector(unlist(RTs$RT))
  names(RToutput) <- sprintf('RT_%d_%s',RTs$arraysize,RTs$targetpresence)
  
  # fit 2 linear models:
  for (target in c('absent','present')) {
    
    # tpval <- c('absent'=0, 'present'=1)[target]
    # model <- lm(RT ~ arraysize, data=RTs[which(RTs$targetpresence==tpval),])
    
    model <- lm(RT ~ arraysize, data=RTs[which(RTs$targetpresence==target),])
    lmoutput <- as.vector(unlist(coef(model)))
    if (lmoutput[1] > 40) {use <- FALSE} # intercept above the maximum RT
    names(lmoutput) <- sprintf('lm_%s_%s',c('intercept','slope'),target)
    output <- c(output, lmoutput)
    
  }
  
  # create named output vector
  output <- as.list(c(output, correctOutput, RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  
  # return to caller:
  return(output)
  
}