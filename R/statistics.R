

# Hautus correction:
# add 0.5 to both the number of hits and the number of false alarms, 
# and add 1 to both the number of signal trials and the number of noise trials;
# dubbed the loglinear approach (Hautus, 1995)

dprime <- function(hits, misses, fas, crs, hautus=FALSE) {
  
  if (hautus) {
    hit_rate    <- (hits + 0.5) / (hits + misses + 1)
    fa_rate     <- (fas  + 0.5) / (fas  + crs    + 1)
    specificity <- (crs  + 0.5) / (crs  + misses + 1)
  } else {
    hit_rate <- hits / (hits + misses)
    fa_rate  <- fas  / (fas  + crs)
    specificity <- (crs) / (crs  + misses)
  }
  
  Zhr    <- qnorm(hit_rate)
  Zfr    <- qnorm(fa_rate)
  dprime <- Zhr - Zfr
  beta   <- exp(-Zhr * Zhr/2 + Zfr * Zfr/2)
  c      <- -(Zhr + Zfr)/2
  
  return( list( 'dprime'      = dprime,
                'beta'        = beta,
                'c'           = c,         
                'sensitivity' = hit_rate,
                'specificity' = specificity ) )
  
}

cowan.k <- function(hits, misses, fas, crs, N) {
  
  hit_rate <- hits / (hits + misses)
  cr_rate  <- crs  / (crs  + fas)
  
  K <- (hit_rate + cr_rate - 1) * N
  
  return(K)
  
}

#This function combines RTs and PC into a combined measure termed 'Balanced
#Integration Score' (Liesefeld, Fu, & Zimmer, 2015, JEP:LMC, 41, 1140-1151;
#                    Liesefeld & Janczyk, in press)
#the input dataframe must contain variables 'mean_rt_c' and 'pc'
#the function calculates BIS with values standardized across all fields of the 
#input and adds the field 'bis', which has the same format as 'rt' and 'pc'

# from this GitHub repo: https://github.com/Liesefeld/BIS

BIS <- function(data) {
  n <- length(data$group)    # sample size to correct var()-function result (which uses n-1)
  srt <- sqrt( ((n-1)/n) * var(data$mean_rt_c) )     # sample standard deviation across all rts
  spc <- sqrt( ((n-1)/n) * var(data$pc) )            # sample standard deviation across all rts
  mrt <- mean(data$mean_rt_c)                        # mean across all rts
  mpc <- mean(data$pc)                               # mean across all pcs
  zrt <- (data$mean_rt_c-mrt)/srt                    # standardized rts
  zpc <- (data$pc-mpc)/spc                           # z-standardized pcs
  data$bis <- zpc - zrt                              # Balanced Integration Score
  
  return(data)                                       # return data.frame with added variable 'bis'
}

