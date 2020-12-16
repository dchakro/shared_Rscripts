# Installing missing dependencies
dependencies <- c("plyr")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages)
rm(missing_packages,dependencies)

summarySE <- function(data=NULL, measurevar=NULL, statistic="mean", groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code. 
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2018, Deepankar Chakroborty, All rights reserved.

# #  Author : Deepankar Chakroborty (https://github.com/dchakro)
# #  Website: https://www.dchakro.com
# #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
# #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE

# # Adapted from: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

# # PURPOSE:
# # Summarizes data by returning count, mean, standard deviation, 
# # standard error of the mean, and confidence interval (default 95%)
# # for a given data frame based on grouping variables
# #   data: a data frame.

# # PARAMETERS
# #   measurevar: the name of a column that contains the variable to be summariezed
# #   groupvars: a vector containing names of columns that contain grouping variables
# #   na.rm: a boolean that indicates whether to ignore NA's
# #   conf.interval: the percent range of the confidence interval (default is 95%)
# #<---------------------------->
  
  # a version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  if(statistic=="mean"){  
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    # Rename the "mean" column    
    datac <- plyr::rename(datac, c("mean" = measurevar))
  }
  if(statistic=="median"){
    datac <- plyr::ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       median = median   (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    # Rename the "median" column    
    datac <- plyr::rename(datac, c("median" = measurevar))
  }
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
