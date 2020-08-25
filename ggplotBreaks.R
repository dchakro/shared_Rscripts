ggplotBreaks <- function(range,tick,skip.steps=0){
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code. 
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2020, Deepankar Chakroborty, All rights reserved.
# #
# #  Author : Deepankar Chakroborty (https://github.com/dchakro)
# #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
# #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE
# #<---------------------------->


# #  PURPOSE:
# #  Returns a list of vectors containing breaks and labels 
# # for a continuous variable mapped to one of the axes for use with ggplot2
# #  User enters: 
# #     - the range of the data 
# #     - the tick amount
# #     - skip.steps (if any) = number of labels to skip 
# #                             (i.e. show tick but no label)
# #     e.g.
# #         ggplotBreaks(c(0,150), tick =10, skip.steps = 1) # gives
# #  $breaks
# #     0  10  20  30  40  50  60  70  80  90 100 110 120 130 140 150
# # 
# #  $labels
# #    "0"  " "  "20"  " "  "40"  " "  "60"  " "  "80"  " "  "100"  " "  "120"  " "  "140"  " "

  if (length(range) != 2){
    stop("Correct format for: range = c(min_value,max_value)")
  }
  if(skip.steps<0){
    stop(" 'skip.steps' should be >= 0")
  }
  breaks <- seq(from = range[1],
                to = range[2],
                by = tick)
  if(skip.steps==0){
    labels <- as.character(breaks)  
  } else {
    tmp <- c()
    labels <- unlist(
      lapply(
        breaks[seq(from = 1,
                   to = length(breaks),
                   by = skip.steps+1)],
        function(x) c(tmp,
                      c(x,
                        rep(" ",skip.steps)))),
                     use.names = F)
    rm(tmp)
    
  }
  if(length(labels) < length(breaks)){
    ## Add more spaces at the end to make lengths match
    labels <- c( labels, rep(" " , (length(breaks) - length(labels))))
  }
  if ( length(breaks) < length(labels)){
    ## Remove spaces from the end to make the lengths match
    labels <- labels[1:length(breaks)]
  }
  return(list( breaks = breaks,
              labels = labels))
}
