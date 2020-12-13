roundUp <- function(x,to=10)
{
# #<---------------------------->
  # # You must include this section when:
  # # Distributing, Using and/or Modifying this code. 
  # # Please read and abide by the terms of the included LICENSE.
  # # Copyright 2018, Deepankar Chakroborty, All rights reserved.

  # #  Author : Deepankar Chakroborty (https://github.com/dchakro)
  # #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
  # #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE

  # # Adapted from: https://stackoverflow.com/a/44691056
  
  # # PURPOSE:
  # # This function takes in single number as input and rounds it 
  # # to the nearest tenth by default. e.g. 7 would be rounded to 10.
  
  # # PARAMETERS
  # #   x : The integer to be rounded
  # #   to: nearest place to round to
  
  # # EXAMPLES
  # roundUp(465,20) would give 480
  # roundUp(462,5) would give 465

  to*(x%/%to + as.logical(x%%to))
}
