# Installing missing dependencies
dependencies <- c("stringi")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages) 
rm(missing_packages,dependencies)

MutSiteFind <- function(MutationColumn){
# #<---------------------------->
# # You must include this section when:
# # Distributing, Using and/or Modifying this code. 
# # Please read and abide by the terms of the included LICENSE.
# # Copyright 2020, Deepankar Chakroborty, All rights reserved.
# #
# #  Author : Deepankar Chakroborty (https://gitlab.utu.fi/deecha)
# #  Report issues: https://gitlab.utu.fi/deecha/shared_scripts/-/issues
# #  License: https://gitlab.utu.fi/deecha/shared_scripts/-/blob/master/LICENSE
# #<---------------------------->


# #  PURPOSE:
# #  For a given vector of amino acid changes like A123T, V256F, E746_A750del
# #  this function returns c(123, 256, 746) as amino acid positions of
# #  the mutated residue. 
# #     In case of indels, it doesn't return the range!! 
# #        (i.e. returns only the start position)


  return(unlist(x = stringi::stri_extract_first_regex(str = MutationColumn,pattern = "[[:digit:]]+"), use.names = F))
}


