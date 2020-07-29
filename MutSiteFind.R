# #<---------------------------->
# # Please include this section when distributing and/or using this code. 
# # Please read and abide by the terms of the included LICENSE
# #
# #  Author : Deepankar Chakroborty (https://gitlab.utu.fi/deecha)
# #  Report issues: https://gitlab.utu.fi/deecha/shared_scripts/-/issues
# #  License: https://gitlab.utu.fi/deecha/shared_scripts/-/blob/master/LICENSE
# #
# #  PURPOSE:
# #  For a given vector of amino acid changes like A123T, V256F, E746_A750del
# #  this function returns c(123, 256, 746) as amino acid positions of
# #  the mutated residue. 
# #     In case of indels, it doesn't return the range!! (returns only the start position)
# #
# #<---------------------------->

dependencies <- c("stringi")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages) 
rm(missing_packages,dependencies)

MutSiteFind <- function(MutationColumn){
  return(unlist(x = stringi::stri_extract_first_regex(str = MutationColumn,pattern = "[[:digit:]]+"), use.names = F))
}


