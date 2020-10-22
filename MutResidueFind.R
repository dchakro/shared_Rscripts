# Installing missing dependencies
dependencies <- c("stringi")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages) 
rm(missing_packages,dependencies)

MutResidueFind <- function(MutationColumn){
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
# #  For a given vector of amino acid changes like A123T, V256F, E746_A750del
# #  this function returns c(A123, V256, E746_A750) as amino acid residues for
# #  the inputed mutations. 
# #     In case of indels, it returns the range (see example above)!! 

# #  USAGE:
# #  MutatedResidue <- MutResidueFind(MutationColumn)


  return(stringi::stri_extract_first(str = MutationColumn, regex = "[ACDEFGHIKLMNPQRSTVWYX]?[0-9]+(_[ACDEFGHIKLMNPQRSTVWYX]?[0-9]+)?"))
}



