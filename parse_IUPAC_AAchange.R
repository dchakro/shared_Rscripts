# Installing missing dependencies
dependencies <- c("stringi")
missing_packages <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if(length(missing_packages)) install.packages(missing_packages) 
rm(missing_packages,dependencies)

parse_IUPAC_AAchange <- function(MutationColumn){
  # #<---------------------------->
  # # You must include this section when:
  # # Distributing, Using and/or Modifying this code. 
  # # Please read and abide by the terms of the included LICENSE.
  # # Copyright 2020, Deepankar Chakroborty, All rights reserved.
  # #
  # #  Author : Deepankar Chakroborty (https://github.com/dchakro)
  # #  Website: https://www.dchakro.com
  # #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
  # #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE
  # #<---------------------------->
  
  
  # #  PURPOSE:
  # #  For a given vector of amino acid changes like A123T, V256F, E746_A750del
  # #  this function returns a data frame with REF, Pos and ALT amino acids. 

  # #  USAGE:
  # #  captureDF <- parse_IUPAC_AAchange(MutationColumn)
  
  if(any(grep(pattern = "^p.",x = MutationColumn))){
    MutationColumn <- gsub("p.", "", MutationColumn, fixed = T)
  }
  
  MutationColumn <- gsub("*", "X", MutationColumn, fixed = T)
  AAPos <- stringi::stri_extract_first_regex(str = MutationColumn,pattern = "[0-9]+")
  REF_AA <- stringi::stri_extract_first_regex(str = MutationColumn,pattern = "[ACDEFGHIKLMNPQRSTVWYX]+")
  ALT_AA <- stringi::stri_extract_last_regex(str = MutationColumn,pattern = "[ACDEFGHIKLMNPQRSTVWYX]+")
  return(data.frame(REF_AA,as.numeric(AAPos),ALT_AA))
  
}